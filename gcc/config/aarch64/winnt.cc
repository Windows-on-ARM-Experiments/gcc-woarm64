/* Subroutines for insn-output.cc for Windows NT.
   Copyright (C) 2022 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "function.h"
#include "basic-block.h"
#include "rtl.h"
#include "tree.h"
#include "output.h"
#include "varasm.h"
#include "gsyms.h"
#include "stringpool.h"
#include "attribs.h"
#include "errors.h"
#include "options.h"
#include "memmodel.h"
#include "emit-rtl.h"

#define TARGET_SEH 1

void
aarch64_pe_asm_named_section (const char *name, unsigned int flags,
			      tree decl)
{
  char flagchars[8], *f = flagchars;

#if defined (HAVE_GAS_SECTION_EXCLUDE) && HAVE_GAS_SECTION_EXCLUDE == 1
  if ((flags & SECTION_EXCLUDE) != 0)
    *f++ = 'e';
#endif

  if ((flags & (SECTION_CODE | SECTION_WRITE)) == 0)
    /* readonly data */
    {
      *f++ ='d';  /* This is necessary for older versions of gas.  */
      *f++ ='r';
    }
  else
    {
      if (flags & SECTION_CODE)
	*f++ = 'x';
      if (flags & SECTION_WRITE)
	*f++ = 'w';
//       if (flags & SECTION_PE_SHARED) // FIXME?
//         *f++ = 's';
#if !defined (HAVE_GAS_SECTION_EXCLUDE) || HAVE_GAS_SECTION_EXCLUDE == 0
      /* If attribute "e" isn't supported we mark this section as
	 never-load.  */
      if ((flags & SECTION_EXCLUDE) != 0)
	*f++ = 'n';
#endif
    }

    // FIXME?
//   /* LTO sections need 1-byte alignment to avoid confusing the
//      zlib decompression algorithm with trailing zero pad bytes.  */
//   if (strncmp (name, LTO_SECTION_NAME_PREFIX,
// 			strlen (LTO_SECTION_NAME_PREFIX)) == 0)
//     *f++ = '0';

  *f = '\0';

  fprintf (asm_out_file, "\t.section\t%s,\"%s\"\n", name, flagchars);

  if (flags & SECTION_LINKONCE)
    {
      /* Functions may have been compiled at various levels of
	 optimization so we can't use `same_size' here.
	 Instead, have the linker pick one, without warning.
	 If 'selectany' attribute has been specified,  MS compiler
	 sets 'discard' characteristic, rather than telling linker
	 to warn of size or content mismatch, so do the same.  */
      bool discard = (flags & SECTION_CODE)
		      || (TREE_CODE (decl) != IDENTIFIER_NODE
			  && lookup_attribute ("selectany",
					       DECL_ATTRIBUTES (decl)));
      fprintf (asm_out_file, "\t.linkonce %s\n",
	       (discard  ? "discard" : "same_size"));
    }
}

bool
aarch64_pe_valid_dllimport_attribute_p (const_tree decl)
{
  // FIXME
//   if (TARGET_NOP_FUN_DLLIMPORT && TREE_CODE (decl) == FUNCTION_DECL)
//     return false;
  return true;
}

/* Mark a function appropriately.  This should only be called for
   functions for which we are not emitting COFF debugging information.
   FILE is the assembler output file, NAME is the name of the
   function, and PUB is nonzero if the function is globally
   visible.  */

void
aarch64_pe_declare_function_type (FILE *file, const char *name, int pub)
{
  fprintf (file, "\t.def\t");
  assemble_name (file, name);
  fprintf (file, ";\t.scl\t%d;\t.type\t%d;\t.endef\n",
	   pub ? (int) C_EXT : (int) C_STAT,
	   (int) DT_FCN << N_BTSHFT);
}



/* Keep a list of external functions.  */

struct GTY(()) extern_list
{
  struct extern_list *next;
  tree decl;
  const char *name;
};

static GTY(()) struct extern_list *extern_head;

/* Assemble an external function reference.  We need to keep a list of
   these, so that we can output the function types at the end of the
   assembly.  We can't output the types now, because we might see a
   definition of the function later on and emit debugging information
   for it then.  */

void
aarch64_pe_record_external_function (tree decl, const char *name)
{
  struct extern_list *p;

  p = ggc_alloc<extern_list> ();
  p->next = extern_head;
  p->decl = decl;
  p->name = name;
  extern_head = p;
}

/* Keep a list of exported symbols.  */

struct GTY(()) export_list
{
  struct export_list *next;
  const char *name;
  int is_data;		/* used to type tag exported symbols.  */
};

/* Keep a list of stub symbols.  */

struct GTY(()) stub_list
{
  struct stub_list *next;
  const char *name;
};

static GTY(()) struct export_list *export_head;
static GTY(()) struct stub_list *stub_head;

/* Assemble an export symbol entry.  We need to keep a list of
   these, so that we can output the export list at the end of the
   assembly.  We used to output these export symbols in each function,
   but that causes problems with GNU ld when the sections are
   linkonce.  Beware, DECL may be NULL if compile_file() is emitting
   the LTO marker.  */

void
aarch64_pe_maybe_record_exported_symbol (tree decl, const char *name, int is_data)
{
  rtx symbol;
  struct export_list *p;

  if (!decl)
    return;

  symbol = XEXP (DECL_RTL (decl), 0);
  gcc_assert (GET_CODE (symbol) == SYMBOL_REF);
  if (!SYMBOL_REF_DLLEXPORT_P (symbol))
    return;

  gcc_assert (TREE_PUBLIC (decl));

  p = ggc_alloc<export_list> ();
  p->next = export_head;
  p->name = name;
  p->is_data = is_data;
  export_head = p;
}

/* Handle a "selectany" attribute;
   arguments as in struct attribute_spec.handler.  */
tree
aarch64_handle_selectany_attribute (tree *node, tree name, tree, int,
				    bool *no_add_attrs)
{
  tree decl = *node;
  /* The attribute applies only to objects that are initialized and have
     external linkage.  However, we may not know about initialization
     until the language frontend has processed the decl.   Therefore
     we make sure that variable isn't initialized as common.  */
  if (TREE_CODE (decl) != VAR_DECL || !TREE_PUBLIC (decl))
    error ("%qE attribute applies only to initialized variables"
	   " with external linkage", name);
  else
    {
      make_decl_one_only (decl, DECL_ASSEMBLER_NAME (decl));
      /* A variable with attribute selectany never can be common.  */
      DECL_COMMON (decl) = 0;
    }

  /* We don't need to keep attribute itself.  */
  *no_add_attrs = true;
  return NULL_TREE;
}

/* Select a set of attributes for section NAME based on the properties
   of DECL and whether or not RELOC indicates that DECL's initializer
   might contain runtime relocations.

   We make the section read-only and executable for a function decl,
   read-only for a const data decl, and writable for a non-const data decl.

   If the section has already been defined, to not allow it to have
   different attributes, as (1) this is ambiguous since we're not seeing
   all the declarations up front and (2) some assemblers (e.g. SVR4)
   do not recognize section redefinitions.  */
/* ??? This differs from the "standard" PE implementation in that we
   handle the SHARED variable attribute.  Should this be done for all
   PE targets?  */

#define SECTION_PE_SHARED	SECTION_MACH_DEP

unsigned int
aarch64_pe_section_type_flags (tree decl, const char *, int reloc)
{
  unsigned int flags;

  /* Ignore RELOC, if we are allowed to put relocated
     const data into read-only section.  */
  if (!flag_writable_rel_rdata)
    reloc = 0;

  if (decl && TREE_CODE (decl) == FUNCTION_DECL)
    flags = SECTION_CODE;
  else if (decl && decl_readonly_section (decl, reloc))
    flags = 0;
  else
    {
      flags = SECTION_WRITE;

      if (decl && TREE_CODE (decl) == VAR_DECL
	  && lookup_attribute ("shared", DECL_ATTRIBUTES (decl)))
	flags |= SECTION_PE_SHARED;
    }

  if (decl && DECL_P (decl) && DECL_ONE_ONLY (decl))
    flags |= SECTION_LINKONCE;

  return flags;
}

/* Also strip the fastcall prefix and stdcall suffix.  */

const char *
aarch64_pe_strip_name_encoding_full (const char *str)
{
  const char *p;
  const char *name = default_strip_name_encoding (str);

  /* Strip leading '@' on fastcall symbols.  */
  if (*name == '@')
    name++;

  /* Strip trailing "@n".  */
  p = strchr (name, '@');
  if (p)
    return ggc_alloc_string (name, p - name);

  return name;
}

void
aarch64_pe_unique_section (tree decl, int reloc)
{
  int len;
  const char *name, *prefix;
  char *string;

  /* Ignore RELOC, if we are allowed to put relocated
     const data into read-only section.  */
  if (!flag_writable_rel_rdata)
    reloc = 0;
  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
  name = aarch64_pe_strip_name_encoding_full (name);

  /* The object is put in, for example, section .text$foo.
     The linker will then ultimately place them in .text
     (everything from the $ on is stripped). Don't put
     read-only data in .rdata section to avoid a PE linker
     bug when .rdata$* grouped sections are used in code
     without a .rdata section.  */
  if (TREE_CODE (decl) == FUNCTION_DECL)
    prefix = ".text$";
  else if (decl_readonly_section (decl, reloc))
    prefix = ".rdata$";
  else
    prefix = ".data$";
  len = strlen (name) + strlen (prefix);
  string = XALLOCAVEC (char, len + 1);
  sprintf (string, "%s%s", prefix, name);

  set_decl_section_name (decl, string);
}

/* Return the type that we should use to determine if DECL is
   imported or exported.  */

static tree
associated_type (tree decl)
{
  return (DECL_CONTEXT (decl) && TYPE_P (DECL_CONTEXT (decl))
	  ?  DECL_CONTEXT (decl) : NULL_TREE);
}

/* Return true if DECL should be a dllexport'd object.  */

static bool
aarch64_pe_determine_dllexport_p (tree decl)
{
  if (TREE_CODE (decl) != VAR_DECL && TREE_CODE (decl) != FUNCTION_DECL)
    return false;

  /* Don't export local clones of dllexports.  */
  if (!TREE_PUBLIC (decl))
    return false;

  if (TREE_CODE (decl) == FUNCTION_DECL
      && DECL_DECLARED_INLINE_P (decl)
      && !flag_keep_inline_dllexport)
    return false;

  if (lookup_attribute ("dllexport", DECL_ATTRIBUTES (decl)))
    return true;

  return false;
}

/* Return true if DECL should be a dllimport'd object.  */

static bool
aarch64_pe_determine_dllimport_p (tree decl)
{
  tree assoc;

  if (TREE_CODE (decl) != VAR_DECL && TREE_CODE (decl) != FUNCTION_DECL)
    return false;

  if (DECL_DLLIMPORT_P (decl))
    return true;

  /* The DECL_DLLIMPORT_P flag was set for decls in the class definition
     by  targetm.cxx.adjust_class_at_definition.  Check again to emit
     error message if the class attribute has been overridden by an
     out-of-class definition of static data.  */
  assoc = associated_type (decl);
  if (assoc && lookup_attribute ("dllimport", TYPE_ATTRIBUTES (assoc))
      && TREE_CODE (decl) == VAR_DECL
      && TREE_STATIC (decl) && TREE_PUBLIC (decl)
      && !DECL_EXTERNAL (decl)
      /* vtable's are linkonce constants, so defining a vtable is not
	 an error as long as we don't try to import it too.  */
      && !DECL_VIRTUAL_P (decl))
	error ("definition of static data member %q+D of "
	       "dllimport%'d class", decl);

  return false;
}

void
aarch64_pe_encode_section_info (tree decl, rtx rtl, int first)
{
  rtx symbol;
  int flags;

  /* Do this last, due to our frobbing of DECL_DLLIMPORT_P above.  */
  default_encode_section_info (decl, rtl, first);

  /* Careful not to prod global register variables.  */
  if (!MEM_P (rtl))
    return;

  symbol = XEXP (rtl, 0);
  gcc_assert (GET_CODE (symbol) == SYMBOL_REF);

  switch (TREE_CODE (decl))
    {
    case FUNCTION_DECL:
    case VAR_DECL:
      break;

    default:
      return;
    }

  /* Mark the decl so we can tell from the rtl whether the object is
     dllexport'd or dllimport'd.  tree.cc: merge_dllimport_decl_attributes
     handles dllexport/dllimport override semantics.  */
  flags = (SYMBOL_REF_FLAGS (symbol) &
	   ~(SYMBOL_FLAG_DLLIMPORT | SYMBOL_FLAG_DLLEXPORT));
  if (aarch64_pe_determine_dllexport_p (decl))
    flags |= SYMBOL_FLAG_DLLEXPORT;
  else if (aarch64_pe_determine_dllimport_p (decl))
    flags |= SYMBOL_FLAG_DLLIMPORT;

  SYMBOL_REF_FLAGS (symbol) = flags;
}


/* x64 Structured Exception Handling unwind info.  */

struct seh_frame_state
{
  /* In prologue.  */
  bool in_prologue;

  /* In epilogue.  */
  bool in_epilogue;

  /* Is building seh_proc ops.  */
  bool is_seh_proc;
};

/* Set up data structures beginning output for SEH.  */

void
aarch64_pe_seh_init (FILE *f)
{
  struct seh_frame_state *seh;

  if (!TARGET_SEH)
    return;
  if (cfun->is_thunk)
    return;

  /* We cannot support DRAP with SEH.  We turned off support for it by
     re-defining MAX_STACK_ALIGNMENT when SEH is enabled.  */
  gcc_assert (!stack_realign_drap);

  seh = XCNEW (struct seh_frame_state);
  cfun->machine->seh = seh;  
  
  fputs ("\t.seh_proc\t", f);
  assemble_name (f, IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (cfun->decl)));
  fputs (" \n", f);
  seh->is_seh_proc = true;
}

/* Emit assembler directives to reconstruct the SEH state.  */

void
aarch64_pe_seh_cold_init (FILE *f, const char *name)
{
  aarch64_pe_seh_init (f);
}



void 
aarch64_pe_seh_function_prologue (FILE *f)
{
  if (!TARGET_SEH)
    return;
    
  if (cfun->is_thunk)
    return;
  
  if (!cfun->machine->seh->is_seh_proc)
    return;

  cfun->machine->seh->in_prologue = true;
}

/* Emit an assembler directive for the end of the prologue.  */

void
aarch64_pe_seh_end_prologue (FILE *f)
{
  if (!TARGET_SEH)
    return;

  if (cfun->is_thunk)
    return;
  
  if (!cfun->machine->seh->is_seh_proc)
    return;

  if (cfun->machine->seh->in_prologue)
  {
    cfun->machine->seh->in_prologue = false;
    fputs ("\t.seh_endprologue\n", f);
  }
}

/* Emit an assembler directive for the end of the function.  */

static void
aarch64_pe_seh_fini (FILE *f)
{
  struct seh_frame_state *seh;

  if (!TARGET_SEH)
    return;
  if (cfun->is_thunk)
    return;
  seh = cfun->machine->seh;
  if (!seh->is_seh_proc)
    return;
  XDELETE (seh);
  cfun->machine->seh = NULL;
  fputs ("\t.seh_endproc\n", f);
}

/* Emit an assembler directive to save REG at CFA - CFA_OFFSET.  */

static void
seh_emit_save (FILE *f, struct seh_frame_state *seh,
	       rtx reg, HOST_WIDE_INT offset)
{
  const unsigned int regno = REGNO (reg);

  fputs ((FP_REGNUM_P (regno) ? " \t.seh_save_freg\t"
	 : GP_REGNUM_P (regno) ?  " \t.seh_save_reg\t"
	 : (gcc_unreachable (), "")), f);
  aarch64_print_reg (reg, 0, f);
  fprintf (f, ", " HOST_WIDE_INT_PRINT_DEC " \n", abs(offset));
}

/* Emit an assembler directive to adjust RSP by OFFSET.  */

static bool
seh_emit_stackalloc (FILE *f, struct seh_frame_state *seh,
		     HOST_WIDE_INT offset)
{
  bool emitted = false;
  if (offset < 0)
    offset = -offset;
  if (offset < SEH_MAX_FRAME_SIZE)
  {
    fprintf (f, "\t.seh_stackalloc\t" HOST_WIDE_INT_PRINT_DEC "\n", offset);
    emitted = true;
  }

  return emitted;
}

static void
seh_emit_end_epilogue (FILE *file, struct seh_frame_state *seh)
{
  if (seh->is_seh_proc)
  {
    if (seh->in_epilogue)
    {
      fputs ("\t.seh_endepilogue\n", file);
    }
  }
  
  seh->in_epilogue = false;
}


#define CALLEE_SAVED_REG_NUMBER(r) \
  (((r) >= R19_REGNUM && (r) <= R30_REGNUM) || ((r) >= V8_REGNUM && (r) <= V15_REGNUM))

#define MIN(a,b) (((a)<(b))?(a):(b))

static HOST_WIDE_INT
seh_parallel_offset (rtx pat, int wanted_regnum)
{
  rtx dest, src;  
  HOST_WIDE_INT result = 0;

  if (GET_CODE (pat) == PARALLEL)
    { 
      int i, n = XVECLEN (pat, 0);

      for (i = 0; i < n; ++i)
        {
          rtx ele = XVECEXP (pat, 0, i);

          if (GET_CODE (ele) != SET)
            continue;

          dest = SET_DEST (ele);
          src = SET_SRC (ele);

          if (GET_CODE (dest) == REG &&
              REGNO (dest) == wanted_regnum &&
              GET_CODE (src) == MEM &&
              GET_CODE (XEXP (src, 0)) == PLUS &&
              XEXP (XEXP (src, 0), 0) == stack_pointer_rtx)
            {
              result = INTVAL (XEXP (XEXP (src, 0), 1));
            }
          
          if (GET_CODE (src) == REG &&
              REGNO (src) == wanted_regnum &&
              GET_CODE (dest) == MEM &&
              GET_CODE (XEXP (dest, 0)) == PLUS &&
              XEXP (XEXP (dest, 0), 0) == stack_pointer_rtx)
            {
              result = INTVAL (XEXP (XEXP (dest, 0), 1));
            }
        }
    }

  return result;
}

static bool
seh_pattern_emit (FILE *f, struct seh_frame_state *seh, rtx pat)
{
  rtx dest, src;  
  bool emitted = false;

   if (GET_CODE (pat) == PARALLEL)
    { 
      int i, n = XVECLEN (pat, 0);
      int regno, min_regno = 32;
      int reg_count = 0;
      HOST_WIDE_INT increment = 0;

      for (i = 0; i < n; ++i)
        {
          rtx ele = XVECEXP (pat, 0, i);

          if (GET_CODE (ele) != SET)
            continue;

          dest = SET_DEST (ele);
          src = SET_SRC (ele);

          if (GET_CODE (dest) == REG &&
              GET_CODE (src) == PLUS &&
              XEXP (src, 0) == stack_pointer_rtx)
          {
            increment = INTVAL (XEXP (src, 1));
          }

          if (seh->in_prologue && GET_CODE (src) == REG)
          {            
            regno = REGNO (src);

            if (CALLEE_SAVED_REG_NUMBER(regno))
              {
                reg_count += 1;
                min_regno = MIN(regno, min_regno);
              }
          }

          if (seh->in_epilogue && GET_CODE (dest) == REG)
            {            
              regno = REGNO (dest);

              if (CALLEE_SAVED_REG_NUMBER(regno))
              {
                reg_count += 1;
                min_regno = MIN(regno, min_regno);
              }
            }
        }

      if (reg_count == 2)
      {
        fprintf (f, "\t.seh_save_%s	x%d, %d\n", 
          increment != 0 ? "regp_x" : "regp",
          min_regno,
          increment != 0 ? abs(increment) : seh_parallel_offset (pat, min_regno));

        emitted = true;
      }
    }
  else
    {
      src = SET_SRC (pat);

      if (GET_CODE (pat) == SET)
      {
        HOST_WIDE_INT increment = 0;
        dest = SET_DEST (pat);

        switch (GET_CODE (dest))
          {
          case REG:
            switch (GET_CODE (src))
              {
              case REG:  
                if (dest == hard_frame_pointer_rtx &&
                    src == stack_pointer_rtx)
                  {
                    fputs ("\t.seh_set_fp\n", f);
                    emitted = true;
                  }
                else if (CALLEE_SAVED_REG_NUMBER(REGNO (dest)) &&
                    src == stack_pointer_rtx)
                  {
                    seh_emit_save(f, seh, dest, INTVAL (XEXP (src, 1)));
                    emitted = true;
                  }
                break;

              case PLUS: 
                increment = INTVAL (XEXP (src, 1));
                src = XEXP (src, 0);
                if (dest == stack_pointer_rtx)
                  {
                    emitted = seh_emit_stackalloc (f, seh, increment);
                  }
                break;

              case MEM:
                src = XEXP (src, 0);
                if (GET_CODE (src) == PLUS &&
                    GET_CODE (XEXP (src, 0)) == REG &&
                    CALLEE_SAVED_REG_NUMBER(REGNO (dest)) &&
                    XEXP (src, 0) == stack_pointer_rtx)
                  {
                    seh_emit_save (f, seh, dest, INTVAL (XEXP (src, 1)));
                    emitted = true;
                  }
                break;

              default:
                break;
              }
            break;

          case MEM: // Save
            dest = XEXP (dest, 0);
            if (GET_CODE (dest) == PRE_DEC &&
                CALLEE_SAVED_REG_NUMBER(REGNO (src)) &&
                XEXP (dest, 0) == stack_pointer_rtx)
              {
                seh_emit_save (f, seh, src, INTVAL (XEXP (dest, 1)));
                emitted = true;
              }
            else if (GET_CODE (dest) == PLUS &&
                CALLEE_SAVED_REG_NUMBER(REGNO (src)) &&
                XEXP (dest, 0) == stack_pointer_rtx)
              {
                seh_emit_save (f, seh, src, INTVAL (XEXP (dest, 1)));
                emitted = true;
              }
            break;

          default:
            break;
          }
      }
      else if (GET_CODE (pat) == RETURN ||
        GET_CODE (pat) == JUMP_INSN)
      {
        if (seh->in_epilogue)
          {
            seh_emit_end_epilogue (f, seh);
            emitted = true;
          }
      }
    }

    return emitted;
}

/* This function looks at a single insn and emits any SEH directives
   required for unwind of this insn.  */

void
aarch64_pe_seh_unwind_emit (FILE *out_file, rtx_insn *insn)
{
  rtx note, pat;
  struct seh_frame_state *seh;

  if (!TARGET_SEH)
    return;

  if (NOTE_P (insn))
  {
    return;
  }

  seh = cfun->machine->seh;

  if (!seh || (!seh->in_prologue && !seh->in_epilogue))
    return;

  pat = PATTERN (insn);

  if (GET_CODE (pat) == SET)
    {
       rtx dest = SET_DEST (pat);
       if (GET_CODE (dest) == MEM && GET_CODE (XEXP (dest, 0)) == SCRATCH)
         return;
    }

  bool related_exp_needed = true;
  bool emitted = false;
  bool end_epilogue = false;

  for (note = REG_NOTES (insn); note ; note = XEXP (note, 1))
    {
      switch (REG_NOTE_KIND (note))
	{
	case REG_FRAME_RELATED_EXPR:
          if (!emitted)
          {
	    pat = XEXP (note, 0);
            emitted = seh_pattern_emit (out_file, seh, pat);
            if (emitted)
	      related_exp_needed = false;
          }
	  break;

	case REG_CFA_DEF_CFA:
	  break;

	case REG_CFA_EXPRESSION:
	case REG_CFA_REGISTER:
	case REG_CFA_ADJUST_CFA:
	case REG_CFA_OFFSET:
    related_exp_needed = false;
	  break;

  case REG_EH_REGION:
  case REG_CALL_DECL:
    if (seh->in_epilogue)
      {
        end_epilogue = true;
      }    
	  break;

	default:
	  break;
	}
    }

  if (related_exp_needed)
    {
      pat = PATTERN (insn);
      emitted = seh_pattern_emit (out_file, seh, pat);
    }

  if (end_epilogue)
    {
      seh_emit_end_epilogue (out_file, seh);
      emitted = true;
    }
  
  if (!emitted)
    {
      fputs ("\t.seh_nop\n", out_file);
    }
}

void
aarch64_pe_seh_emit_except_personality (rtx personality)
{
  int flags = 0;

  if (!TARGET_SEH)
    return;

  fputs ("\t.seh_handler\t", asm_out_file);
  output_addr_const (asm_out_file, personality);
  fputs (", @unwind, @except\n", asm_out_file);
}

void aarch64_pe_seh_asm_final_postscan_insn (FILE *f, rtx_insn *insn, rtx* pat, int op_count)
{
  struct seh_frame_state *seh;

  if (!TARGET_SEH)
    return;

  seh = cfun->machine->seh;

  if (seh && seh->is_seh_proc)
    {
      if (seh->in_prologue ||
        seh->in_epilogue)
        {      
          rtx pat = PATTERN(insn);

          if (GET_CODE(pat) == SET &&
              GET_CODE(SET_SRC(pat)) == ASM_OPERANDS)
            {
              int i;
              for (i = 0; i < op_count; ++i)
                fputs("\t.seh_nop\n", f);
            }
        }
    }
}

void
aarch64_pe_seh_init_sections (void)
{
  if (TARGET_SEH)
    exception_section = get_unnamed_section (0, output_section_asm_op,
					     "\t.seh_handlerdata");
}

void
aarch64_pe_end_function (FILE *f, const char *, tree)
{
  aarch64_pe_end_epilogue (f);
  aarch64_pe_seh_fini (f);
}

void
aarch64_pe_end_cold_function (FILE *f, const char *, tree)
{
  aarch64_pe_end_epilogue (f);
  aarch64_pe_seh_fini (f);
}

void
aarch64_pe_end_epilogue (FILE *file)
{
  struct seh_frame_state *seh;

  if (!TARGET_SEH)
    return;

  seh = cfun->machine->seh;

  if (seh)
  {
    seh_emit_end_epilogue(file, seh);
  }
}

void
aarch64_pe_begin_epilogue (FILE *file)
{
  struct seh_frame_state *seh;

  if (!TARGET_SEH)
    return;

  seh = cfun->machine->seh;  

  if (seh->is_seh_proc)
    {
      if (seh->in_prologue)
        {      
          fputs ("\t.seh_endprologue\n", file);
        }
      
      fputs ("\t.seh_startepilogue\n", file);
    }

  seh->in_prologue = false;
  seh->in_epilogue = true;
}

void 
aarch64_pe_override_options (void)
{
  global_options.x_flag_shrink_wrap = false;
  global_options.x_flag_stack_check = STATIC_BUILTIN_STACK_CHECK;
}


void
aarch64_pe_record_stub (const char *name)
{
  struct stub_list *p;

  if (!name || *name == 0)
    return;

  p = stub_head;
  while (p != NULL)
    {
      if (p->name[0] == *name
          && !strcmp (p->name, name))
	return;
      p = p->next;
    }

  p = ggc_alloc<stub_list> ();
  p->next = stub_head;
  p->name = name;
  stub_head = p;
}


#ifdef CXX_WRAP_SPEC_LIST

/* Search for a function named TARGET in the list of library wrappers
   we are using, returning a pointer to it if found or NULL if not.
   This function might be called on quite a few symbols, and we only
   have the list of names of wrapped functions available to us as a
   spec string, so first time round we lazily initialise a hash table
   to make things quicker.  */

static const char *
aarch64_find_on_wrapper_list (const char *target)
{
  static char first_time = 1;
  static hash_table<nofree_string_hash> *wrappers;

  if (first_time)
    {
      /* Beware that this is not a complicated parser, it assumes
         that any sequence of non-whitespace beginning with an
	 underscore is one of the wrapped symbols.  For now that's
	 adequate to distinguish symbols from spec substitutions
	 and command-line options.  */
      static char wrapper_list_buffer[] = CXX_WRAP_SPEC_LIST;
      char *bufptr;
      /* Breaks up the char array into separated strings
         strings and enter them into the hash table.  */
      wrappers = new hash_table<nofree_string_hash> (8);
      for (bufptr = wrapper_list_buffer; *bufptr; ++bufptr)
	{
	  char *found = NULL;
	  if (ISSPACE (*bufptr))
	    continue;
	  if (*bufptr == '_')
	    found = bufptr;
	  while (*bufptr && !ISSPACE (*bufptr))
	    ++bufptr;
	  if (*bufptr)
	    *bufptr = 0;
	  if (found)
	    *wrappers->find_slot (found, INSERT) = found;
	}
      first_time = 0;
    }

  return wrappers->find (target);
}

#endif /* CXX_WRAP_SPEC_LIST */

/* This is called at the end of assembly.  For each external function
   which has not been defined, we output a declaration now.  We also
   output the .drectve section.  */

void
aarch64_pe_file_end (void)
{
  struct extern_list *p;

  for (p = extern_head; p != NULL; p = p->next)
    {
      tree decl;

      decl = p->decl;

      /* Positively ensure only one declaration for any given symbol.  */
      if (! TREE_ASM_WRITTEN (decl)
	  && TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl)))
	{
#ifdef CXX_WRAP_SPEC_LIST
	  /* To ensure the DLL that provides the corresponding real
	     functions is still loaded at runtime, we must reference
	     the real function so that an (unused) import is created.  */
	  const char *realsym = aarch64_find_on_wrapper_list (p->name);
	  if (realsym)
	    aarch64_pe_declare_function_type (asm_out_file,
		concat ("__real_", realsym, NULL), TREE_PUBLIC (decl));
#endif /* CXX_WRAP_SPEC_LIST */
	  TREE_ASM_WRITTEN (decl) = 1;
	  aarch64_pe_declare_function_type (asm_out_file, p->name,
					 TREE_PUBLIC (decl));
	}
    }

  if (export_head)
    {
      struct export_list *q;
      drectve_section ();
      for (q = export_head; q != NULL; q = q->next)
	{
	  fprintf (asm_out_file, "\t.ascii \" -export:\\\"%s\\\"%s\"\n",
		   default_strip_name_encoding (q->name),
		   (q->is_data ? ",data" : ""));
	}
    }

  if (stub_head)
    {
      struct stub_list *q;

      for (q = stub_head; q != NULL; q = q->next)
	{
	  const char *name = q->name;
	  const char *oname;

	  if (name[0] == '*')
	    ++name;
	  oname = name;
	  if (name[0] == '.')
	    ++name;
	  if (!startswith (name, "refptr."))
	    continue;
	  name += 7;
	  fprintf (asm_out_file, "\t.section\t.rdata$%s, \"dr\"\n"
			"\t.globl\t%s\n"
			"\t.align 3\n"
			"\t.linkonce\tdiscard\n", oname, oname);
	  fprintf (asm_out_file, "%s:\n\t.quad\t%s\n", oname, name);
	}
    }
}

#include "gt-winnt.h"