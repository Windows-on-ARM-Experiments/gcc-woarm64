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

/* Keep a list of exported symbols.  */

struct GTY(()) export_list
{
  struct export_list *next;
  const char *name;
  int is_data;		/* used to type tag exported symbols.  */
};

static GTY(()) struct export_list *export_head;

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
  /* SEH records offsets relative to the lowest address of the fixed stack
     allocation.  If there is no frame pointer, these offsets are from the
     stack pointer; if there is a frame pointer, these offsets are from the
     value of the stack pointer when the frame pointer was established, i.e.
     the frame pointer minus the offset in the .seh_setframe directive.

     We do not distinguish these two cases, i.e. we consider that the offsets
     are always relative to the "current" stack pointer.  This means that we
     need to perform the fixed stack allocation before establishing the frame
     pointer whenever there are registers to be saved, and this is guaranteed
     by the prologue provided that we force the frame pointer to point at or
     below the lowest used register save area, see ix86_compute_frame_layout.

     This tracks the current stack pointer offset from the CFA.  */
  HOST_WIDE_INT sp_offset;

  /* The CFA is located at CFA_REG + CFA_OFFSET.  */
  HOST_WIDE_INT cfa_offset;
  rtx cfa_reg;

  /* The offset wrt the CFA where register N has been saved.  */
  HOST_WIDE_INT reg_offset[FIRST_PSEUDO_REGISTER];

  /* True if we are past the end of the epilogue.  */
  bool after_prologue;

  /* True if we are in the cold section.  */
  bool in_cold_section;
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

  seh->sp_offset = INCOMING_FRAME_SP_OFFSET;
  seh->cfa_offset = INCOMING_FRAME_SP_OFFSET;
  seh->cfa_reg = stack_pointer_rtx;

  fputs ("\t.seh_proc\t", f);
  assemble_name (f, IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (cfun->decl)));
  fputc ('\n', f);
}

/* Emit an assembler directive for the end of the prologue.  */

void
aarch64_pe_seh_end_prologue (FILE *f)
{
  if (!TARGET_SEH)
    return;
  if (cfun->is_thunk)
    return;
  cfun->machine->seh->after_prologue = true;
  fputs ("\t.seh_endprologue\n", f);
}

/* Emit assembler directives to reconstruct the SEH state.  */

void
aarch64_pe_seh_cold_init (FILE *f, const char *name)
{
  struct seh_frame_state *seh;
  HOST_WIDE_INT alloc_offset, offset;

  if (!TARGET_SEH)
    return;
  if (cfun->is_thunk)
    return;
  seh = cfun->machine->seh;

  fputs ("\t.seh_proc\t", f);
  assemble_name (f, name);
  fputc ('\n', f);

  /* In the normal case, the frame pointer is near the bottom of the frame
     so we can do the full stack allocation and set it afterwards.  There
     is an exception if the function overflows the SEH maximum frame size
     or accesses prior frames so, in this case, we need to pre-allocate a
     small chunk of stack before setting it.  */
  offset = seh->sp_offset - INCOMING_FRAME_SP_OFFSET;
  if (offset < SEH_MAX_FRAME_SIZE && !crtl->accesses_prior_frames)
    alloc_offset = seh->sp_offset;
  else
    alloc_offset = MIN (seh->cfa_offset + 240, seh->sp_offset);

  offset = alloc_offset - INCOMING_FRAME_SP_OFFSET;
  if (offset > 0)
    fprintf (f, "\t.seh_stackalloc\t" HOST_WIDE_INT_PRINT_DEC "\n", offset);

  for (int regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (seh->reg_offset[regno] > 0 && seh->reg_offset[regno] <= alloc_offset)
      {
	if (FP_REGNUM_P (regno))
	  fputs ("//  \t.seh_save_freg\t", f);
	else if (GP_REGNUM_P (regno))
	  fputs ("//  \t.seh_save_reg\t", f);
	else
	  gcc_unreachable ();
	aarch64_print_reg (gen_rtx_REG (DImode, regno), 0, f);
	fprintf (f, ", " HOST_WIDE_INT_PRINT_DEC "\n",
		 seh->reg_offset[regno] - alloc_offset);
      }

  if (seh->cfa_reg != stack_pointer_rtx)
    {
      offset = alloc_offset - seh->cfa_offset;

      gcc_assert ((offset & 15) == 0);
      gcc_assert (IN_RANGE (offset, 0, 240));

      fputs ("\t.seh_setframe\t", f);
      aarch64_print_reg (seh->cfa_reg, 0, f);
      fprintf (f, ", " HOST_WIDE_INT_PRINT_DEC "\n", offset);
    }

  if (alloc_offset != seh->sp_offset)
    {
      offset = seh->sp_offset - alloc_offset;
      if (offset > 0 && offset < SEH_MAX_FRAME_SIZE)
	fprintf (f, "\t.seh_stackalloc\t" HOST_WIDE_INT_PRINT_DEC "\n", offset);

      for (int regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	if (seh->reg_offset[regno] > alloc_offset)
	  {
	    if (FP_REGNUM_P (regno))
	      fputs ("// \t.seh_save_freg\t", f);
	    else if (GP_REGNUM_P (regno))
	      fputs ("// \t.seh_save_reg\t", f);
	    else
	      gcc_unreachable ();
	    aarch64_print_reg (gen_rtx_REG (DImode, regno), 0, f);
	    fprintf (f, ", " HOST_WIDE_INT_PRINT_DEC "\n",
		     seh->reg_offset[regno] - seh->sp_offset);
	  }
    }

  fputs ("\t.seh_endprologue\n", f);
}

/* Emit an assembler directive for the end of the function.  */

static void
aarch64_pe_seh_fini (FILE *f, bool cold)
{
  struct seh_frame_state *seh;

  if (!TARGET_SEH)
    return;
  if (cfun->is_thunk)
    return;
  seh = cfun->machine->seh;
  if (cold != seh->in_cold_section)
    return;
  XDELETE (seh);
  cfun->machine->seh = NULL;
  fputs ("\t.seh_endproc\n", f);
}

/* Emit an assembler directive to save REG via a PUSH.  */

static void
seh_emit_push (FILE *f, struct seh_frame_state *seh, rtx reg)
{
  const unsigned int regno = REGNO (reg);

  gcc_checking_assert (GP_REGNUM_P (regno));

  seh->sp_offset += UNITS_PER_WORD;
  seh->reg_offset[regno] = seh->sp_offset;
  if (seh->cfa_reg == stack_pointer_rtx)
    seh->cfa_offset += UNITS_PER_WORD;

  fputs ("\t.seh_pushreg\t", f);
  aarch64_print_reg (reg, 0, f);
  fputc ('\n', f);
}

/* Emit an assembler directive to save REG at CFA - CFA_OFFSET.  */

static void
seh_emit_save (FILE *f, struct seh_frame_state *seh,
	       rtx reg, HOST_WIDE_INT cfa_offset)
{
  const unsigned int regno = REGNO (reg);
  HOST_WIDE_INT offset;

  seh->reg_offset[regno] = cfa_offset;

  /* Negative save offsets are of course not supported, since that
     would be a store below the stack pointer and thus clobberable.  */
  // FIXME gcc_assert (seh->sp_offset >= cfa_offset);
  offset = cfa_offset - seh->sp_offset;

  fputs ((FP_REGNUM_P (regno) ? "// \t.seh_save_freg\t"
	 : GP_REGNUM_P (regno) ?  "// \t.seh_save_reg\t"
	 : (gcc_unreachable (), "")), f);
  aarch64_print_reg (reg, 0, f);
  fprintf (f, ", " HOST_WIDE_INT_PRINT_DEC "\n", offset);
}

/* Emit an assembler directive to adjust RSP by OFFSET.  */

static void
seh_emit_stackalloc (FILE *f, struct seh_frame_state *seh,
		     HOST_WIDE_INT offset)
{
  /* We're only concerned with prologue stack allocations, which all
     are subtractions from the stack pointer.  */
  // FIXME gcc_assert (offset < 0);
  offset = -offset;

  if (seh->cfa_reg == stack_pointer_rtx)
    seh->cfa_offset += offset;
  seh->sp_offset += offset;

  /* Do not output the stackalloc in that case (it won't work as there is no
     encoding for very large frame size).  */
  if (offset < SEH_MAX_FRAME_SIZE)
    fprintf (f, "\t.seh_stackalloc\t" HOST_WIDE_INT_PRINT_DEC "\n", offset);
}

/* Process REG_CFA_ADJUST_CFA for SEH.  */

static void
seh_cfa_adjust_cfa (FILE *f, struct seh_frame_state *seh, rtx pat)
{
  rtx dest, src;
  HOST_WIDE_INT reg_offset = 0;
  unsigned int dest_regno;

  dest = SET_DEST (pat);
  src = SET_SRC (pat);

  if (GET_CODE (src) == PLUS)
    {
      reg_offset = INTVAL (XEXP (src, 1));
      src = XEXP (src, 0);
    }
  else if (GET_CODE (src) == MINUS)
    {
      reg_offset = -INTVAL (XEXP (src, 1));
      src = XEXP (src, 0);
    }
  gcc_assert (src == stack_pointer_rtx);
  gcc_assert (seh->cfa_reg == stack_pointer_rtx);
  dest_regno = REGNO (dest);

  if (dest_regno == STACK_POINTER_REGNUM)
    seh_emit_stackalloc (f, seh, reg_offset);
  else if (dest_regno == HARD_FRAME_POINTER_REGNUM)
    {
      HOST_WIDE_INT offset;

      seh->cfa_reg = dest;
      seh->cfa_offset -= reg_offset;

      offset = seh->sp_offset - seh->cfa_offset;

      gcc_assert ((offset & 15) == 0);
      gcc_assert (IN_RANGE (offset, 0, 240));

      fputs ("\t.seh_setframe\t", f);
      aarch64_print_reg (seh->cfa_reg, 0, f);
      fprintf (f, ", " HOST_WIDE_INT_PRINT_DEC "\n", offset);
    }
  else
    gcc_unreachable ();
}

/* Process REG_CFA_OFFSET for SEH.  */

static void
seh_cfa_offset (FILE *f, struct seh_frame_state *seh, rtx pat)
{
  rtx dest, src;
  HOST_WIDE_INT reg_offset;

  dest = SET_DEST (pat);
  src = SET_SRC (pat);

  gcc_assert (MEM_P (dest));
  dest = XEXP (dest, 0);
  if (REG_P (dest))
    reg_offset = 0;
  else
    {
      gcc_assert (GET_CODE (dest) == PLUS);
      reg_offset = INTVAL (XEXP (dest, 1));
      dest = XEXP (dest, 0);
    }
  // FIXME gcc_assert (dest == seh->cfa_reg);

  seh_emit_save (f, seh, src, seh->cfa_offset - reg_offset);
}

/* Process a FRAME_RELATED_EXPR for SEH.  */

static void
seh_frame_related_expr (FILE *f, struct seh_frame_state *seh, rtx pat)
{
  rtx dest, src;
  HOST_WIDE_INT addend;

  /* See the full loop in dwarf2out_frame_debug_expr.  */
  if (GET_CODE (pat) == PARALLEL || GET_CODE (pat) == SEQUENCE)
    {
      int i, n = XVECLEN (pat, 0), pass, npass;

      npass = (GET_CODE (pat) == PARALLEL ? 2 : 1);
      for (pass = 0; pass < npass; ++pass)
	for (i = 0; i < n; ++i)
	  {
	    rtx ele = XVECEXP (pat, 0, i);

	    if (GET_CODE (ele) != SET)
	      continue;
	    dest = SET_DEST (ele);

	    /* Process each member of the PARALLEL independently.  The first
	       member is always processed; others only if they are marked.  */
	    if (i == 0 || RTX_FRAME_RELATED_P (ele))
	      {
		/* Evaluate all register saves in the first pass and all
		   register updates in the second pass.  */
		if ((MEM_P (dest) ^ pass) || npass == 1)
		  seh_frame_related_expr (f, seh, ele);
	      }
	  }
      return;
    }

  dest = SET_DEST (pat);
  src = SET_SRC (pat);

  switch (GET_CODE (dest))
    {
    case REG:
      switch (GET_CODE (src))
	{
	case REG:
	  /* REG = REG: This should be establishing a frame pointer.  */
	  gcc_assert (src == stack_pointer_rtx);
	  gcc_assert (dest == hard_frame_pointer_rtx);
	  seh_cfa_adjust_cfa (f, seh, pat);
	  break;

	case PLUS:
	  addend = INTVAL (XEXP (src, 1));
	  src = XEXP (src, 0);
	  if (dest == hard_frame_pointer_rtx)
	    seh_cfa_adjust_cfa (f, seh, pat);
	  else if (dest == stack_pointer_rtx)
	    {
	      gcc_assert (src == stack_pointer_rtx);
	      seh_emit_stackalloc (f, seh, addend);
	    }
	  else
	    gcc_unreachable ();
	  break;

	default:
	  // FIXME gcc_unreachable ();
	  break;
	}
      break;

    case MEM:
      /* A save of some kind.  */
      dest = XEXP (dest, 0);
      if (GET_CODE (dest) == PRE_DEC)
	{
	  gcc_checking_assert (GET_MODE (src) == Pmode);
	  gcc_checking_assert (REG_P (src));
	  seh_emit_push (f, seh, src);
	}
      else
	seh_cfa_offset (f, seh, pat);
      break;

    default:
      gcc_unreachable ();
    }
}

/* This function looks at a single insn and emits any SEH directives
   required for unwind of this insn.  */

void
aarch64_pe_seh_unwind_emit (FILE *out_file, rtx_insn *insn)
{
  rtx note, pat;
  bool handled_one = false;
  struct seh_frame_state *seh;

  if (NOTE_P (insn))
  {
    fprintf (asm_out_file, "// %s\n", GET_NOTE_INSN_NAME (NOTE_KIND (insn)));
  }

  if (!TARGET_SEH)
    return;

  seh = cfun->machine->seh;
  if (NOTE_P (insn) && NOTE_KIND (insn) == NOTE_INSN_SWITCH_TEXT_SECTIONS)
    {
      /* See ix86_output_call_insn/seh_fixup_eh_fallthru for the rationale.  */
      rtx_insn *prev = prev_active_insn (insn);
      if (prev && (CALL_P (prev) || !insn_nothrow_p (prev)))
	fputs ("\tnop\n", out_file);
      fputs ("\t.seh_endproc\n", out_file);
      seh->in_cold_section = true;
      return;
    }

  if (NOTE_P (insn) || !RTX_FRAME_RELATED_P (insn))
    return;

  /* Skip RTX_FRAME_RELATED_P insns that are associated with the epilogue.  */
  if (seh->after_prologue)
    return;

  for (note = REG_NOTES (insn); note ; note = XEXP (note, 1))
    {
      switch (REG_NOTE_KIND (note))
	{
	case REG_FRAME_RELATED_EXPR:
	  pat = XEXP (note, 0);
	  goto found;

	case REG_CFA_DEF_CFA:
	case REG_CFA_EXPRESSION:
	  /* Only emitted with DRAP and aligned memory access using a
	     realigned SP, both of which we disable.  */
	  // FIXME gcc_unreachable ();
	  break;

	case REG_CFA_REGISTER:
	  /* Only emitted in epilogues, which we skip.  */
	  gcc_unreachable ();

	case REG_CFA_ADJUST_CFA:
	  pat = XEXP (note, 0);
	  if (pat == NULL)
	    {
	      pat = PATTERN (insn);
	      if (GET_CODE (pat) == PARALLEL)
		pat = XVECEXP (pat, 0, 0);
	    }
	  seh_cfa_adjust_cfa (out_file, seh, pat);
	  handled_one = true;
	  break;

	case REG_CFA_OFFSET:
	  pat = XEXP (note, 0);
	  if (pat == NULL)
	    pat = single_set (insn);
	  seh_cfa_offset (out_file, seh, pat);
	  handled_one = true;
	  break;

	default:
	  break;
	}
    }
  if (handled_one)
    return;
  pat = PATTERN (insn);
 found:
  seh_frame_related_expr (out_file, seh, pat);
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
  aarch64_pe_seh_fini (f, false);
}

void
aarch64_pe_end_cold_function (FILE *f, const char *, tree)
{
  aarch64_pe_seh_fini (f, true);
}

void
aarch64_pe_start_epilogue (FILE *file ATTRIBUTE_UNUSED)
{
  fputs ("//\t.seh_startepilogue\n", asm_out_file);
}


#include "gt-winnt.h"