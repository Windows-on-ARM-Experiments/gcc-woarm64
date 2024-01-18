/* Subroutines for insn-output.cc for Windows NT.
   Copyright (C) 2024 Free Software Foundation, Inc.

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

void
aarch64_pe_asm_named_section (const char *name, unsigned int flags,
			      tree decl)
{
  char flagchars[8], *f = flagchars;

#if defined (HAVE_GAS_SECTION_EXCLUDE) && HAVE_GAS_SECTION_EXCLUDE == 1
  if ((flags & SECTION_EXCLUDE) != 0)
    *f++ = 'e';
#endif

  if ((flags & (SECTION_CODE | SECTION_WRITE)) == 0)  /* readonly data */
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
#if !defined (HAVE_GAS_SECTION_EXCLUDE) || HAVE_GAS_SECTION_EXCLUDE == 0
      /* If attribute "e" isn't supported we mark this section as
	 never-load.  */
      if ((flags & SECTION_EXCLUDE) != 0)
	*f++ = 'n';
#endif
    }

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
}

void 
aarch64_pe_override_options (void)
{
  /* gcse causes internal compiler errors for this target */
  global_options.x_flag_gcse = false;

  /* keep prologues simple */
  global_options.x_flag_shrink_wrap = false;

  /* alternative to calling __chkstk */
  global_options.x_flag_stack_check = STATIC_BUILTIN_STACK_CHECK;
}
