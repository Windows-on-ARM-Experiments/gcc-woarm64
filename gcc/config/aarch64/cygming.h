/* Operating system specific defines to be used when targeting GCC for
   hosting on Windows32, using a Unix style C library and tools.
   Copyright (C) 1995-2021 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_AARCH64_CYGMING_H
#define GCC_AARCH64_CYGMING_H

#define DWARF2_DEBUGGING_INFO 1

#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

#undef DEFAULT_ABI
#define DEFAULT_ABI MS_ABI

#undef TARGET_PECOFF
#define TARGET_PECOFF 1

#undef TARGET_SEH
#define TARGET_SEH  1

#include <stdbool.h>
#ifdef __MINGW32__
#include <stdio.h>
#endif

#define TARGET_ASM_NAMED_SECTION  aarch64_pe_asm_named_section

extern void aarch64_pe_file_end (void);
extern void aarch64_pe_record_external_function (tree decl, const char *name);
extern void aarch64_pe_record_stub (const char *name);
extern void aarch64_pe_asm_named_section (const char *, unsigned int, tree);
extern void aarch64_pe_maybe_record_exported_symbol (tree, const char *, int);
extern void aarch64_pe_declare_function_type (FILE *file, const char *name, int pub);

#define TARGET_OS_CPP_BUILTINS()                                 \
  do                                                            \
    {                                                           \
      builtin_define ("__MSVCRT__");                            \
      builtin_define ("__MINGW32__");                           \
      builtin_define ("_WIN32");                                \
      if (TARGET_SEH)						\
        builtin_define ("__SEH__");				\
      builtin_define_std ("WIN32");                             \
      builtin_define_std ("WINNT");                             \
      builtin_define_with_int_value ("_INTEGRAL_MAX_BITS",      \
				     TYPE_PRECISION (intmax_type_node));\
      builtin_define ("__MINGW64__");                       \
      builtin_define_std ("WIN64");                         \
      builtin_define ("_WIN64");                            \
      builtin_define ("__stdcall=__attribute__((__stdcall__))");      \
      builtin_define ("__fastcall=__attribute__((__fastcall__))");    \
      builtin_define ("__thiscall=__attribute__((__thiscall__))");    \
      builtin_define ("__cdecl=__attribute__((__cdecl__))");          \
    }                                                           \
  while (0)

/* Windows64 continues to use a 32-bit long type.  */
#undef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE 32

#undef SIZE_TYPE
#undef PTRDIFF_TYPE
#define SIZE_TYPE "long long unsigned int"
#define PTRDIFF_TYPE "long long int"

#undef WCHAR_TYPE_SIZE
#undef WCHAR_TYPE
#define WCHAR_TYPE_SIZE 16
#define WCHAR_TYPE "short unsigned int"

#define drectve_section() \
  (fprintf (asm_out_file, "\t.section .drectve\n"), \
   in_section = NULL)


/* This implements the `alias' attribute, keeping any stdcall or
   fastcall decoration.  */
#undef	ASM_OUTPUT_DEF_FROM_DECLS
#define	ASM_OUTPUT_DEF_FROM_DECLS(STREAM, DECL, TARGET)			\
  do									\
    {									\
      const char *alias							\
	= IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (DECL));		\
      aarch64_pe_maybe_record_exported_symbol (DECL, alias, 0);		\
      if (TREE_CODE (DECL) == FUNCTION_DECL)				\
	aarch64_pe_declare_function_type (STREAM, alias,			\
				       TREE_PUBLIC (DECL));		\
      ASM_OUTPUT_DEF (STREAM, alias, IDENTIFIER_POINTER (TARGET));	\
    } while (0)

/* Enable alias attribute support.  */
#ifndef SET_ASM_OP
#define SET_ASM_OP "\t.set\t"
#endif

/* GNU as supports weak symbols on PECOFF. */
#ifdef HAVE_GAS_WEAK
#define ASM_WEAKEN_LABEL(FILE, NAME)  \
  do                                  \
    {                                 \
      fputs ("\t.weak\t", (FILE));    \
      assemble_name ((FILE), (NAME)); \
      fputc ('\n', (FILE));           \
    }                                 \
  while (0)

#endif /* HAVE_GAS_WEAK */

/* Get tree.cc to declare a target-specific specialization of
   merge_decl_attributes.  */
#define TARGET_DLLIMPORT_DECL_ATTRIBUTES 1

#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM, BODY, VALUE, REL)		\
  do {									\
    switch (GET_MODE (BODY))						\
      {									\
      case E_QImode:							\
	asm_fprintf (STREAM, "\t.byte\t(%LL%d - %LLrtx%d) / 4\n",	\
		     VALUE, REL);					\
	break;								\
      case E_HImode:							\
	asm_fprintf (STREAM, "\t.2byte\t(%LL%d - %LLrtx%d) / 4\n",	\
		     VALUE, REL);					\
	break;								\
      case E_SImode:							\
      case E_DImode: /* See comment in aarch64_output_casesi.  */		\
	asm_fprintf (STREAM, "\t.word\t(%LL%d - %LLrtx%d) / 4\n",	\
		     VALUE, REL);					\
	break;								\
      default:								\
	gcc_unreachable ();						\
      }									\
  } while (0)

#define READONLY_DATA_SECTION_ASM_OP "\t.section .rdata,\"dr\""

/* Use section relative relocations for debugging offsets.  Unlike
   other targets that fake this by putting the section VMA at 0, PE
   won't allow it.  */
#define ASM_OUTPUT_DWARF_OFFSET(FILE, SIZE, LABEL, OFFSET, SECTION) \
  do {								\
    switch (SIZE)						\
      {								\
      case 4:							\
	fputs ("\t.secrel32\t", FILE);				\
	assemble_name (FILE, LABEL);				\
	if ((OFFSET) != 0)					\
	  fprintf (FILE, "+" HOST_WIDE_INT_PRINT_DEC,		\
		   (HOST_WIDE_INT) (OFFSET));			\
	break;							\
      case 8:							\
	/* This is a hack.  There is no 64-bit section relative	\
	   relocation.  However, the COFF format also does not	\
	   support 64-bit file offsets; 64-bit applications are	\
	   limited to 32-bits of code+data in any one module.	\
	   Fake the 64-bit offset by zero-extending it.  */	\
	fputs ("\t.secrel32\t", FILE);				\
	assemble_name (FILE, LABEL);				\
	if ((OFFSET) != 0)					\
	  fprintf (FILE, "+" HOST_WIDE_INT_PRINT_DEC,		\
		   (HOST_WIDE_INT) (OFFSET));			\
	fputs ("\n\t.long\t0", FILE);				\
	break;							\
      default:							\
	gcc_unreachable ();					\
      }								\
  } while (0)

#define SUBTARGET_ATTRIBUTE_TABLE \
  { "selectany", 0, 0, true, false, false, false, \
    aarch64_handle_selectany_attribute, NULL }, \
  { "__selectany__", 0, 0, true, false, false, false, \
    aarch64_handle_selectany_attribute, NULL }

/* Select attributes for named sections.  */
#define TARGET_SECTION_TYPE_FLAGS  aarch64_pe_section_type_flags

#ifndef HAVE_GAS_ALIGNED_COMM
# define HAVE_GAS_ALIGNED_COMM 0
#endif

#define TARGET_ASM_UNIQUE_SECTION aarch64_pe_unique_section

#define SUPPORTS_ONE_ONLY 1

#undef TARGET_PECOFF
#define TARGET_PECOFF 1

#undef DWARF2_UNWIND_INFO
#define DWARF2_UNWIND_INFO 0

#define TARGET_ENCODE_SECTION_INFO  aarch64_pe_encode_section_info

#undef MAX_OFILE_ALIGNMENT
#define MAX_OFILE_ALIGNMENT (8192 * 8)

/* Output function declarations at the end of the file.  */
#undef TARGET_ASM_FILE_END
#define TARGET_ASM_FILE_END aarch64_pe_file_end

/* Add an external function to the list of functions to be declared at
   the end of the file.  */
#undef ASM_OUTPUT_EXTERNAL
#define ASM_OUTPUT_EXTERNAL(FILE, DECL, NAME)				\
  do									\
    {									\
      if (TREE_CODE (DECL) == FUNCTION_DECL)				\
	aarch64_pe_record_external_function ((DECL), (NAME));		\
      aarch64_asm_output_external (FILE, DECL, NAME);			\
    }									\
  while (0)

/* Declare the type properly for any external libcall.  */
#define ASM_OUTPUT_EXTERNAL_LIBCALL(FILE, FUN) \
  aarch64_pe_declare_function_type (FILE, XSTR (FUN, 0), 1)

extern void aarch64_print_reg (rtx, int, FILE*);
extern void aarch64_pe_end_function (FILE *f, const char *, tree);
extern void aarch64_pe_end_cold_function (FILE *f, const char *, tree);
extern void aarch64_pe_end_epilogue (FILE *file);
extern void aarch64_pe_begin_epilogue (FILE *file);
extern void aarch64_pe_seh_init (FILE *);
extern void aarch64_pe_seh_end_prologue (FILE *);
extern void aarch64_pe_seh_function_prologue (FILE *);
extern void aarch64_pe_seh_cold_init (FILE *, const char *);
extern void aarch64_pe_seh_unwind_emit (FILE *, rtx_insn *);
extern void aarch64_pe_seh_emit_except_personality (rtx);
extern void aarch64_pe_seh_init_sections (void);
extern void aarch64_pe_seh_asm_final_postscan_insn (FILE *stream, rtx_insn *insn, rtx*, int);

#define SEH_MAX_FRAME_SIZE ((2U << 30) - 256)

/* Support hooks for SEH.  */
#undef  TARGET_ASM_UNWIND_EMIT
#define TARGET_ASM_UNWIND_EMIT  aarch64_pe_seh_unwind_emit
#undef  TARGET_ASM_UNWIND_EMIT_BEFORE_INSN
#define TARGET_ASM_UNWIND_EMIT_BEFORE_INSN  false
#undef  TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE aarch64_pe_seh_function_prologue
#undef  TARGET_ASM_FUNCTION_END_PROLOGUE
#define TARGET_ASM_FUNCTION_END_PROLOGUE  aarch64_pe_seh_end_prologue
#undef  TARGET_ASM_EMIT_EXCEPT_PERSONALITY
#define TARGET_ASM_EMIT_EXCEPT_PERSONALITY aarch64_pe_seh_emit_except_personality
#undef  TARGET_ASM_INIT_SECTIONS
#define TARGET_ASM_INIT_SECTIONS  aarch64_pe_seh_init_sections
#undef  SUBTARGET_ASM_UNWIND_INIT
#define SUBTARGET_ASM_UNWIND_INIT  aarch64_pe_seh_init
#undef  TARGET_ASM_FINAL_POSTSCAN_INSN
#define TARGET_ASM_FINAL_POSTSCAN_INSN aarch64_pe_seh_asm_final_postscan_insn

#undef ASM_DECLARE_FUNCTION_SIZE
#define ASM_DECLARE_FUNCTION_SIZE(FILE,NAME,DECL) \
  aarch64_pe_end_function (FILE, NAME, DECL)

#undef ASM_DECLARE_COLD_FUNCTION_SIZE
#define ASM_DECLARE_COLD_FUNCTION_SIZE(FILE,NAME,DECL) \
  aarch64_pe_end_cold_function (FILE, NAME, DECL)

#undef  TARGET_ASM_FUNCTION_BEGIN_EPILOGUE
#define TARGET_ASM_FUNCTION_BEGIN_EPILOGUE aarch64_pe_begin_epilogue

#endif
