/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */

/* This test should succeed on both 32- and 64-bit configurations.  */
#include <altivec.h>

__vector unsigned char
doAbsoluteDifferenceUnsignedCharMacro (__vector unsigned char *p,
				       __vector unsigned char *q)
{
  __vector unsigned char result, source_1, source_2;

  source_1 = *p;
  source_2 = *q;

  result = vec_absd (source_1, source_2);
  return result;
}

/* { dg-final { scan-assembler "vabsdub" } } */
