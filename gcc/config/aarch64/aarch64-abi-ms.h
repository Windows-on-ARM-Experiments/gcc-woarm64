/* Machine description for AArch64 MS ABI.
   Copyright (C) 2024 Free Software Foundation, Inc.

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

#ifndef GCC_AARCH64_ABI_MS_H
#define GCC_AARCH64_ABI_MS_H

/* X18 reserved for the TEB on Windows.  */

#undef FIXED_REGISTERS
#define FIXED_REGISTERS					\
  {							\
    0, 0, 0, 0,   0, 0, 0, 0,	/* R0 - R7.  */		\
    0, 0, 0, 0,   0, 0, 0, 0,	/* R8 - R15.  */	\
    0, 0, 1, 0,   0, 0, 0, 0,	/* R16 - R23.  */	\
    0, 0, 0, 0,   0, 1, 0, 1,	/* R24 - R30, SP.  */	\
    0, 0, 0, 0,   0, 0, 0, 0,	/* V0 - V7.  */		\
    0, 0, 0, 0,   0, 0, 0, 0,   /* V8 - V15.  */	\
    0, 0, 0, 0,   0, 0, 0, 0,   /* V16 - V23.  */	\
    0, 0, 0, 0,   0, 0, 0, 0,   /* V24 - V31.  */	\
    1, 1, 1, 1,			/* SFP, AP, CC, VG.  */	\
    0, 0, 0, 0,   0, 0, 0, 0,	/* P0 - P7.  */		\
    0, 0, 0, 0,   0, 0, 0, 0,   /* P8 - P15.  */	\
    1, 1,			/* FFR and FFRT.  */	\
    1, 1, 1, 1, 1, 1, 1, 1	/* Fake registers.  */	\
  }

#undef CALL_REALLY_USED_REGISTERS
#define CALL_REALLY_USED_REGISTERS			\
  {							\
    1, 1, 1, 1,   1, 1, 1, 1,	/* R0 - R7.  */		\
    1, 1, 1, 1,   1, 1, 1, 1,	/* R8 - R15.  */	\
    1, 1, 0, 0,   0, 0, 0, 0,   /* R16 - R23.  */	\
    0, 0, 0, 0,   0, 1, 1, 1,	/* R24 - R30, SP.  */	\
    1, 1, 1, 1,   1, 1, 1, 1,	/* V0 - V7.  */		\
    0, 0, 0, 0,   0, 0, 0, 0,	/* V8 - V15.  */	\
    1, 1, 1, 1,   1, 1, 1, 1,   /* V16 - V23.  */	\
    1, 1, 1, 1,   1, 1, 1, 1,   /* V24 - V31.  */	\
    1, 1, 1, 0,			/* SFP, AP, CC, VG.  */	\
    1, 1, 1, 1,   1, 1, 1, 1,	/* P0 - P7.  */		\
    1, 1, 1, 1,   1, 1, 1, 1,	/* P8 - P15.  */	\
    1, 1,			/* FFR and FFRT.  */	\
    0, 0, 0, 0, 0, 0, 0, 0	/* Fake registers.  */	\
  }

#undef  STATIC_CHAIN_REGNUM
#define STATIC_CHAIN_REGNUM R17_REGNUM

#endif /* GCC_AARCH64_ABI_MS_H.  */
