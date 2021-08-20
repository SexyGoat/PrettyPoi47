#ifndef _BAPM14MACROS_INCLUDED
#define _BAPM14MACROS_INCLUDED
;------------------------------------------------------------------------------
; Macros for storing byte-addressable data in 14-bit Program Memory
; (For use with gpasm, an assembler with a C preprocessor)
;------------------------------------------------------------------------------


; (c) Copyright 2021, Daniel Neville

; This file is part of PrettyPoi47.
;
; PrettyPoi47 is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; PrettyPoi47 is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with PrettyPoi47. If not, see <https://www.gnu.org/licenses/>.


;------------------------------------------------------------------------------
; Introduction
;------------------------------------------------------------------------------


; "Byte-addressable" program memory blocks store seven full bytes in
; four 14-bit Program Memory words. Bytes at even offsets (within the
; 7-byte groups) are accessed by the usual means and bytes at odd offsets
; (within the 7-byte group) are spread in the upper 6 bits of multiple
; PM words. The assignment of bytes is such that a shortened run of
; bytes never consumes more PM words than necessary.
;
;   7-Byte-to-4-Word assignment
;
;                   PM Bit
;              DBCA98 76543210
;   PM Word 0: 111111 00000000  Bytes 0, 2, 4 and 6 are at even offsets
;   PM Word 1: 333311 22222222  within a BAPM group of four PM words.
;   PM Word 2: 553333 44444444
;   PM Word 3: 555555 66666666
;   --------------------------
;   PM Word 4: 888888 77777777  Bytes 7, 9, 11 and 13 are at even
;   PM Word 5: AAAA88 99999999  offsets within their BAPM group.
;   PM Word 6: CCAAAA BBBBBBBB
;   PM Word 7: CCCCCC DDDDDDDD
;
; Though reading the stored bytes in sequence is fairly straightforward,
; random access is complicated by the need for division by 7.


;------------------------------------------------------------------------------


        include mdupmacros.inc


;------------------------------------------------------------------------------
; Internal stuff
;------------------------------------------------------------------------------


_bapmc_w = -999999
_bapmc_i = -899999


;------------------------------------------------------------------------------
; Byte-addressed Program Memory macros
;------------------------------------------------------------------------------


BAPM_MODE = 0
BAPM_BYTE_OFFSET = -1


; Begin a byte-addressable block.

bablock macro
  if BAPM_MODE
    error "Second bablock marker encountered in one byte-addressable array."
  endif
_bapmc_w = 0
_bapmc_i = 0
BAPM_MODE = 1
BAPM_BYTE_OFFSET = 0
  endm


; End a byte-addressable block (flushing the PM word cache).

endbab macro
  if !(BAPM_MODE)
    error "An endbab was encountered without a matching bablock."
  endif
  if (_bapmc_i)
        dw (_bapmc_w)
  endif
_bapmc_w = -999999
_bapmc_i = -499999
BAPM_MODE = 0
BAPM_BYTE_OFFSET = -1
  endm


; Append a byte to a byte-addressable block.

dbab macro x
  if !(BAPM_MODE)
    error "Stray dbab encountered outside of bablock."
  endif
  if ((x) < -128) || ((x) > 255)
    error "Byte value out of range."
  endif
  if (_bapmc_i) & 1
        dw (((x) << (7 + (_bapmc_i))) & 0x3F00) | (_bapmc_w)
_bapmc_w = (((x) << (1 + (_bapmc_i))) & 0x3F00)
  else
    if (_bapmc_i)
_bapmc_w = (_bapmc_w) | (x)
      if (_bapmc_i) == 6
        dw (_bapmc_w)
_bapmc_w = 0
_bapmc_i = -1
      endif
    else
_bapmc_w = (x)
    endif
  endif
_bapmc_i = (_bapmc_i) + 1
BAPM_BYTE_OFFSET = BAPM_BYTE_OFFSET + 1
  endm


; Append a (little-endian) 16-bit word.

dbaw macro x
  if ((x) < -32768) || ((x) > 65535)
    error "Word value out of range."
  endif
  dbab LOW(x)
  dbab HIGH(x)
  endm


; Append a (little-endian) 32-bit longword.

dbal macro x
  dbab (x) & 255
  dbab ((x) >> 8) & 255
  dbab ((x) >> 16) & 255
  dbab ((x) >> 24) & 255
  endm


;------------------------------------------------------------------------------
; Variable between BAPM format or PM word low-bytes-only format
;------------------------------------------------------------------------------


; Append a byte in PM, in BAPM format if BAPM_MODE set.

dbvb macro x
  if (BAPM_MODE)
    dbab (x)
  else
    if ((x) < -128) || ((x) > 255)
      error "Byte value out of range."
    endif
    dw (x)
  endif
  endm


; Append a 16-bit word in PM, in BAPM format if BAPM_MODE set.

dbvw macro x
  if (BAPM_MODE)
    dbaw (x)
  else
    if ((x) < -32768) || ((x) > 65535)
      error "Word value out of range."
    endif
    dw (x) & 255, ((x) >> 8) & 255
  endif
  endm


; Append a 32-bit longword in PM, in BAPM format if BAPM_MODE set.

dbvl macro x
  if (BAPM_MODE)
    dbal (x)
  else
    dw (x) & 255, ((x) >> 8) & 255, ((x) >> 16) & 255, ((x) >> 24) & 255
  endif
  endm


;------------------------------------------------------------------------------
; Multiple bytes, words or longwords on one line
;------------------------------------------------------------------------------


dba1b macro d1
  dbab d1
  endm
dba2b macro d1,d2
  mdup2 dbab, d1,d2
  endm
dba3b macro d1,d2,d3
  mdup3 dbab, d1,d2,d3
  endm
dba4b macro d1,d2,d3,d4
  mdup4 dbab, d1,d2,d3,d4
  endm
dba5b macro d1,d2,d3,d4, d5
  mdup5 dbab, d1,d2,d3,d4, d5
  endm
dba6b macro d1,d2,d3,d4, d5,d6
  mdup6 dbab, d1,d2,d3,d4, d5,d6
  endm
dba7b macro d1,d2,d3,d4, d5,d6,d7
  mdup7 dbab, d1,d2,d3,d4, d5,d6,d7
  endm
dba8b macro d1,d2,d3,d4, d5,d6,d7,d8
  mdup8 dbab, d1,d2,d3,d4, d5,d6,d7,d8
  endm
dba9b macro d1,d2,d3,d4, d5,d6,d7,d8, d9
  mdup9 dbab, d1,d2,d3,d4, d5,d6,d7,d8, d9
  endm
dba10b macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10
  mdup10 dbab, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10
  endm
dba11b macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11
  mdup11 dbab, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11
  endm
dba12b macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12
  mdup12 dbab, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12
  endm
dba13b macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13
  mdup13 dbab, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13
  endm
dba14b macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14
  mdup14 dbab, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14
  endm
dba15b macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14,d15
  mdup15 dbab, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14,d15
  endm
dba16b macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14,d15,d16
  mdup16 dbab, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14,d15,d16
  endm

dba1w macro d1
  dbaw d1
  endm
dba2w macro d1,d2
  mdup2 dbaw, d1,d2
  endm
dba3w macro d1,d2,d3
  mdup3 dbaw, d1,d2,d3
  endm
dba4w macro d1,d2,d3,d4
  mdup4 dbaw, d1,d2,d3,d4
  endm
dba5w macro d1,d2,d3,d4, d5
  mdup5 dbaw, d1,d2,d3,d4, d5
  endm
dba6w macro d1,d2,d3,d4, d5,d6
  mdup6 dbaw, d1,d2,d3,d4, d5,d6
  endm
dba7w macro d1,d2,d3,d4, d5,d6,d7
  mdup7 dbaw, d1,d2,d3,d4, d5,d6,d7
  endm
dba8w macro d1,d2,d3,d4, d5,d6,d7,d8
  mdup8 dbaw, d1,d2,d3,d4, d5,d6,d7,d8
  endm
dba9w macro d1,d2,d3,d4, d5,d6,d7,d8, d9
  mdup9 dbaw, d1,d2,d3,d4, d5,d6,d7,d8, d9
  endm
dba10w macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10
  mdup10 dbaw, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10
  endm
dba11w macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11
  mdup11 dbaw, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11
  endm
dba12w macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12
  mdup12 dbaw, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12
  endm
dba13w macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13
  mdup13 dbaw, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13
  endm
dba14w macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14
  mdup14 dbaw, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14
  endm
dba15w macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14,d15
  mdup15 dbaw, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14,d15
  endm
dba16w macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14,d15,d16
  mdup16 dbaw, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14,d15,d16
  endm

dba1l macro d1
  dbal d1
  endm
dba2l macro d1,d2
  mdup2 dbal, d1,d2
  endm
dba3l macro d1,d2,d3
  mdup3 dbal, d1,d2,d3
  endm
dba4l macro d1,d2,d3,d4
  mdup4 dbal, d1,d2,d3,d4
  endm
dba5l macro d1,d2,d3,d4, d5
  mdup5 dbal, d1,d2,d3,d4, d5
  endm
dba6l macro d1,d2,d3,d4, d5,d6
  mdup6 dbal, d1,d2,d3,d4, d5,d6
  endm
dba7l macro d1,d2,d3,d4, d5,d6,d7
  mdup7 dbal, d1,d2,d3,d4, d5,d6,d7
  endm
dba8l macro d1,d2,d3,d4, d5,d6,d7,d8
  mdup8 dbal, d1,d2,d3,d4, d5,d6,d7,d8
  endm
dba9l macro d1,d2,d3,d4, d5,d6,d7,d8, d9
  mdup9 dbal, d1,d2,d3,d4, d5,d6,d7,d8, d9
  endm
dba10l macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10
  mdup10 dbal, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10
  endm
dba11l macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11
  mdup11 dbal, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11
  endm
dba12l macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12
  mdup12 dbal, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12
  endm
dba13l macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13
  mdup13 dbal, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13
  endm
dba14l macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14
  mdup14 dbal, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14
  endm
dba15l macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14,d15
  mdup15 dbal, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14,d15
  endm
dba16l macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14,d15,d16
  mdup16 dbal, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14,d15,d16
  endm


;------------------------------------------------------------------------------
; Multiple bytes, words or longwords on one line (BAPM_MODE sensitive)
;------------------------------------------------------------------------------


dbv1b macro d1
  dbvb d1
  endm
dbv2b macro d1,d2
  mdup2 dbvb, d1,d2
  endm
dbv3b macro d1,d2,d3
  mdup3 dbvb, d1,d2,d3
  endm
dbv4b macro d1,d2,d3,d4
  mdup4 dbvb, d1,d2,d3,d4
  endm
dbv5b macro d1,d2,d3,d4, d5
  mdup5 dbvb, d1,d2,d3,d4, d5
  endm
dbv6b macro d1,d2,d3,d4, d5,d6
  mdup6 dbvb, d1,d2,d3,d4, d5,d6
  endm
dbv7b macro d1,d2,d3,d4, d5,d6,d7
  mdup7 dbvb, d1,d2,d3,d4, d5,d6,d7
  endm
dbv8b macro d1,d2,d3,d4, d5,d6,d7,d8
  mdup8 dbvb, d1,d2,d3,d4, d5,d6,d7,d8
  endm
dbv9b macro d1,d2,d3,d4, d5,d6,d7,d8, d9
  mdup9 dbvb, d1,d2,d3,d4, d5,d6,d7,d8, d9
  endm
dbv10b macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10
  mdup10 dbvb, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10
  endm
dbv11b macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11
  mdup11 dbvb, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11
  endm
dbv12b macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12
  mdup12 dbvb, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12
  endm
dbv13b macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13
  mdup13 dbvb, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13
  endm
dbv14b macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14
  mdup14 dbvb, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14
  endm
dbv15b macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14,d15
  mdup15 dbvb, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14,d15
  endm
dbv16b macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14,d15,d16
  mdup16 dbvb, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14,d15,d16
  endm

dbv1w macro d1
  dbvw d1
  endm
dbv2w macro d1,d2
  mdup2 dbvw, d1,d2
  endm
dbv3w macro d1,d2,d3
  mdup3 dbvw, d1,d2,d3
  endm
dbv4w macro d1,d2,d3,d4
  mdup4 dbvw, d1,d2,d3,d4
  endm
dbv5w macro d1,d2,d3,d4, d5
  mdup5 dbvw, d1,d2,d3,d4, d5
  endm
dbv6w macro d1,d2,d3,d4, d5,d6
  mdup6 dbvw, d1,d2,d3,d4, d5,d6
  endm
dbv7w macro d1,d2,d3,d4, d5,d6,d7
  mdup7 dbvw, d1,d2,d3,d4, d5,d6,d7
  endm
dbv8w macro d1,d2,d3,d4, d5,d6,d7,d8
  mdup8 dbvw, d1,d2,d3,d4, d5,d6,d7,d8
  endm
dbv9w macro d1,d2,d3,d4, d5,d6,d7,d8, d9
  mdup9 dbvw, d1,d2,d3,d4, d5,d6,d7,d8, d9
  endm
dbv10w macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10
  mdup10 dbvw, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10
  endm
dbv11w macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11
  mdup11 dbvw, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11
  endm
dbv12w macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12
  mdup12 dbvw, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12
  endm
dbv13w macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13
  mdup13 dbvw, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13
  endm
dbv14w macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14
  mdup14 dbvw, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14
  endm
dbv15w macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14,d15
  mdup15 dbvw, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14,d15
  endm
dbv16w macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14,d15,d16
  mdup16 dbvw, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14,d15,d16
  endm

dbv1l macro d1
  dbvl d1
  endm
dbv2l macro d1,d2
  mdup2 dbvl, d1,d2
  endm
dbv3l macro d1,d2,d3
  mdup3 dbvl, d1,d2,d3
  endm
dbv4l macro d1,d2,d3,d4
  mdup4 dbvl, d1,d2,d3,d4
  endm
dbv5l macro d1,d2,d3,d4, d5
  mdup5 dbvl, d1,d2,d3,d4, d5
  endm
dbv6l macro d1,d2,d3,d4, d5,d6
  mdup6 dbvl, d1,d2,d3,d4, d5,d6
  endm
dbv7l macro d1,d2,d3,d4, d5,d6,d7
  mdup7 dbvl, d1,d2,d3,d4, d5,d6,d7
  endm
dbv8l macro d1,d2,d3,d4, d5,d6,d7,d8
  mdup8 dbvl, d1,d2,d3,d4, d5,d6,d7,d8
  endm
dbv9l macro d1,d2,d3,d4, d5,d6,d7,d8, d9
  mdup9 dbvl, d1,d2,d3,d4, d5,d6,d7,d8, d9
  endm
dbv10l macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10
  mdup10 dbvl, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10
  endm
dbv11l macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11
  mdup11 dbvl, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11
  endm
dbv12l macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12
  mdup12 dbvl, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12
  endm
dbv13l macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13
  mdup13 dbvl, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13
  endm
dbv14l macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14
  mdup14 dbvl, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14
  endm
dbv15l macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14,d15
  mdup15 dbvl, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14,d15
  endm
dbv16l macro d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14,d15,d16
  mdup16 dbvl, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14,d15,d16
  endm


;------------------------------------------------------------------------------

#endif ; _BAPM14MACROS_INCLUDED
