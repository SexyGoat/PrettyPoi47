#ifndef _RGBMACROS_INCLUDED
#define _RGBMACROS_INCLUDED
;------------------------------------------------------------------------------
; Macros for coding 24-bit RGB values in Program Memory
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


        include bapmmacros.inc
        include mdupmacros.inc


;------------------------------------------------------------------------------


; 24-bit RGB in conventional low-byte-only format (three program words)

dwrgb macro c
  if (BAPM_MODE)
    error "A dwrgb macro must not occur within a BAPM block."
  endif
  if (c < 0) || (c > 0xFFFFFF)
    messg "RGB value truncated to 24 bits."
  endif
  dw ((c) >> 16) & 255, ((c) >> 8) & 255, (c) & 255
  endm


; 24-bit RGB in BAPM format (to be used within a bablock/endbab pair)

dbargb macro c
  if !(BAPM_MODE)
    error "A dbargb macro may only occur in a BAPM block."
  endif
  if (c < 0) || (c > 0xFFFFFF)
    messg "RGB value truncated to 24 bits."
  endif
  dba3b ((c) >> 16) & 255, ((c) >> 8) & 255, (c) & 255
  endm


; 24-bit RGB in conventional or BAPM format depending on BAPM_MODE

dbvrgb macro c
  if BAPM_MODE
    dbargb c
  else
    dwrgb c
  endif
  endm


;------------------------------------------------------------------------------
; Multiple 24-bit RGB values on one line
;------------------------------------------------------------------------------


dw1rgb macro c1
  dwrgb c1
  endm
dw2rgb macro c1, c2
  mdup2 dwrgb, c1, c2
  endm
dw3rgb macro c1, c2, c3
  mdup3 dwrgb, c1, c2, c3
  endm
dw4rgb macro c1, c2, c3, c4
  mdup4 dwrgb, c1, c2, c3, c4
  endm
dw5rgb macro c1, c2, c3, c4, c5
  mdup5 dwrgb, c1, c2, c3, c4, c5
  endm
dw6rgb macro c1, c2, c3, c4, c5, c6
  mdup6 dwrgb, c1, c2, c3, c4, c5, c6
  endm
dw7rgb macro c1, c2, c3, c4, c5, c6, c7
  mdup7 dwrgb, c1, c2, c3, c4, c5, c6, c7
  endm
dw8rgb macro c1, c2, c3, c4, c5, c6, c7, c8
  mdup8 dwrgb, c1, c2, c3, c4, c5, c6, c7, c8
  endm

dba1rgb macro c1
  dbargb c1
  endm
dba2rgb macro c1, c2
  mdup2 dbargb, c1, c2
  endm
dba3rgb macro c1, c2, c3
  mdup3 dbargb, c1, c2, c3
  endm
dba4rgb macro c1, c2, c3, c4
  mdup4 dbargb, c1, c2, c3, c4
  endm
dba5rgb macro c1, c2, c3, c4, c5
  mdup5 dbargb, c1, c2, c3, c4, c5
  endm
dba6rgb macro c1, c2, c3, c4, c5, c6
  mdup6 dbargb, c1, c2, c3, c4, c5, c6
  endm
dba7rgb macro c1, c2, c3, c4, c5, c6, c7
  mdup7 dbargb, c1, c2, c3, c4, c5, c6, c7
  endm
dba8rgb macro c1, c2, c3, c4, c5, c6, c7, c8
  mdup8 dbargb, c1, c2, c3, c4, c5, c6, c7, c8
  endm

dbv1rgb macro c1
  dbvrgb c1
  endm
dbv2rgb macro c1, c2
  mdup2 dbvrgb, c1, c2
  endm
dbv3rgb macro c1, c2, c3
  mdup3 dbvrgb, c1, c2, c3
  endm
dbv4rgb macro c1, c2, c3, c4
  mdup4 dbvrgb, c1, c2, c3, c4
  endm
dbv5rgb macro c1, c2, c3, c4, c5
  mdup5 dbvrgb, c1, c2, c3, c4, c5
  endm
dbv6rgb macro c1, c2, c3, c4, c5, c6
  mdup6 dbvrgb, c1, c2, c3, c4, c5, c6
  endm
dbv7rgb macro c1, c2, c3, c4, c5, c6, c7
  mdup7 dbvrgb, c1, c2, c3, c4, c5, c6, c7
  endm
dbv8rgb macro c1, c2, c3, c4, c5, c6, c7, c8
  mdup8 dbvrgb, c1, c2, c3, c4, c5, c6, c7, c8
  endm


;------------------------------------------------------------------------------

#endif ; _RGBMACROS_INCLUDED
