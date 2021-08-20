#ifndef _PATMACROS_INCLUDED
#define _PATMACROS_INCLUDED
;------------------------------------------------------------------------------
; Macros for storing 3-LED flash patterns in Program Memory
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
        include rgbmacros.inc


;------------------------------------------------------------------------------


; Pattern header (before the Palette, Map and Frames pointers)

pattern macro dfmt, ncols, tmult, pfmt, nframes
  if ((dfmt) >= 0) && ((dfmt) <= 4)
    if (dfmt) < 2
      if ncols
        messg "The given display format should have zero palette colours."
      endif
    else
      if (dfmt) < 6
        if (ncols) > (((0x00030FFF >> (((dfmt) - 2) * 8)) & 255) + 1)
          messg "Too many colours for display format."
        endif
      endif
      if (ncols) < 1
        error "A paletted display format must have at least one colour."
      endif
    endif
  else
    messg "Unknown display format."
  endif
  if (pfmt) & ~(PATSF_BAPM_PMF)
    messg "The pattern storage flags argument contain spurious bits."
  endif
  if ((tmult) < 1) || ((tmult) > 255)
    error "The unit frame period in PWM cycles must be in the range 1..255."
  endif
    dw ((dfmt)<<8)|((ncols)&255), ((pfmt)<<8)|(tmult), (nframes)&255
  endm


; 24-bit direct colour load-or-delta 3-LED animation frame

pf24b macro fperiod, deltamode, rgb1, rgb2, rgb3
  if ((fperiod) < 1) || ((fperiod) > 255)
    error "The frame period in PWM cycles must be in the range 1..255."
  endif
  if ((deltamode) < 0) || ((deltamode) > 1)
    error "Delta mode must be either 0 (load) or 1 (delta)."
  endif
  dbv2b (fperiod) - 1, 128 * (deltamode)
  dbv3rgb (rgb1), (rgb2), (rgb3)
  endm


; 12-bit direct colour load-or-delta 3-LED animation frame

pf12b macro fperiod, deltamode, rgb1, rgb2, rgb3
  if ((fperiod) < 1) || ((fperiod) > 255)
    error "The frame period in PWM cycles must be in the range 1..255."
  endif
  if ((deltamode) < 0) || ((deltamode) > 1)
    error "Delta mode must be either 0 (load) or 1 (delta)."
  endif
  if ((rgb1) < 0) || ((rgb1) > 0xFFF)
    error "RGB12 argument 1 is out of range."
  endif
  if ((rgb2) < 0) || ((rgb2) > 0xFFF)
    error "RGB12 argument 2 is out of range."
  endif
  if ((rgb3) < 0) || ((rgb3) > 0xFFF)
    error "RGB12 argument 3 is out of range."
  endif
  dbv2b (fperiod) - 1, (128 * (deltamode)) | (((rgb1) >> 8) & 0x0F)
  dbv2b ((rgb1) & 0xFF), (((rgb2) >> 4) & 0xFF)
  dbv2b (((rgb2) << 4) & 0xF0) | (((rgb3) >> 8) & 0x0F), ((rgb3) & 0xFF)
  endm


; 256-colour indexed colour 3-LED animation frame

pf256c macro fperiod, colix1, colix2, colix3
  if ((fperiod) < 1) || ((fperiod) > 255)
    error "The frame period in PWM cycles must be in the range 1..255."
  endif
  dbv4b (fperiod) - 1, colix1, colix2, colix3
  endm


; 16-colour indexed colour 3-LED animation frame

pf16c macro fperiod, colix1, colix2, colix3
  if ((fperiod) < 1) || ((fperiod) > 16)
    error "The frame period in PWM cycles must be in the range 1..16."
  endif
  if ((colix1) < 0) || ((colix1) > 16)
    error "Colour index argument 1 is outside the range 0..15."
  endif
  if ((colix2) < 0) || ((colix2) > 16)
    error "Colour index argument 2 is outside the range 0..15."
  endif
  if ((colix3) < 0) || ((colix3) > 16)
    error "Colour index argument 3 is outside the range 0..15."
  endif
  dbvb ((((fperiod) - 1) & 15) << 4) | ((colix1) & 15)
  dbvb (((colix2) & 15) << 4) | ((colix3) & 15)
  endm


; 4-colour indexed colour 3-LED animation frame

pf4c macro fperiod, colix1, colix2, colix3
  if ((fperiod) < 1) || ((fperiod) > 4)
    error "The frame period in PWM cycles must be in the range 1..4."
  endif
  if ((colix1) < 0) || ((colix1) > 3)
    error "colour index argument 1 is outside the range 0..3."
  endif
  if ((colix2) < 0) || ((colix2) > 3)
    error "colour index argument 2 is outside the range 0..3."
  endif
  if ((colix3) < 0) || ((colix3) > 3)
    error "colour index argument 3 is outside the range 0..3."
  endif
_x6 = ((((fperiod) - 1) & 3) << 6)
_x4 = (((colix1) & 3) << 4)
_x2 = (((colix2) & 3) << 2)
_x0 = (((colix3) & 3) << 0)
  dbvb _x6 | _x4 | _x2 | _x0
  endm


;------------------------------------------------------------------------------

#endif ; _PATMACROS_INCLUDED
