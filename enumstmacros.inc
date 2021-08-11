#ifndef _ENUMSTMACROS_INCLUDED
#define _ENUMSTMACROS_INCLUDED
;------------------------------------------------------------------------------
; Enumerating storage macros
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


; These macros allow enumerated indexes into a 1-dimensional table to be
; defined with the table to ensure consistency is maintained when reordering.


;------------------------------------------------------------------------------


resetenum macro
ENUMIX = 0
  endm

enumdat macro enumname, storedirective, value
enumname equ ENUMIX
        storedirective value
ENUMIX = ENUMIX + 1
  endm


;------------------------------------------------------------------------------

#endif ; _ENUMSTMACROS_INCLUDED
