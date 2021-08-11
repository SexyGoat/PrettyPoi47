#ifndef _SFRMASKMACROS_INCLUDED
#define _SFRMASKMACROS_INCLUDED
;------------------------------------------------------------------------------
; Special Function Register as File Register (for banked memory access)
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


#define SFR_FR(sfraddr) (0x7F & (sfraddr))

#define SFR_MEM(sfraddr) (sfraddr)



;------------------------------------------------------------------------------

#endif ; _SFRMASKMACROS_INCLUDED
