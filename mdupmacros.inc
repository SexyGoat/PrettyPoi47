#ifndef _MDUPMACROS_INCLUDED
#define _MDUPMACROS_INCLUDED
;------------------------------------------------------------------------------
; Macros for repeated invocation of other single-argument macros
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


; Because of limitations of gpasm-1.4.0 such as lacking a working REPT macro
; and a very limited macro nesting depth, it appears necessary to define a
; series of duplication macros to invoke a single-argument storage macro for
; each argument, the number of which is a part of the duplicating macro's
; name.


;------------------------------------------------------------------------------


mdup1   macro   m, d1
        m       d1
        endm

mdup2   macro   m, d1, d2
        m       d1
        m       d2
        endm

mdup3   macro   m, d1, d2, d3
        m       d1
        m       d2
        m       d3
        endm

mdup4   macro   m, d1, d2, d3, d4
        m       d1
        m       d2
        m       d3
        m       d4
        endm

mdup5   macro   m, d1, d2, d3, d4, d5
        mdup3   m, d1, d2, d3
        mdup2   m, d4, d5
        endm

mdup6   macro   m, d1, d2, d3, d4, d5, d6
        mdup3   m, d1, d2, d3
        mdup3   m, d4, d5, d6
        endm

mdup7   macro   m, d1, d2, d3, d4, d5, d6, d7
        mdup4   m, d1, d2, d3, d4
        mdup3   m, d5, d6, d7
        endm

mdup8   macro   m, d1,d2,d3,d4, d5,d6,d7,d8
        mdup4   m, d1, d2, d3, d4
        mdup4   m, d5, d6, d7, d8
        endm

mdup9   macro   m, d1,d2,d3,d4, d5,d6,d7,d8, d9
        mdup3   m, d1, d2, d3
        mdup3   m, d4, d5, d6
        mdup3   m, d7, d8, d9
        endm

mdup10  macro   m, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10
        mdup4   m, d1, d2, d3, d4
        mdup4   m, d5, d6, d7, d8
        mdup2   m, d9, d10
        endm

mdup11  macro   m, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11
        mdup4   m, d1, d2, d3, d4
        mdup4   m, d5, d6, d7, d8
        mdup3   m, d9, d10, d11
        endm

mdup12  macro   m, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12
        mdup4   m, d1, d2, d3, d4
        mdup4   m, d5, d6, d7, d8
        mdup4   m, d9, d10, d11, d12
        endm

mdup13  macro   m, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13
        mdup4   m, d1, d2, d3, d4
        mdup3   m, d5, d6, d7
        mdup3   m, d8, d9, d10
        mdup3   m, d11, d12, d13
        endm

mdup14  macro   m, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14
        mdup4   m, d1, d2, d3, d4
        mdup4   m, d5, d6, d7, d8
        mdup4   m, d9, d10, d11, d12
        mdup2   m, d13, d14
        endm

mdup15  macro   m, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14,d15
        mdup4   m, d1, d2, d3, d4
        mdup4   m, d5, d6, d7, d8
        mdup4   m, d9, d10, d11, d12
        mdup3   m, d13, d14, d15
        endm

mdup16  macro   m, d1,d2,d3,d4, d5,d6,d7,d8, d9,d10,d11,d12, d13,d14,d15,d16
        mdup4   m, d1, d2, d3, d4
        mdup4   m, d5, d6, d7, d8
        mdup4   m, d9, d10, d11, d12
        mdup4   m, d13, d14, d15, d16
        endm


;------------------------------------------------------------------------------

#endif ; _MDUPMACROS_INCLUDED
