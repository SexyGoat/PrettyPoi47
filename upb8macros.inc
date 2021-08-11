#ifndef _UPB8MACROS_INCLUDED
#define _UFB8MACROS_INCLUDED
;------------------------------------------------------------------------------
; Unified Port Bit macros for 8-bit Pic MCUs
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
;
; A Unified Port Bit (index) or UPB is a numerical code for a particular bit
; in a particular IO port. UPBs start at 0 for bit 0 of port A and end at 31
; for bit 7 of port D, if such a port existed. UPBs are not directly related
; to the pin numbers on the chip.
;
; Macros are provided to extract the PORTx, TRISx, LATx, ANSELx and WPUx
; addresses and the bit numbers for any single UPB. Though these macros
; are convenient for dealing with single IO pins at a time, the Unified
; Port Mask macros are far more efficient for stuffing multiple bits at
; once into each Special Function Register for the IO ports.
;
; A Unified Port Mask can be assembled in the usual way by left-shifting bit
; 1s, bit 0s or multi-bit values by UPBs as if all the ports were accessed as
; a single 32-bit wide IO port. When it comes to dealing with the real 8-bit
; port registers, macros are provided to extract from a UPM the 8-bit field
; relevant to the SFR indicated by the macro name.
;
; For example:
;   UPB_ACTIVITY_LED = UPB('C', 3)
; will set UPB_ACTIVITY_LED to 8*2 + 3 = 19
;
; Note that _single_ quotes are required for the C preprocessor to correctly
; recognise the port letter as a character and not a string, say.
;
; SFR addresses (as File Register or memory addresses) are given by
;
;   PORT_REG_FR(u), TRIS_REG_FR(u), LAT_REG_FR(u),
;   ANSEL_REG_FR(u), WPU_REG_FR(u)
;
; and
;
;   PORT_REG_MEM(u), TRIS_REG_MEM(u), LAT_REG_MEM(u),
;   ANSEL_REG_MEM(u), WPU_REG_MEM(u)
;
; and the SFR register bit is given by
;
;   PBIT(u)
;
; (which merely extracts the lower three bits of the given UPB).
;
; For the BSF, BCF, BTFSS and BTFSC instructions, which take an SFR address
; and a bit number, macros are provided to supply both arguments at once
; from one UPB number:
;
;   PORT_REGBIT_FR(upb), TRIS_REGBIT_FR(upb), LAT_REGBIT_FR(upb),
;   ANSEL_REGBIT_F(upb), WPU_REGBIT_FR(upb)
;
; A simple example which deals with single IO bits at a time:
;
;   UPB_MAGIC_BUTTON = UPB('B', 5)
;   UPB_SAUSAGE_READY = UPB('B', 0)
;   UPB_DOOM_LED = UPB('C', 7)
;           btfsc   PIN_REGBIT_FR(UPB_MAGIC_BUTTON)
;           bcf     PORT_REGBIT_FR(UPB_DOOM_LED)
;           ; = bcf    PORT_REG_FR(UPB_DOOM_LED), PBIT(UPB_DOOM_LED)
;           movf    PORT_REG_FR(UPB_SAUSAGE_READY), W
;           bsf     WREG, PBIT(UPB_SAUSAGE_READY)
;
; A more complex example which deals with whole registers at a time:
;
;   UPB_BUTTON_MAGIC = UPB('B', 5) ; External pull-up provided
;   UPB_BUTTON_PIZZA = UPB('A', 4) ; Needs MCU weak pull-up enabled
;   UPB_BUTTON_BISCUITS = UPB('A', 0) ; Also needs MCU weak pull-ups.
;   UPB_I2C_SCL = UPB('B', 2) ; Latch low, toggle TRIS to send
;   UPB_I2C_SDA = UPB('B', 6) ; Latch low, toggle TRIS to send
;   UPB_DOOMSDAY = UPB('A', 1)
;   UPB_NOT_DISCONBOBULATE = UPB('B', 7)
;
;           Avoid setting reserved bits.
;           UPM_FULL_IO = 0x0000FFFF
;           ; Set data direction.
;           TRIS_UPM = 0
;           TRIS_UPM = TRIS_UPM | (0 << UPB_BUTTON_MAGIC)
;           TRIS_UPM = TRIS_UPM | (0 << UPB_BUTTON_PIZZA)
;           TRIS_UPM = TRIS_UPM | (0 << UPB_BUTTON_BISCUITS)
;           TRIS_UPM = TRIS_UPM | (0 << UPB_I2C_SCL)
;           TRIS_UPM = TRIS_UPM | (0 << UPB_I2C_SCL)
;           TRIS_UPM = TRIS_UPM | (1 << UPB_DOOMSDAY)
;           TRIS_UPM = TRIS_UPM | (1 << UPB_NOT_DISCONBOBULATE)
;           ; Set latches (for outputs) and pull-ups (for inputs).
;           LAT_UPM = 0
;           LAT_UPM = LAT_UPM | (0 << UPB_BUTTON_MAGIC)
;           LAT_UPM = LAT_UPM | (1 << UPB_BUTTON_PIZZA)
;           LAT_UPM = LAT_UPM | (1 << UPB_BUTTON_BISCUITS)
;           LAT_UPM = LAT_UPM | (0 << UPB_I2C_SCL)
;           LAT_UPM = LAT_UPM | (0 << UPB_I2C_SCL)
;           LAT_UPM = LAT_UPM | (0 << UPB_DOOMSDAY)
;           LAT_UPM = LAT_UPM | (1 << UPB_NOT_DISCONBOBULATE)
;
;           mmvlw   PAMASK_UPM(TRIS_UPM)
;           movwf   SFR_FR(TRISA)
;           mmvlw   PBMASK_UPM(TRIS_UPM)
;           movwf   SFR_FR(TRISB)
;
;           mmvlw   PAMASK_UPM(LAT_UPM)
;           movwf   SFR_FR(LATA)
;           mmvlw   PBMASK_UPM(LAT_UPM)
;           movwf   SFR_FR(LATB)
;
; The SFR names must already be defined before this file is included so that
; the maps of valid PORTx, TRISx, LATx, ANSELx and WPUx SFR addresses can be
; calculated.
;
;------------------------------------------------------------------------------


        include sfrmask.inc


;------------------------------------------------------------------------------

; INTERNAL STUFF

; Define addresses for ports which may or may not exist.
; These are used to construct port register address maps
; in symbols defined further below. (The maps do not
; consume MCU registers or memory.)

_DUMMY_PORT_ADDR = 0xFFFF

#ifdef PORTA
_MAYBE_PORTA  equ PORTA
_MAYBE_LATA   equ LATA
_MAYBE_TRISA  equ TRISA
_MAYBE_WPUA   equ WPUA
_MAYBE_ANSELA equ ANSELA
#else
_MAYBE_PORTA  equ _DUMMY_PORT_ADDR
_MAYBE_LATA   equ _DUMMY_PORT_ADDR
_MAYBE_TRISA  equ _DUMMY_PORT_ADDR
_MAYBE_WPUA   equ _DUMMY_PORT_ADDR
_MAYBE_ANSELA equ _DUMMY_PORT_ADDR
#endif

#ifdef PORTB
_MAYBE_PORTB  equ PORTB
_MAYBE_LATB   equ LATB
_MAYBE_TRISB  equ TRISB
_MAYBE_WPUB   equ WPUB
_MAYBE_ANSELB equ ANSELB
#else
_MAYBE_PORTB  equ _DUMMY_PORT_ADDR
_MAYBE_LATB   equ _DUMMY_PORT_ADDR
_MAYBE_TRISB  equ _DUMMY_PORT_ADDR
_MAYBE_WPUB   equ _DUMMY_PORT_ADDR
_MAYBE_ANSELB equ _DUMMY_PORT_ADDR
#endif

#ifdef PORTC
_MAYBE_PORTC  equ PORTC
_MAYBE_LATC   equ LATC
_MAYBE_TRISC  equ TRISC
_MAYBE_WPUC   equ WPUC
_MAYBE_ANSELC equ ANSELC
#else
_MAYBE_PORTC  equ _DUMMY_PORT_ADDR
_MAYBE_LATC   equ _DUMMY_PORT_ADDR
_MAYBE_TRISC  equ _DUMMY_PORT_ADDR
_MAYBE_WPUC   equ _DUMMY_PORT_ADDR
_MAYBE_ANSELC equ _DUMMY_PORT_ADDR
#endif

#ifdef PORTD
_MAYBE_PORTD  equ PORTD
_MAYBE_LATD   equ LATD
_MAYBE_TRISD  equ TRISD
_MAYBE_WPUD   equ WPUD
_MAYBE_ANSELD equ ANSELD
#else
_MAYBE_PORTD  equ _DUMMY_PORT_ADDR
_MAYBE_LATD   equ _DUMMY_PORT_ADDR
_MAYBE_TRISD  equ _DUMMY_PORT_ADDR
_MAYBE_WPUD   equ _DUMMY_PORT_ADDR
_MAYBE_ANSELD equ _DUMMY_PORT_ADDR
#endif

; Port address maps as 32-bit words (for internal use)

#define _PX_REG_MAPL(a, b, c, d) (((d)<<24)|((c)<<16)|((b)<<8)|(a))
#define _PX_REG_MAPH(a, b, c, d) (((d)<<24)|((c)<<16)|((b)<<8)|(a))

#define _PORT_REG_MAPL _PX_REG_MAPL(LOW(_MAYBE_PORTA), LOW(_MAYBE_PORTB), LOW(_MAYBE_PORTC), LOW(_MAYBE_PORTD))
#define _PORT_REG_MAPH _PX_REG_MAPH(HIGH(_MAYBE_PORTA), HIGH(_MAYBE_PORTB), HIGH(_MAYBE_PORTC), HIGH(_MAYBE_PORTD))

#define _TRIS_REG_MAPL _PX_REG_MAPL(LOW(_MAYBE_TRISA), LOW(_MAYBE_TRISB), LOW(_MAYBE_TRISC), LOW(_MAYBE_TRISD))
#define _TRIS_REG_MAPH _PX_REG_MAPH(HIGH(_MAYBE_TRISA), HIGH(_MAYBE_TRISB), HIGH(_MAYBE_TRISC), HIGH(_MAYBE_TRISD))

#define _LAT_REG_MAPL _PX_REG_MAPL(LOW(_MAYBE_LATA), LOW(_MAYBE_LATB), LOW(_MAYBE_LATC), LOW(_MAYBE_LATD))
#define _LAT_REG_MAPH _PX_REG_MAPH(HIGH(_MAYBE_LATA), HIGH(_MAYBE_LATB), HIGH(_MAYBE_LATC), HIGH(_MAYBE_LATD))

#define _ANSEL_REG_MAPL _PX_REG_MAPL(LOW(_MAYBE_ANSELA), LOW(_MAYBE_ANSELB), LOW(_MAYBE_ANSELC), LOW(_MAYBE_ANSELD))
#define _ANSEL_REG_MAPH _PX_REG_MAPH(HIGH(_MAYBE_ANSELA), HIGH(_MAYBE_ANSELB), HIGH(_MAYBE_ANSELC), HIGH(_MAYBE_ANSELD))

#define _WPU_REG_MAPL _PX_REG_MAPL(LOW(_MAYBE_WPUA), LOW(_MAYBE_WPUB), LOW(_MAYBE_WPUC), LOW(_MAYBE_WPUD))
#define _WPU_REG_MAPH _PX_REG_MAPH(HIGH(_MAYBE_WPUA), HIGH(_MAYBE_WPUB), HIGH(_MAYBE_WPUC), HIGH(_MAYBE_WPUD))


; Generic port address mapping functions (for internal use)
#define _PX_REG_AMBI(mapl, maph, upb) (((((maph) >> (8 * ((upb) >> 3))) & 255) << 8) | (((mapl) >> (8 * ((upb) >> 3))) & 255))

; Safe register mask for any single Unified Port Bit (for internal use)
; (If the UPB is not for the given port (specified here by the UPB bit
; position of bit 0 of that port), zero will be returned.)
#define _PXMASK(upb0, upb) (((255 << (upb0)) & (1 << (upb))) >> (upb0))

; Safe register mask for any Unified Port Mask (for internal use)
#define _PXMASK_UPM(upb0, upm) (((upm) >> (upb0)) & 255)

; Unprotected UPB definition macros
#define _UPB0_RAW(portletter) (((portletter) - 'A') * 8)
#define _UPB_RAW(portletter, bit) (_UPB0_RAW(portletter) + (bit))

; Port letter validation (returning 0 for False and 1 for True)
#define _PORT_EXISTS(p) ((((p)>='A') & ((p)<='D')) & ((((((_PORT_REG_MAPH)>>_UPB0_RAW(p)) & 255) << 8) | (((_PORT_REG_MAPL)>>_UPB0_RAW(p)) & 255)) != (_DUMMY_PORT_ADDR)))

; For invalid port letters, the guarded UPB macro throws a division by zero
; to draw attention.
#define _UPB_GUARDED(p, bit) (((1 / _PORT_EXISTS(p)) * _UPB_RAW((p), (bit))))


;------------------------------------------------------------------------------


; UNIFIED PORT BIT MACROS

; Unified Port Bit index assignment
#define UPB(portletter, bit) _UPB_GUARDED((portletter), (bit))

; Ambiguous IO/memory addresses
#define PORT_REG_AMBI(upb) (_PX_REG_AMBI((_PORT_REG_MAPL), (_PORT_REG_MAPH), (upb)))
#define TRIS_REG_AMBI(upb) (_PX_REG_AMBI((_TRIS_REG_MAPL), (_TRIS_REG_MAPH), (upb)))
#define LAT_REG_AMBI(upb) (_PX_REG_AMBI((_LAT_REG_MAPL), (_LAT_REG_MAPH), (upb)))
#define ANSEL_REG_AMBI(upb) (_PX_REG_AMBI((_ANSEL_REG_MAPL), (_ANSEL_REG_MAPH), (upb)))
#define WPU_REG_AMBI(upb) (_PX_REG_AMBI((_WPU_REG_MAPL), (_WPU_REG_MAPH), (upb)))

; Banked File Register addresses
#define PORT_REG_FR(upb) (SFR_FR(PORT_REG_AMBI(upb)))
#define TRIS_REG_FR(upb) (SFR_FR(TRIS_REG_AMBI(upb)))
#define LAT_REG_FR(upb) (SFR_FR(LAT_REG_AMBI(upb)))
#define ANSEL_REG_FR(upb) (SFR_FR(ANSEL_REG_AMBI(upb)))
#define WPU_REG_FR(upb) (SFR_FR(WPU_REG_AMBI(upb)))

; Memory addresses
#define PORT_REG_MEM(upb) (SFR_MEM(PORT_REG_AMBI(upb)))
#define TRIS_REG_MEM(upb) (SFR_MEM(TRIS_REG_AMBI(upb)))
#define LAT_REG_MEM(upb) (SFR_MEM(LAT_REG_AMBI(upb)))
#define ANSEL_REG_MEM(upb) (SFR_MEM(ANSEL_REG_AMBI(upb)))
#define WPU_REG_MEM(upb) (SFR_MEM(WPU_REG_AMBI(upb)))

; Register bit index (the lower three bits) of a Unified Port Bit index
#define PBIT(upb) ((upb) & 7)

; FR address and bit pairs for bsf/bcf and btfss/btfsc
#define PORT_REGBIT_FR(upb) PORT_REG_FR(upb), ((upb) & 7)
#define TRIS_REGBIT_FR(upb) TRIS_REG_FR(upb), ((upb) & 7)
#define LAT_REGBIT_FR(upb) LAT_REG_FR(upb), ((upb) & 7)
#define ANSEL_REGBIT_FR(upb) ANSEL_REG_FR(upb), ((upb) & 7)
#define WPU_REGBIT_FR(upb) WPU_REG_FR(upb), ((upb) & 7)

; Safe port register masks for any single Unified Port Bit index
; (If the UPB bit is not for the given port, zero will be returned.)
#define PAMASK(upb) (_PXMASK(UPB('A', 0), (upb)))
#define PBMASK(upb) (_PXMASK(UPB('B', 0), (upb)))
#define PCMASK(upb) (_PXMASK(UPB('C', 0), (upb)))
#define PDMASK(upb) (_PXMASK(UPB('D', 0), (upb)))

; Safe register masks for any Unified Port Mask
; (The resulting 8-bit mask only contains bits from the UPM
; belonging to the specific port.)
#define PAMASK_UPM(upm) _PXMASK_UPM(UPB('A', 0), (upm))
#define PBMASK_UPM(upm) _PXMASK_UPM(UPB('B', 0), (upm))
#define PCMASK_UPM(upm) _PXMASK_UPM(UPB('C', 0), (upm))
#define PDMASK_UPM(upm) _PXMASK_UPM(UPB('D', 0), (upm))

; Return 1 if a port of the given letter exists, otherwise 0.
#define PORT_EXISTS(portletter) (_PORT_EXISTS(portletter))


;------------------------------------------------------------------------------

#endif ; _UFB8MACROS_INCLUDED
