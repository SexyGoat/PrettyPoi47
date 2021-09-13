;------------------------------------------------------------------------------
; PrettyPoi47 code for the New Series "Ninja Poi" incorporating the PIC16F1847
; (For use with gpasm, an assembler with a C preprocessor)
;------------------------------------------------------------------------------


; PrettyPoi47 firmware for Ninja LED stick poi
; Version: 0.9.6.1
; (c) Copyright 2021, Daniel Neville


; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program. If not, see <https://www.gnu.org/licenses/>.


;------------------------------------------------------------------------------
; Introduction
;------------------------------------------------------------------------------


; This replacement firmware for Home of Poi's Ninja LED stick poi is based on
; PrettyPoi, which was written from scratch to include several improvements
; over the standard poi firmware.
;
; There are several feature improvements:
;
;   * Many patterns are included, mainly made for human eyes rather than for
;     cameras with long open shutter times.
;   * Favourite patterns can be stored, reordered and deleted one by one.
;     The user may choose whether a full Favourites list means adding a
;     new entry is disallowed or instead that adding a new entry forces
;     the automatic deletion of an existing entry.
;   * A brightness control is provided via a boot menu. (The default is 5/5.)
;   * The updating of the Last Used Pattern memory can be switched on or off
;     via the boot menu.
;   * Via a boot menu option, the Favourites list can be locked and switching
;     between banks of patterns may additionally be disabled.
;   * When the Favourites list is full, adding a new item is inhibited in
;     to protect existing items from being pushed off the end of the list.
;     This protection feature may be disabled by choosing the most permissive
;     setting in the Restrictions boot menu.
;   * A Slideshow (auto-advance) mode, with one of three switching intervals,
;     may be activated for any bank of patterns, including the Favourites
;     list. The Mode Button still works as usual in Slideshow mode, without
;     stopping the slideshow.
;   * Pressing the Pattern Button while in Slideshow mode causes the previous
;     pattern in the cycle to be selected. (The slideshow feature can be used
;     for easily stepping backwards through a long list of patterns.)
;   * A short press of the Mode Button (longer than a tap) jumps to the
;     Favourites list, if any Favourites entries exist, otherwise to the
;     first bank of preset patterns.
;   * A short cue is displayed when a Pattern or Mode selection wraps around.
;     (but only as a result of a button press).
;   * The Pattern Adjustment Mode may be invoked for any preset pattern. The
;     timing may be adjusted for any such pattern. Preset patterns using one
;     or more colours from one, two ot three colour ramps allow the user to
;     select up to three independent colour ramps. (The background colour,
;     most often black, is set by the first ramp selector.) The adjustment
;     is temporary until that pattern is saved to the Favourites list.
;
; There are a few technical improvements:
;
;   * The Power Latch control works correctly. In the Off state, the Mode
;     button being pressed for less than the required turn-on hold time will
;     cause the MCU tp properly turn off the power to itself and the LEDs.
;   * The Last Used Pattern memory is not updated by the Slideshow mode
;     automatically advancing the selected pattern. (Stopping the slideshow
;     or using the Mode Button or the Pattern Buttons will cause an update
;     to be performed, if the LUP feature is enabled.)
;   * EEPROM wear levelling is employed for the LUP feature so that the
;     buttons to advance the pattern selection can be pressed millions of
;     times before the EEPROM wears out. (EEPROM memory cells are each good
;     for about 100,000 erase-program cycles.
;   * The animation player, with a few display formats available, is fairly
;     flexible and is used (and abused) by the menu system to cut down on
;     tedious user interface coding.
;   * The Pulse Width Modulator runs at a slightly increased speed, at 443Hz.
;   * Phase-correct PWM is used so that colour fringing is at least symmetric.
;   * The pins leading to the cathodes of the common anode RGB LEDs are
;     switched between high-impedance (dark) and output low (lit) rather
;     than between output high and output low.
;   * Many data structures are packed in a 7-bytes-to-4-words "BAPM" format
;     to make the most of the 14-bit Program Memory words. (The requirement
;     for random access is why there is a divide-by-seven routine).
;
; PrettyPoi27 was designed with PrettyPoi47 in mind, but kept to the limit of
; 4096 Program Wprds for the PIC16F1827 used in the older Ninja poi. This code,
; on the other hand, can afford to be extravagant with features and patterns
; as the PIC16F1847 has twice the Program Memory.
;
; The RGB LEDs are numbered as they are labelled on the PCB, in 3-2-1 order
; from the lanyard and MCU end (proximal) to the negative battery terminal
; end (distal). The LED numbering on the PCB is the same for the Old Series
; PCB and the New Series PCB. However, the New Series has LED3 on pins 11,
; 12 and 13 and LED1 on pins 7, 2 and 3 whereas the Old Series has the pin
; assignments exchanged between LED3 and LED1. (The channel assignments
; within each RGB LED is the same.),
;
; Curiously, the common anode RGB LEDs used in both the Old Series and New
; Series poi have their leads in G-An-R-B order in contrast to the usual
; R-An-G-B or R-K-G-B order.
;
; Because there is enough room on the PIC16F1847 to use the 12-bit direct
; colour display format, the FetchAnimFrame_Delta_12b function, excluded
; from PrettyPoi27, is included in PrettyPoi47.


;------------------------------------------------------------------------------
; Wiring for the Old Series poi (normally with the PIC16F1827 MCU)
;
; This diagram is only applicable in the case of a PIC16F1847 installed where
; a PIC16F1827 would normally be.
;
;------------------------------------------------------------------------------


;
;                                               Old Series Ninja poi
;                                               Buttons and MCU side
;                                      +------------------------------------ -
;              Microchip               |           D1  D3   (o)    C1   C2
;           PIC16F1827-I/SS            |                       (o)
;                 MCU                  |               (2) (5)      +------+
;                                      |              __________    |      |
;         +----------------+           | B1   B2     [____MCU___](4)|[LED3]|
; LATCH  1|RA2          RA1|20 SENSE   |                            |      |
;    B3  2|RA3          RA0|19 PATTERN |            (1) (o)         +------+
;    R3  3|RA4          RA7|18 MODE    |              (3) (o)   C3
;        4|~MCLR/RA5    RA6|17         |         (o)
;        5|VSS          VDD|16         +------------------------------------ -
;        6|VSS          VDD|15             (Vias are numbered for PicKit2)
;    G3  7|RB0  ICSPDAT/RB7|14
;    B2  8|RB1  ICSPCLK/RB6|13 B1             PICKit2 programmer
;    R2  9|RB2          RB5|12 R1      -----------+  ____
;    G2 10|RB3          RB4|11 G1                >|1 MCLR (12V)
;         +----------------+            PICKit2   |2 VDD = +5V
;                                      programmer |3 VSS = GND
;                                        from     |4 ICSPDAT
;                                      Microchip  |5 ICSPCLK
;                                      (or clone) |6 AUX (Not used)
;                                      -----------+


;------------------------------------------------------------------------------
; Wiring for the New Series poi (with the PIC16F1847 MCU)
;
; This is the usual target for PrettyPoi47.
;
;------------------------------------------------------------------------------


;                                                 Ninja poi NS-2 1.2 PCB
;                                                     Component side
;            PIC16F1847-I/SS                 +----------------------------- -
;                  MCU                       |(o)VSS  (o)VDD=+5V
;          +----------------+                |  =GND          ____ +------+
;  LATCH  1|RA2          RA1|20 SENSE        |       [MCU] (o)MCLR |[LED3]|
;     B1  2|RA3          RA0|19 PATTERN      |                     +------+
;     R1  3|RA4          RA7|18 MODE         |     ICSPDAT(o)  ICSPCLK(o)
;         4|~MCLR/RA5    RA6|17              +----------------------------- -
;         5|VSS          VDD|16
;         6|VSS          VDD|15                     PICKit2 programmer
;     G1  7|RB0  ICSPDAT/RB7|14              -----------+  ____
;     B2  8|RB1  ICSPCLK/RB6|13 B3                     >|1 MCLR (12V)
;     R2  9|RB2          RB5|12 R3            PICKit2   |2 VDD = +5V
;     G2 10|RB3          RB4|11 G3           programmer |3 VSS = GND
;          +----------------+                  from     |4 ICSPDAT
;                                            Microchip  |5 ICSPCLK
;                                                       |6 AUX (Not used)
;                                            -----------+


;------------------------------------------------------------------------------
; Index
;------------------------------------------------------------------------------


; Target
; Includes
; Pin assignments
; Constants
; Derived timing constants (and range checks)
; Structures
; EEPROM addresses
; Variables in SRAM
;
; Page 0 (0x0000..0x07FF)
;
;   ResetVector
;   InterruptServiceRoutine (dummy)
;
;   Set-up and shutdown:
;     ConfigureHardware
;     (DelayBy325)
;     (DelayBy5NPlus5)
;     ShutDown
;     ShutDownImmediately
;     MashTheSnoozeButton
;   Mathematical functions:
;     Mult_u8
;     Div7_u16
;   Memory functions:
;     ReadPMHighBits
;     FetchPMWordFromTable
;     ReadBAPMByte
;     AddBAPMOffset
;   EEPROM functions:
;     ReadEEPROMByte
;     WriteEEPROMByte
;     WriteEEPROMByte_Smart
;     (ReadEEPROMBlock)
;     (WriteEEPROMBlock)
;   EEPROM wear-levelling functions:
;     FindCurrentEWLIndex
;     GetEWLDataAddress
;     AdvanceEWLStatusRing
;   Pulse Width Modulation (PWM):
;     (PerformFastPWMCycle) (slow)
;   Phase-correct Pulse Width Modulation (PWM):
;     PerformPCPWMCycle (faster and phase-correct)
;   Intensity control:
;     LoadLEDOCRs_FullIntensity
;     LoadLEDOCRs_HalfIntensity
;     LoadLEDOCRs_SixteenthIntensity
;     DimLEDOCRs
;     LoadLEDOCRs
;   Pattern player - Low level:
;     LoadMappedPalette
;     FetchAnimFrame_Common
;     FetchAnimFrame_Delta_24b
;     FetchAnimFrame_Delta_12b
;     FetchAnimFrame_Paletted_256c
;     FetchAnimFrame_Paletted_16c
;     FetchAnimFrame_Paletted_4c
;     FetchAnimFrame_Invalid
;     FetchAnimFrame_Paletted_Common
;     ReadyPatternForLoading
;     FinishPatternLoading
;   Pattern player - High level:
;     GetEnumeratedSysPatAddr
;     LoadPattern
;     JumpToAnimFrame
;     FetchAnimFrame
;     AdvanceAnimCounters
;   Bank and Pattern-specific:
;     GetPatternFromBank_Tramp0
;     PatIsModified
;     UpdateEffectivePatMod
;     ModifyPattern_Tramp0
;     ForbidAllBlackPatMod_Tramp0
;   Last Used Pattern:
;     SetEWLAccessForLUP
;     GetLastUsedPattern
;     SaveLastUsedPattern
;   Favourites:
;     SetEWLAccessForFav
;     GetFavMetrics
;     GetFavouriteDataSlotIx
;     GetFavourite
;     SaveFavourite
;     SaveFavourite_HaveEWLAccess
;     ClampOrInvalidateFavIx
;     PromoteOrDelFavourite_Common
;     PromoteFavourite
;     DeleteFavourite
;   Simple settings in EPROM:
;     LoadRestrictionsFromEEPROM
;     SaveRestrictionsToEEPROM
;     LoadIntensityIxFromEEPROM
;     SaveIntensityIxToEEPROM
;   Input and UI functions:
;     WaitForReleaseOfAllButtons
;     RegisterButtonPress
;     AnimateCue
;     AnimateLongPress
;     AnimateBootMenuOption_Tramp0
;   Main functions:
;     Main
;
; Page 1 (0x0800..0x0FFF) and beyond
;
;   Basic system initialisation:
;     ClearLinearMemory_Page1
;     ClearCommonMemory_Page1
;     ConditionalyInitEEPROM_Page1
;     ClearMemAndInitHardware_Page1
;   Additional UI stuff:
;     Add4WBAPMOffset_Page1
;     GetPatternFromBank_Page1
;     ModifyPattern_Page1
;     ForbidAllBlackPatMod_Page1
;     AnimateBootMenuOption_Page1
;     AnimateCue_Page1
;     AnimatePatAdjMode_Page1
;     AnalysePaletteUse_Page1
;
;   Palette for system cues and menus
;   System error patterns
;   System cues and menus
;   System pattern addresses, enumerated
;   Basic palette, enumerated
;   Colour ramps (palette mappings to be applied to suitable patterns)
;   Ramp16 palettes
;   Ramp16 palette addresses, enumerated
;   Patterns
;   Pattern address table, enumerated
;   Pattern bank storage macros
;   Pattern banks
;   Configuration words


;------------------------------------------------------------------------------
; Target
;------------------------------------------------------------------------------


        ; The processor (MCU) type can be specified here or via the
        ; command line. E.g: "gpasm -p p16f1847 prettypoi.asm"

        PROCESSOR PIC16F1847

        ifdef __16F1847
          include p16f1847.inc
          radix dec
TARGET_MCU equ 2
LEDS_ORIENTAION equ 1
FULL_RAM_SIZE equ 1024
        endif

        ifndef TARGET_MCU
          error "Processor not specified. Specify p16f1827 or p16f1847."
        endif


;------------------------------------------------------------------------------
; Includes
;------------------------------------------------------------------------------


        RADIX dec
        include sfrmask.inc
        include upb8macros.inc
        include bapmmacros.inc
        include rgbmacros.inc
        include patmacros.inc
        include enumstmacros.inc
        RADIX dec


;------------------------------------------------------------------------------
; Pin assignments
;------------------------------------------------------------------------------


  if (LEDS_ORIENTAION) == 0
    ; Old series
UPB_LED3_RED    equ UPB('A', 4)
UPB_LED3_GREEN  equ UPB('B', 0)
UPB_LED3_BLUE   equ UPB('A', 3)

UPB_LED2_RED    equ UPB('B', 2)
UPB_LED2_GREEN  equ UPB('B', 3)
UPB_LED2_BLUE   equ UPB('B', 1)

UPB_LED1_RED    equ UPB('B', 5)
UPB_LED1_GREEN  equ UPB('B', 4)
UPB_LED1_BLUE   equ UPB('B', 6)
  endif

  if (LEDS_ORIENTAION) == 1
    ; New series
UPB_LED3_RED    equ UPB('B', 5)
UPB_LED3_GREEN  equ UPB('B', 4)
UPB_LED3_BLUE   equ UPB('B', 6)

UPB_LED2_RED    equ UPB('B', 2)
UPB_LED2_GREEN  equ UPB('B', 3)
UPB_LED2_BLUE   equ UPB('B', 1)

UPB_LED1_RED    equ UPB('A', 4)
UPB_LED1_GREEN  equ UPB('B', 0)
UPB_LED1_BLUE   equ UPB('A', 3)
  endif

UPB_MODE_BUTTON equ UPB('A', 7)
UPB_PAT_BUTTON  equ UPB('A', 0)
UPB_LATCH       equ UPB('A', 2)
UPB_SENSE       equ UPB('A', 1)

sfrm = 0
sfrm = sfrm | (1 << UPB_LED3_RED)
sfrm = sfrm | (1 << UPB_LED3_GREEN)
sfrm = sfrm | (1 << UPB_LED3_BLUE)
UPM_LED3 equ sfrm

sfrm = 0
sfrm = sfrm | (1 << UPB_LED2_RED)
sfrm = sfrm | (1 << UPB_LED2_GREEN)
sfrm = sfrm | (1 << UPB_LED2_BLUE)
UPM_LED2 equ sfrm

sfrm = 0
sfrm = sfrm | (1 << UPB_LED1_RED)
sfrm = sfrm | (1 << UPB_LED1_GREEN)
sfrm = sfrm | (1 << UPB_LED1_BLUE)
UPM_LED1 equ sfrm

sfrm = 0
sfrm = sfrm | (UPM_LED3)
sfrm = sfrm | (UPM_LED2)
sfrm = sfrm | (UPM_LED1)
UPM_ALL_LEDS equ sfrm

sfrm = 0
sfrm = sfrm | (UPB_MODE_BUTTON)
sfrm = sfrm | (UPB_PAT_BUTTON)
UPM_ALL_BUTTONS equ sfrm

sfrm = 0
sfrm = sfrm | (1 << UPB_LATCH)
sfrm = sfrm | UPM_ALL_LEDS
UPM_ALL_OUPUTS equ sfrm

sfrm = 0
sfrm = sfrm | (1 << UPB_SENSE)
sfrm = sfrm | UPM_ALL_BUTTONS
UPM_ALL_INPUTS equ sfrm


;------------------------------------------------------------------------------
; Constants
;------------------------------------------------------------------------------


TURNON_HOLDTIME         equ 1000  ; milliseconds
DEBOUNCE_PRESS_TIME     equ 40    ; milliseconds
DEBOUNCE_RELEASE_TIME   equ 60    ; milliseconds
EDITOR_CUE_TIME         equ 500   ; milliseconds

DEFAULT_INTENSITY_IX    equ 0     ; 0 is full intensity.

; Animation control flags
ACF_BIT_LOOP_COMPLETED  equ 7
ACF_BIT_FRAME_LOADED    equ 6
ACF_BIT_FORMAT0         equ 0
; The pattern display format occupies the lower bits of the ACF register.
ACF_FORMAT_MASK         equ 0x07

; Debounced button flags
INPUT_BIT_MODE          equ 0
INPUT_BIT_PATTERN       equ 1

; Frame control flags
FCF_BIT_DELTA           equ 7  ; 0 => Load, 1 => Delta

; Mode flags
MF_BIT_CUE              equ 7  ; A system feedback cue is playing.
MF_BIT_SLIDESHOW        equ 6  ; Slideshow mode is active.
MF_BIT_EXTERNALRAMPS    equ 5  ; Colour ramps may be set by the user.
MF_BIT_RAMP16           equ 4  ; A special single HQ ramp palette is used.
MF_BIT_NOFAVSPILL       equ 3  ; Adding a new Favourite requires a free slot.
MF_BIT_LUPLOCKED        equ 2  ; Last Used Pattern record no longer updated.
MF_BIT_FAVPROTECT       equ 1  ; Favourites list cannot be altered.
MF_BIT_RESTRICTED       equ 0  ; Bank cannot be changed.

x = 0
x |= (1 << MF_BIT_NOFAVSPILL)
x |= (1 << MF_BIT_LUPLOCKED)
x |= (1 << MF_BIT_FAVPROTECT)
x |= (1 << MF_BIT_RESTRICTED)
MF_MASK_RESTRICTIONS    equ x

DEFAULT_LOCK_BITS       equ (1 << MF_BIT_NOFAVSPILL)

; Byte-Addressed Program Memory pointer status byte
BAPM_BIT_CACHED         equ 7  ; Part of an odd byte was fetched earlier
BAPM_BIT_LOW_BYTES_ONLY equ 3  ; Signals plain, non-BAPM mode
BAPM_BIT_OFFSET0        equ 0  ; Byte index 0..6 within 4-word group

; Pattern display formats
PATDF_24B     equ 0  ; Direct colour load-or-delta (24 bpp)
PATDF_12B     equ 1  ; Direct colour load-or-delta (12 bpp)
PATDF_256C    equ 2  ; Indexed colour (256 colours, 8 bpp)
PATDF_16C     equ 3  ; Indexed colour (16 colours, 4 bpp)
PATDF_4C      equ 4  ; Indexed colour (4 colours, 2 bpp)

; Pattern BAPM storage format bits and masks
; (To save space, patterns which can be loaded to SRAM can be stored
; in the 7-bytes-in-4-words BAPM format.)
PAT_BAPM_BIT_FRAMES     equ 0
PAT_BAPM_BIT_MAP        equ 1
PAT_BAPM_BIT_PALETTE    equ 2
PATSF_BAPM_NONE     equ 0
PATSF_BAPM_F        equ (1 << PAT_BAPM_BIT_FRAMES)
PATSF_BAPM_M        equ (1 << PAT_BAPM_BIT_MAP)
PATSF_BAPM_MF       equ PATSF_BAPM_M | PATSF_BAPM_F
PATSF_BAPM_P        equ (1 << PAT_BAPM_BIT_PALETTE)
PATSF_BAPM_PF       equ PATSF_BAPM_P | PATSF_BAPM_F
PATSF_BAPM_PM       equ PATSF_BAPM_P | PATSF_BAPM_M
PATSF_BAPM_PMF      equ PATSF_BAPM_P | PATSF_BAPM_M | PATSF_BAPM_F

RAMP_IX_MASK        equ 0x3F

; Linear RAM metrics
LINEAR_RAM_SIZE     equ FULL_RAM_SIZE - 16
LINEAR_RAM_START    equ 0x2000
LINEAR_RAM_END      equ LINEAR_RAM_START + LINEAR_RAM_SIZE

; Where the expanded pattern goes, ready to be read into the pattern player
DEFAULT_PATTERN_ADDR  equ LINEAR_RAM_START + 80
MAX_PATTERN_SIZE      equ LINEAR_RAM_END - DEFAULT_PATTERN_ADDR
; (Straddling the Common SRAM between 0x2049 and 0x2050 is perfectly fine.)

; If the normal animation player is broken, the fallback shutdown cue
; will help with debugging by making programmed shutdown obvious.
USE_FALLBACK_SHUTDOWN_CUE   equ 0

; The PWM cycle frequency is used in expressions to calculate the required
; interval in units of PWM cycles.

PWM_CYCLE_FREQUENCY   equ 443  ; Hz,

SLIDESHOW_INTERVAL_1  equ 3000   ; milliseconds
SLIDESHOW_INTERVAL_2  equ 7000   ; milliseconds
SLIDESHOW_INTERVAL_3  equ 15000  ; milliseconds


;------------------------------------------------------------------------------
; Derived timing constants (and range checks)
;------------------------------------------------------------------------------


  if TURNON_HOLDTIME > 65535
    error "TURNON_HOLDTIME is too large."
  else
    if TURNON_HOLDTIME < 1
      error "TURNON_HOLDTIME is too small."
    endif
  endif

DEBOUNCE_PRESS_TIME_X8 equ 8 * DEBOUNCE_PRESS_TIME
  if DEBOUNCE_PRESS_TIME_X8 > 65535
    error "DEBOUNCE_PRESS_TIME is too large."
  else
    if DEBOUNCE_PRESS_TIME_X8 < 1
      error "DEBOUNCE_PRESS_TIME is too small."
    endif
  endif

DEBOUNCE_RELEASE_TIME_X8 equ 8 * DEBOUNCE_RELEASE_TIME
  if DEBOUNCE_RELEASE_TIME_X8 > 65535
    error "DEBOUNCE_RELEASE_TIME is too large."
  else
    if DEBOUNCE_RELEASE_TIME_X8 < 1
      error "DEBOUNCE_RELEASE_TIME is too small."
    endif
  endif

DB_RELEASE_TIME_PWM_CYCLES = DEBOUNCE_RELEASE_TIME * PWM_CYCLE_FREQUENCY / 1000
  if DB_RELEASE_TIME_PWM_CYCLES > 255
    error "DEBOUNCE_RELEASE_TIME is too large."
  else
    if DB_RELEASE_TIME_PWM_CYCLES < 1
      error "DEBOUNCE_RELEASE_TIME is too small."
    endif
  endif

EDITOR_CUE_TIME_PWM_CYCLES = EDITOR_CUE_TIME * PWM_CYCLE_FREQUENCY / 1000
  if EDITOR_CUE_TIME_PWM_CYCLES > 255
    error "EDITOR_CUE_TIME is too large."
  else
    if EDITOR_CUE_TIME_PWM_CYCLES < 1
      error "EDITOR_CUE_TIME is too small."
    endif
  endif

SSHOW_INTV_1_PWM_CYCLES_14 equ SLIDESHOW_INTERVAL_1 * PWM_CYCLE_FREQUENCY/1000
SSHOW_INTV_2_PWM_CYCLES_14 equ SLIDESHOW_INTERVAL_2 * PWM_CYCLE_FREQUENCY/1000
SSHOW_INTV_3_PWM_CYCLES_14 equ SLIDESHOW_INTERVAL_3 * PWM_CYCLE_FREQUENCY/1000

  if SSHOW_INTV_1_PWM_CYCLES_14 > 16383
    error "SLIDESHOW_INTERVAL_1 is too large."
  else
    if SSHOW_INTV_1_PWM_CYCLES_14 < 1
      error "SLIDESHOW_INTERVAL_1 is too small."
    endif
  endif
  if SSHOW_INTV_2_PWM_CYCLES_14 > 16383
    error "SLIDESHOW_INTERVAL_2 is too large."
  else
    if SSHOW_INTV_2_PWM_CYCLES_14 < 1
      error "SLIDESHOW_INTERVAL_2 is too small."
    endif
  endif
  if SSHOW_INTV_3_PWM_CYCLES_14 > 16383
    error "SLIDESHOW_INTERVAL_3 is too large."
  else
    if SSHOW_INTV_3_PWM_CYCLES_14 < 1
      error "SLIDESHOW_INTERVAL_3 is too small."
    endif
  endif


;------------------------------------------------------------------------------
; Structures
;------------------------------------------------------------------------------


; EEPROM wear-levelling access object
;
; At most, only the current index needs to be saved in SRAM. Even then,
; the current index can be recomputed by having the EWL Status Ring
; scanned again to find the slot index whose sequence number x isn't
; followed by a sequence number that is congruent to x + 1 modulo 256.
        CBLOCK 0
EWLAccess_CurrentIndex: 1
EWLAccess_Length: 1
EWLAccess_StatusRing: 1
EWLAccess_DataRing: 1
EWLAccess_DataSlotSize: 1
EWLAccessSize:
        ENDC

; EEPROM wear-levelling search arguments and result
        CBLOCK 0
EWLFind_DataSlotAddr: 1
EWLFind_Ix: 1
EWLFind_SeqNumber: 1
EWLFind_NextIx: 1
EWLFindSize:
        ENDC

; Last Used Pattern record in EEPROM wear levelling data slot
;
; Every time the Mode or Pattern button is pressed (and updating of
; the Last Used Pattern memory is enabled), the newly selected Bank
; and Pattern indices are saved to EEPROM. EEPROM memory cells are
; typically good for 100,000 writes. To avoid burning a hole in EEPROM
; memory with frequently updated data, both the data and its location
; is spread over many bytes in EEPROM.
        CBLOCK 0
LUPRec_BankIx: 1      ; 255 if referencing a Favourite pattern
LUPRec_PatternIx: 1   ; (or FavouriteIx)
LUPRecSize:           ; Program code currently expects the the size to be 2.
        ENDC

; Favourite pattern record in EEPROM wear levelling data slot
;
; The Favourite Patterns feature uses the same EEPROM Wear-Levelling
; system as the Last Used Pattern feature but with a much smaller Status
; and Data ring. Though favourites are added in increasing address order
; in EEPROM like LUPs, they are indexed by the application in decreasing
; address order (wrapping around within a ring), with the most recently
; added Favourite having an index of zero. A Favourites entry that is all
; bit ones (0xFF, 0xFF, 0xFF) is used in a ring that is less than full to
; mark the end of the Favourites list.

        CBLOCK 0
FavRec_BankIx: 1         ; [7:5]: TempoIx, [4:0]: Bank Index
FavRec_PatternIx: 1      ; [7:6]: RampIxC[1:0], [5:0]: Pattern Index
FacRec_RampIxA: 1        ; [7:6]: RampIxC[3:2], [5:0]: RampIxA
FacRec_RampIxB: 1        ; [7:6]: RampIxC[5:4], [5:0]: RampIxB
FavRecSize:              ; Program code currently expects the the size to be 4.
        ENDC

; Byte-Addressed Program Memory pointer
;
; The PIC16F1847 and PIC16F1827 uses 14-bit words in its program memory.
; Normally only the lower 8 bits of a PM word are accessed. The BAPM
; format allows 7 bytes to be packed into 4 words.
        CBLOCK 0
BAPM_Status: 1  ; Remember to clear BAPM_BIT_CACHED when modifying.
BAPM_OddFrag: 1
BAPMSize:
        ENDC

; Division-by-7 working space
;
; To allow random access of bytes in BAPM format, a divide-by-seven
; routine is needed to find which group of four PM words from a given
; location contain the byte to be accessed.
        CBLOCK 0
Div7_Dividend: 2
Div7_Quotient: 2
Div7_Remainder: 2
Div7_DDL: 1
Div7_DDH: 1
Div7_DDE: 1
Div7Size:
        ENDC

; Palette mapping working variables
;
; Patterns using palettes can be stored compactly by using display formats
; with few bits per colour index, referencing colour records in a small
; palette generated by mapping from a larger one.
        CBLOCK 0
MapWS_SourcePtr: 2
MapWS_DestPtr: 2
MapWS_MapPtr: 2
MapWS_SourceBAPMStatus: 1
MapWS_MapBAPMStatus: 1
MapWS_NumEntries: 1
MapWSSize:
        ENDC

; General animation state
;
; All that is necessary to sustain a a standard animating pattern is in
; (or referenced by) the General Animation State.
        CBLOCK 0
; Basic set-up parameters
GAS_PalettePtr: 2
GAS_ExpPatPtr: 2
GAS_NumFrames: 1
GAS_FrameRecordStride: 1
GAS_FramePeriodUnit: 1
GAS_AnimControlFlags: 1
; To be cleared during setup
GAS_FrameIx: 1
GAS_FrameTCounter: 1
GAS_FramePWMCycleCounter: 1
; Automatically set during animation
GAS_FramePeriod: 1
GAS_ExpPatFramePtr: 2
GASSize:
        ENDC

; Phase-correct Pulse-Width Modulation state
;
; The design of this software PCPWM is peculiar to the quirks of this
; Pic chip. Instead of a downward and upward counting PWM Match register,
; separate counters are used for each up-or-down phase for each RGB LED
; channel.
        CBLOCK 0
PCPWM9_LED3_Red_Ctr0: 1
PCPWM9_LED3_Red_Ctr1: 1
PCPWM9_LED3_Green_Ctr0: 1
PCPWM9_LED3_Green_Ctr1: 1
PCPWM9_LED3_Blue_Ctr0: 1
PCPWM9_LED3_Blue_Ctr1: 1
PCPWM9_LED2_Red_Ctr0: 1
PCPWM9_LED2_Red_Ctr1: 1
PCPWM9_LED2_Green_Ctr0:
PCPWM9_LED2_Green_Ctr1: 1
PCPWM9_LED2_Blue_Ctr0: 1
PCPWM9_LED2_Blue_Ctr1: 1
PCPWM9_LED1_Red_Ctr0: 1
PCPWM9_LED1_Red_Ctr1: 1
PCPWM9_LED1_Green_Ctr0: 1
PCPWM9_LED1_Green_Ctr1: 1
PCPWM9_LED1_Blue_Ctr0: 1
PCPWM9_LED1_Blue_Ctr1: 1
PCPWM9_IORegA_Delta: 1
PCPWM9_IORegB_Delta: 1
PCPWM9Size:
        ENDC

; Pattern Modifier
;
; All patterns can have their animation speed adjusted. Most can also have
; one or more user-selectable colours or colour ramps.
        CBLOCK 0
PatMod_TempoIx: 1  ; All patterns may have their tempo adjusted.
PatMod_RampIxA: 1
PatMod_RampIxB: 1
PatMod_RampIxC: 1
PatModSize:
        ENDC

; Palette Usage Metrics
;
; A pattern is analysed to find how many user-selectable ramps it has
; and whether or not it is entirely black.
        CBLOCK 0
PUM_IsPaletted: 1
PUM_HighestPalIx: 1
PUM_NumUserRamps: 1
PUMSize:
        ENDC


;------------------------------------------------------------------------------
; EEPROM addresses
;------------------------------------------------------------------------------


; The standard initial EEPROM state expected by the application is all
; bit ones (0xFF), though Intel hex files may show an unprogrammed byte
; as "FF00" to be consistent with the 14-bit program memory format.
; To present spurious advancement of the EEPROM Wear-Levelling rings,
; the EEPROM should be reset to the 0xFF in each byte,

FAVOURITES_NUM_SLOTS  equ 16
LUPEWL_NUM_SLOTS      equ 57  ; Last Used Pattern EEPROM wear levelling

        CBLOCK  0x00
EEPROM_PROBABLY_WORN_OUT_BY_NOW: 2  ; Probably worn out by the NS-2 V1.2
EEPROM_Brightness: 1
EEPROM_LockBits: 1
EEPROM_FavStatusRing: FAVOURITES_NUM_SLOTS
EEPROM_FavDataRing: FavRecSize * FAVOURITES_NUM_SLOTS
EEPROM_LUPStatusRing: LUPEWL_NUM_SLOTS
EEPROM_LUPDataRing: LUPRecSize * LUPEWL_NUM_SLOTS
EEPROM_END:
        ENDC

  if EEPROM_END > 256
    error "EEPROM memory limit exceeded."
  endif


;------------------------------------------------------------------------------
; Variables in SRAM
;------------------------------------------------------------------------------


        CBLOCK  0x0070

; The output of the pattern player is stored here. The delta mode of the
; 24-bit and 12-bit display format requires these levels to also be inputs,
; precluding in-place brightness control.
; There is no special reason for these level registers to be in Common RAM
; (0x70..0x7F) aside from allowing the expanded pattern in Linear Memory
; (which excludes Common RAM) to use some of the file registers at the end
; of Bank 0 of Traditional Memory.
LED3Level_Red: 1
LED3Level_Green: 1
LED3Level_Blue: 1
LED2Level_Red: 1
LED2Level_Green: 1
LED2Level_Blue: 1
LED1Level_Red: 1
LED1Level_Green: 1
LED1Level_Blue: 1

; These general-purpose calculation, counter and scratch registers are
; suitably placed in Common RAM, accessible whatever the state of the
; Bank Select Register.
PWMCounter: 0
Mult_u8_Multiplier: 0
Mult_u8_Product: 0
Mult_u8_ProductL: 0
LoopCtr0: 1
Mult_u8_ProductH: 0
PriorINTCONState: 0
LoopCtr1: 1
SlideshowCounter: 2
Scratch0: 1
Scratch1: 1
Scratch2: 1

        ; 16/16
        ENDC

        CBLOCK  0x0070
; As the palette mapping and player animation never occur at the same
; time, the palette mapping working variables can neatly overlap the
; LED level registers.
MapWS: MapWSSize  ; 9
        ENDC

        CBLOCK  0x0020

NumFavs: 1
FavHeadEWLSlotIx: 1
LUPEWLSlotIx: 1

FavouriteIx: 1      ; 255 means "no favourite is playing".
BankIx: 1           ; "Modes" are just indices of banks of patterns.
PatternIx: 1        ; (Within the current Bank)
PWMCycleCounter: 1  ; This is used for animation-assisted UI timing.
ModeFlags: 1        ; General operating mode, including cues and restrictions

PatternAddr: 2          ; Stored while cue is playing
GAState: GASSize        ; 14 bytes of General Animation State
IntensityIx: 1          ; 0 (for full brightness) to 4 (16th brightness)
PatAdjItemIx: 0         ; Index of item to adjust in Pattern Adjustment Mode
SlideshowIntervalIx: 1  ; Index into table of slideshow intervals in PWM cycles

PatternFUPScaler: 1     ; Frame Unit Period scaler: 0..3 => (1, 2, 8, 32)

UserPatMod: PatModSize        ; The user's favourite colours (and tempo)
FavRecPatMod: PatModSize      ; Fetched from a pattern in Favourites
DefaultPatMod: PatModSize     ; Bank-sourced palette modification
EffectivePatMod: PatModSize   ; To be saved when a Favourite Pattern is saved
ModFavouriteIx: 1
ModBankIx: 1
ModPatternIx: 1
PaletteUsageMetrics: PUMSize
        ; 49
        ENDC

        CBLOCK  0x0051
; EPROM Wear-Levelling and general EEPROM access
EWLFind: EWLFindSize  ; 4
EWLAccess: EWLAccessSize  ; 5
EEPROMPtr: 1
        ; 10
        ENDC

        CBLOCK  0x0051
; Peculiar stand-ins for OCRs for the Phase-Correct PWM implementation
PCPWM9State: PCPWM9Size
; Pretend Output Compare Registers, after intensity adjustment
LED3OCR_Red: 1
LED3OCR_Green: 1
LED3OCR_Blue: 1
LED2OCR_Red: 1
LED2OCR_Green: 1
LED2OCR_Blue: 1
LED1OCR_Red: 1
LED1OCR_Green: 1
LED1OCR_Blue: 1
        ; 29
        ENDC

        CBLOCK  0x0051
; Byte-Aligned Program Memory access and divide-by-7 working registers
BAPMRec: BAPMSize
Div7Rec: Div7Size
        ; 11
        ENDC


;==============================================================================
; Page 0 (0x0000..0x07FF)
;==============================================================================

        ORG     0

ResetVector:
        goto    Main

        nop
        nop
        nop


InterruptServiceRoutine:
        ; Do not use GOTO, as PCLATH could be set to any value.
        retfie


;------------------------------------------------------------------------------
; Set-up and shutdown
;------------------------------------------------------------------------------


ConfigureHardware:
; Set the CPU speed at the maximum (4MIPS) and fully configure the IO pins.
;
; The output latches for the IO pins connected to the cathodes for the common
; anode RGB LEDs are kept low. The states of the LEDs are controlled with the
; TRISx register bits rather than the PORTx or LATx bits. When a TRISx bit
; for an LED is 1, the corresponding IO pin has high impedance (high-Z) and
; the LED is dark. When the TRISx bit for that LED is 0, the output latch is
; connected and the IO pin sinks current from the cathode, lighting the LED.

        banksel PORTA
        ; Latch the power on so that the power no longer needs
        ; to be supplied through the Mode button.
        bsf     PORT_REGBIT_FR(UPB_LATCH)

        ; Set the internal oscillator to 16MHz. Since an instruction
        ; cycle takes 4 clock cycles, 4MIPS will be achieved.
        ;
        ; OSCCON.SPLLEN = 1      -> Software PLL enable (not used)
        ; OSCCON.IRCF0 = 0b1111) -> 16MHz (4MIPS)
        ; OSCCON.SCS0 = 0b10)    -> Internal clock source

        banksel OSCCON
        movlw   ((1 << SPLLEN) | (0xF << IRCF0) | (0x2 << SCS0))
        movwf   SFR_FR(OSCCON)

        ; OSCTUNE is a 6-bit 2's complement value in the range
        ; 0x20..0x00..0x1F. High values increase the frequency.
        movlw   0x00
        movwf   SFR_FR(OSCTUNE)

        banksel ANSELA
        clrf    SFR_FR(ANSELA)
        clrf    SFR_FR(ANSELB)

        banksel WPUA
        clrf    SFR_FR(WPUA)
        clrf    SFR_FR(WPUB)

        ; Set all but the Power Latch to input mode (high-Z).
        banksel TRISA
sfrm = ~(1 << UPB_LATCH)
        movlw   PAMASK_UPM(sfrm)
        movwf   SFR_FR(TRISA)
        movlw   PBMASK_UPM(sfrm)
        movwf   SFR_FR(TRISB)

        ; Set the output latches low for the LEDs so that each one is lit
        ; when its corresponding TRISx bit is low (output mode).
sfrm = (1 << UPB_LATCH)
        banksel PORTA
        movlw   PAMASK_UPM(sfrm)
        movwf   SFR_FR(PORTA)
        movlw   PBMASK_UPM(sfrm)
        movwf   SFR_FR(PORTB)

        ; Leave BSR in the default state.

        retlw   0


;------------------------------------------------------------------------------


  if USE_FALLBACK_SHUTDOWN_CUE

DelayBy325:
; Delay by 325 MCU cycles. At 4MIPS, that comes to 81.25us.
;
; Out: LoopCtr1:LoopCtr0 cleared
;      W preserved

        clrf    LoopCtr1
        clrf    LoopCtr0
        bsf     LoopCtr1, 6
        ; Fall through to DelayBy5NPlus5

DelayBy5NPlus5:
; Delay 5*(n + 1) cycles (including call) where n is LoopCtr.
; The maximum delay, where n = 65535, is 327680 cycles.
;
; The delay will not work properly if n is zero.
;
; In: LoopCtr1:LoopCtr0 (16-bit) set to the delay period in 5 CPU cucles.
; Out: LoopCtr1:LoopCtr0 cleared
;      W preserved

        incf    LoopCtr1, F
_Delay_Loop:
        decfsz  LoopCtr0, F
        bra     $ + 2
        decfsz  LoopCtr1, F
        bra     _Delay_Loop
        return

        ; 5*(n + 1) cycles, including call
  endif


;------------------------------------------------------------------------------


ShutDown:
; Switch the MCU off, ultimately by turning off the Power Latch transistor.

  if USE_FALLBACK_SHUTDOWN_CUE
        movlp   0x00
        movlb   0x01
; Briefly flash the LEDs red, then shut down properly.
sfrm = ~0
sfrm &= ~(1 << UPB_LED3_RED)
sfrm &= ~(1 << UPB_LED2_RED)
sfrm &= ~(1 << UPB_LED1_RED)
        movlw   PAMASK_UPM(sfrm)
        movwf   SFR_FR(TRISA)
        movlw   PBMASK_UPM(sfrm)
        movwf   SFR_FR(TRISB)
        call    DelayBy325
sfrm = ~0
sfrm &= ~(0 << UPB_LED3_RED)
sfrm &= ~(1 << UPB_LED2_RED)
sfrm &= ~(0 << UPB_LED1_RED)
        movlw   PAMASK_UPM(sfrm)
        movwf   SFR_FR(TRISA)
        movlw   PBMASK_UPM(sfrm)
        movwf   SFR_FR(TRISB)
        call    DelayBy325
        movlb   0x00
  endif
        ; Fall through to ShutDownImmediately.


;------------------------------------------------------------------------------


ShutDownImmediately:
; Shut down right now.
; Don't trust the stack pointer!

        movlb   0x01
sfrm = ~(1 << UPB_LATCH)
        movlw   PAMASK_UPM(sfrm)
        movwf   SFR_FR(TRISA)
        movlw   PBMASK_UPM(sfrm)
        movwf   SFR_FR(TRISB)
_SDI_ClearPowerLatch:
        movlb   0x04
        bcf     WPU_REGBIT_FR(UPB_LATCH)
        movlb   0x00
        bcf     PORT_REGBIT_FR(UPB_LATCH)
        movlb   0x01
        bcf     TRIS_REGBIT_FR(UPB_LATCH)
        movlb   0x00
_SDI_TheEndIsNigh:
        bra     _SDI_TheEndIsNigh


;------------------------------------------------------------------------------


MashTheSnoozeButton:
; At the very beginning of power-on, when the system clock is running at
; 500kHz, good for 125kIPS, this routine is called to ensure that the
; signal to turn on the MCU is not merely a transient.

_MtSB_Restart:
        ; Drain the power latch transistor's base.
        movlb   0x00
        bcf     PORT_REGBIT_FR(UPB_LATCH)
        movlb   0x01
        bcf     TRIS_REGBIT_FR(UPB_LATCH)
        movlb   0x00
        ; Given that the system clock is set to the default of 500kHz,
        ; LoopCtr1:LoopCtr0 will be set to the required hold time in
        ; milliseconds.
        movlw   LOW(TURNON_HOLDTIME)
        movwf   LoopCtr0
        movlw   HIGH(TURNON_HOLDTIME)
        movwf   LoopCtr1
        incf    LoopCtr1, F
_MtSB_Loop:
        btfss   PORT_REGBIT_FR(UPB_MODE_BUTTON)
        bra     _MtSB_Restart
        nop
        movlw   39  ; Set loop delay 8 + 3n cycles
_MtSB_InnerDelayLoop:
        decfsz  WREG, F
        bra     _MtSB_InnerDelayLoop
        decfsz  LoopCtr0, F
        bra     $ + 2
        decfsz  LoopCtr1, F
        bra     _MtSB_Loop  ; 125 cycles per iteratinn
        ; The Mode button has been pressed long enough.
        retlw   0


;------------------------------------------------------------------------------
; Mathematical functions
;------------------------------------------------------------------------------


Mult_u8:
; Multiply the unsigned 8-bit values in W and Mult_u8_Multiplier.
;
; In: W = Multiplicand
;     Mult_u8_Multiplier (same address as Mult_u8_ProductL)
; Out: Mult_u8_Product (16 bits)

MULT_U8_FRAG macro
        btfsc   STATUS, C
        addwf   Mult_u8_ProductH, F
        rrf     Mult_u8_ProductH, F
        rrf     Mult_u8_ProductL, F
  endm

        clrf    Mult_u8_ProductH
        rrf     Mult_u8_ProductL, F
        clrf    Scratch0
        bsf     Scratch0, 3
Mult_u8_Loop:
        MULT_U8_FRAG
        decfsz  Scratch0, F
        bra     Mult_u8_Loop
        retlw   0


;------------------------------------------------------------------------------


Div7_u16:
; Divide an unsigned 16-bit value by 7, rounding downwards.
;
; In: Div7Rec.Div7_Dividend (16 bits)
; Out: Div7Rec.Div7_Dividend preserved
;      Div7Rec.Div7_Quotient (16 bits)
;      Div7Rec.Div7_Remainder (16 bits, only lowest three bits significant)
;      W = Remainder (0..6)

        ; The division is performed by fixed-point multiplication of the
        ; dividend by an approximation of 1/7:
        ;
        ;   1/7 ~= 0.0010010010010010011 (rounded strictly upward)
        ;
        ;   Q = (D * m) >> 19  (good for D in 0..104858)
        ;
        ; where m = 74899 = 0x1_00100100_10010011.
        ;
        ; Though the multiplier is 17 significant bits long, only a total
        ; of seven shifts need be performed as the addition of the 3-byte
        ; shifted copy of the dividend, DDE:DDH:DDL, is offset within the
        ; partial quotient by varying byte offsets.
        ;
        ; The partial quotient Q is temporarily stored in file registers
        ; that will be overwritten with the integer quotient and remainder.
        ;
        ;   Q3: Quotient,  low byte
        ;   Q2: Quotient,  high byte
        ;   Q1: Remainder,  low byte
        ;   Q0: Remainder,  high byte

        ; Multiplier bit 0 and part of bit 16
        movf    Div7Rec + Div7_Dividend + 1, W
        movwf   Div7Rec + Div7_DDH
        clrf    Div7Rec + Div7_DDE
        movwf   Div7Rec + Div7_Remainder + 1
        movf    Div7Rec + Div7_Dividend + 0, W
        movwf   Div7Rec + Div7_DDL
        movwf   Div7Rec + Div7_Remainder + 0
        movwf   Div7Rec + Div7_Quotient + 0
        clrf    Div7Rec + Div7_Quotient + 1
        ; Multiplier bit 1
        call    _Div7_LSL1AndAddAtB0
        ; Multiplier bit 10
        call    _Div7_LSL1AndAddAtB1
        ; Multiplier bit 4
        call    _Div7_LSL2AndAddAtB0
        ; Multiplier bit 13
        call    _Div7_LSL1AndAddAtB1
        ; Multiplier bit 15
        call    _Div7_LSL2AndAddAtB0
        ; Complete the Multiplier bit 16 processing that was put off earlier.
        ; This is the only place an overflow into bit 32 (the Carry Flag)
        ; can occur. The Carry Flag will be shifted right back in during the
        ; shift-by-19 step (implemented as a shift-by-3 with the lower bytes
        ; discarded).
        movf    Div7Rec + Div7_Dividend + 1, W
        addwf   Div7Rec + Div7_Quotient + 1, F
        ; The 14-bit quotient lies in the Carry Flag and Q[31:19].
        rrf     Div7Rec + Div7_Quotient + 1, F
        rrf     Div7Rec + Div7_Quotient + 0, F
        lsrf    Div7Rec + Div7_Quotient + 1, F
        rrf     Div7Rec + Div7_Quotient + 0, F
        lsrf    Div7Rec + Div7_Quotient + 1, F
        rrf     Div7Rec + Div7_Quotient + 0, F
        ; Compute the remainder.
        ; R = D - 7Q
        ;   = D - ((Q<<3) - Q)
        ;   = D + Q - (Q<<3)
        lslf    Div7Rec + Div7_Quotient + 0, W
        lslf    WREG, F
        lslf    WREG, F
        subwf   Div7Rec + Div7_Quotient + 0, W
        addwf   Div7Rec + Div7_Dividend + 0, W
        movwf   Div7Rec + Div7_Remainder + 0
        ; Though the quotient is in the range 0..9362, only 3-bit shift
        ; and arithmetic operations are needed to return the correct
        ; remainder in the range 0..6. A whole byte is enough. For the
        ; sake of consistency, the upper byte of the remainder is cleared.
        clrf    Div7Rec + Div7_Remainder + 1
        return
_Div7_LSL1AndAddAtB1:
        lslf    Div7Rec + Div7_DDL, F
        rlf     Div7Rec + Div7_DDH, F
        rlf     Div7Rec + Div7_DDE, F
        movf    Div7Rec + Div7_DDL, W
        addwf   Div7Rec + Div7_Remainder + 1, F
        movf    Div7Rec + Div7_DDH, W
        addwfc  Div7Rec + Div7_Quotient + 0, F
        movf    Div7Rec + Div7_DDE, W
        addwfc  Div7Rec + Div7_Quotient + 1, F
        return
_Div7_LSL2AndAddAtB0:
        lslf    Div7Rec + Div7_DDL, F
        rlf     Div7Rec + Div7_DDH, F
        rlf     Div7Rec + Div7_DDE, F
_Div7_LSL1AndAddAtB0:
        lslf    Div7Rec + Div7_DDL, F
        rlf     Div7Rec + Div7_DDH, F
        rlf     Div7Rec + Div7_DDE, F
        movf    Div7Rec + Div7_DDL, W
        addwf   Div7Rec + Div7_Remainder + 0, F
        movf    Div7Rec + Div7_DDH, W
        addwfc  Div7Rec + Div7_Remainder + 1, F
        movf    Div7Rec + Div7_DDE, W
        addwfc  Div7Rec + Div7_Quotient + 0, F
        movlw   0
        addwfc  Div7Rec + Div7_Quotient + 1, F
        return


;------------------------------------------------------------------------------
; Memory functions
;------------------------------------------------------------------------------


ReadPMHighBits:
; Read the upper 6 bits of a 14-bit Program Memory word.
;
; This routine is used by ReadBAPMByte and FetchPMWordFromTable.
;
; In: FSR0 = Address in Program Memory (high bit optional)
; Out: W = Upper 6 bits of the 14-bit word at FSR0

        movlb   0x03
        movf    FSR0L, W
        movwf   SFR_FR(EEADRL)
        movf    FSR0H, W
        andlw   0x7F
        movwf   SFR_FR(EEADRH)
        bcf     SFR_FR(EECON1), CFGS
        bsf     SFR_FR(EECON1), EEPGD
        bcf     INTCON, GIE
        bsf     SFR_FR(EECON1), RD
        nop     ; Mandatory
        nop     ; Mandatory
        movlb   0x00
        btfsc   PriorINTCONState, GIE
        bsf     INTCON, GIE
        movlb   0x03
        movf    SFR_FR(EEDATH), W
        movlb   0x00
        andlw   0x3F
        return


;------------------------------------------------------------------------------


FetchPMWordFromTable:
; In: FSR0 = Raw 14-bits-per-entry table address in Program Memory
;     W = Index
; Out: FSR0 = Entry at index W (The caller may need to set bit 15.)

        addwf   FSR0L, F
        movlw   0
        addwfc  FSR0H, F
        call    ReadPMHighBits
        movwf   Scratch0
        movf    INDF0, W
        movwf   FSR0L
        movf    Scratch0, W
        movwf   FSR0H
        retlw   0


;------------------------------------------------------------------------------


ReadBAPMByte:
; Read a byte from Byte-Addressable Program Memory.
;
; The BAPM memory format stores seven 8-bit bytes in four 14-bit program
; words. In each group of four words, the four even bytes are stored in
; the lower 8 bits a program word and the three odd bytes are split among
; the upper 6-bit of the (14-bit) program words.
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
; In: FSR0 = Address in Program Memory (0x8000 onwards)
;     BAPMRec structure
; Out: W = Byte fetched
;      FSR0 and BAPMRec updated to reflect a post-increment

        btfsc   BAPMRec + BAPM_Status, BAPM_BIT_LOW_BYTES_ONLY
        bra     _ReadBAPMB_LBO_Mode
        incf    BAPMRec + BAPM_Status, F
        movf    BAPMRec + BAPM_Status, W
        andlw   0x07
        brw
        bra     _ReadBAPMB_0  ; Invalid state
        bra     _ReadBAPMB_0
        bra     _ReadBAPMB_1
        bra     _ReadBAPMB_2
        bra     _ReadBAPMB_3
        bra     _ReadBAPMB_4
        bra     _ReadBAPMB_5
_ReadBAPMB_6:
        clrf    BAPMRec + BAPM_Status
        moviw   FSR0++
        return
_ReadBAPMB_LBO_Mode:
        moviw   FSR0++
        return
_ReadBAPMB_0:
        call    _ReadBAPMB_ReadPMHi6
_ReadBAPMB_2:
_ReadBAPMB_4:
        movf    INDF0, W
        return
_ReadBAPMB_1:
        btfss   BAPMRec + BAPM_Status, BAPM_BIT_CACHED
        call    _ReadBAPMB_ReadPMHi6
        movf    BAPMRec + BAPM_OddFrag, W
        movwf   Scratch0
        addfsr  FSR0, 1
        call    _ReadBAPMB_ReadPMHi6
        btfsc   WREG, 0
        bsf     Scratch0, 6
        btfsc   WREG, 1
        bsf     Scratch0, 7
        movf    Scratch0, W
        return
_ReadBAPMB_3:
        btfss   BAPMRec + BAPM_Status, BAPM_BIT_CACHED
        call    _ReadBAPMB_ReadPMHi6
        lsrf    BAPMRec + BAPM_OddFrag, W
        movwf   Scratch0
        lsrf    Scratch0, F
        addfsr  FSR0, 1
        call    _ReadBAPMB_ReadPMHi6
        swapf   WREG, F
        andlw   0xF0
        iorwf   Scratch0, W
        return
_ReadBAPMB_5:
        btfss   BAPMRec + BAPM_Status, BAPM_BIT_CACHED
        call    _ReadBAPMB_ReadPMHi6
        lslf    BAPMRec + BAPM_OddFrag, W
        movwf   Scratch0
        lslf    Scratch0, F
        addfsr  FSR0, 1
        call    _ReadBAPMB_ReadPMHi6
        lslf    Scratch0, F
        rlf     WREG, F
        lslf    Scratch0, F
        rlf     WREG, F
        return
_ReadBAPMB_ReadPMHi6:
        call    ReadPMHighBits
        movwf   BAPMRec + BAPM_OddFrag
        bsf     BAPMRec + BAPM_Status, BAPM_BIT_CACHED
        return


;------------------------------------------------------------------------------


AddBAPMOffset:
; In: FSR0, BAPMRec = Base byte address in Program Memory
;     Div7Rec.Div7_Dividend = Byte offset to add (16 bits unsigned)
; Out: FSR0, BAPMRec = Target byte address in Program Memory
;      Div7Rec modified (including quotient, sometimes)
;
; Neither the the base address nor the beginning of the group-of-7
; implied by FSR0 and BAPMRec need be a multiple of 7 from 0x8000,
; the start of program memory. The byte offset within the Status
; field of BAPMRec is considered a part of the base address.
;
; This implementation decrements FSR0 by the amount necessary to get
; to where the group-of-7 byte offset is zero and then adds that byte
; offset to the 16-bit byte offset supplied by Div7_Dividend. (Care
; is taken to avoid overflowing the offset during the adjustment.)

        btfsc   BAPMRec + BAPM_Status, BAPM_BIT_LOW_BYTES_ONLY
        bra     _AddBAPMOffset_LBO_Mode
        btfss   Div7Rec + Div7_Dividend + 1, 7
        bra     _AddBAPMOffset_OffsetSafe
        ; A big jump! Avoid the risk of overflow by jumping forward
        ; by a whole group-of-7 bytes (4 words).
        movlw   7
        subwf   Div7Rec + Div7_Dividend + 0, F
        movlw   0
        subwfb  Div7Rec + Div7_Dividend + 1, F
        addfsr  FSR0, 4
_AddBAPMOffset_OffsetSafe:
        ; Step back to where the group-of-7 byte offset is zero and
        ; increase the 16-bit offset to add accordingly.
        movlw   0x07
        andwf   BAPMRec + BAPM_Status, F
        lsrf    BAPMRec + BAPM_Status, W
        subwf   FSR0L, F
        movlw   0
        subwfb  FSR0H, F
        movf    BAPMRec + BAPM_Status, W
        addwf   Div7Rec + Div7_Dividend + 0, F
        movlw   0
        addwfc  Div7Rec + Div7_Dividend + 1, F
        ; FSR0, BAPMRec now point to the beginning of the group-of-7 so
        ; that the multiplication of the offset by 4/7 works correctly.
        ; (This alignment need not be congruent to 0x8000.)

        ; With x as the (adjusted) 16-bit byte displacement to add, the
        ; word offset of the target group of four words (seven bytes)
        ; is 4 * floor(x / 7).
        call    Div7_u16
        lslf    Div7Rec + Div7_Quotient + 0, F
        rlf     Div7Rec + Div7_Quotient + 1, F
        lslf    Div7Rec + Div7_Quotient + 0, F
        rlf     Div7Rec + Div7_Quotient + 1, F
        movf    Div7Rec + Div7_Quotient + 0, W
        addwf   FSR0L, F
        movf    Div7Rec + Div7_Quotient + 1, W
        addwfc  FSR0H, F
        ; The word offset within the group is floor(remainder / 2)
        lsrf    Div7Rec + Div7_Remainder, W
        addwf   FSR0L, F
        movlw   0
        addwfc  FSR0H, F
        movf    Div7Rec + Div7_Remainder, W
        ; The remainder goes directly into the BAPM Status byte, clearing
        ; the cache bit in the process.
        movwf   BAPMRec + BAPM_Status
        return
_AddBAPMOffset_LBO_Mode:
        movf    Div7Rec + Div7_Dividend + 0, W
        addwf   FSR0L, F
        movf    Div7Rec + Div7_Dividend + 1, W
        addwfc  FSR0H, F
        movf    BAPMRec + BAPM_Status, W
        return


;------------------------------------------------------------------------------
; EEPROM functions
;------------------------------------------------------------------------------


ReadEEPROMByte:
; In: EEPROMPtr
; Out: W = Byte fetched from EEPROM
;      EEPROMPtr incremented

        movf    EEPROMPtr, W
        incf    EEPROMPtr, F
        movlb   0x03
        movwf   SFR_FR(EEADRL)
        bcf     SFR_FR(EECON1), CFGS
        bcf     SFR_FR(EECON1), EEPGD
        bsf     SFR_FR(EECON1), RD
        movf    SFR_FR(EEDAT), W
        movlb   0x00
        return


;------------------------------------------------------------------------------


WriteEEPROMByte:
; In: EEPROMPtr
;     W = Byte to write to EEPROM
; Out: EEPROMPtr incremented

        movlb   0x03
        movwf   SFR_FR(EEDAT)
        movlb   0x00
        movf    EEPROMPtr, W
        incf    EEPROMPtr, F
        movf    INTCON, F
        movwf   PriorINTCONState
        movlb   0x03
        movwf   SFR_FR(EEADRL)
        bcf     SFR_FR(EECON1), CFGS
        bcf     SFR_FR(EECON1), EEPGD
        bsf     SFR_FR(EECON1), WREN
        bcf     INTCON, GIE
        movlw   0x55
        movwf   SFR_FR(EECON2)
        movlw   0xAA
        movwf   SFR_FR(EECON2)
        bsf     SFR_FR(EECON1), WR
        movlb   0x00
        btfsc   PriorINTCONState, GIE
        bsf     INTCON, GIE
        movlb   0x03
        bcf     SFR_FR(EECON1), WREN
_WriteEB_Wait:
        btfsc   SFR_FR(EECON1), WR
        goto    _WriteEB_Wait
        movlb   0x00
        retlw   0


;------------------------------------------------------------------------------


WriteEEPROMByte_Smart:
; Update the byte in EEPROM only if is not already the target value.
;
; In: EEPROMPtr
;     W = Byte to write to EEPROM, if it is different to what's already there.
; Out: EEPROMPtr incremented whether not the byte was actually written

        movwf   Scratch1
        call    ReadEEPROMByte
        xorwf   Scratch1, F
        btfsc   STATUS, Z
        return
        decf    EEPROMPtr, F
        xorwf   Scratch1, W
        goto    WriteEEPROMByte


;------------------------------------------------------------------------------


;ReadEEPROMBlock:
;; In: W = Number of bytes to read from EEPROM (0 means none)
;;     EEPROMPtr = Source in EEPROM
;;     FSR0 = Destination in RAM
;; Out: EEPROMPtr and PSR0 incremented
;
;        movf    WREG, W
;        btfsc   STATUS, Z
;        retlw   0
;        movwf   LoopCtr0
;_ReadEEPBlock_Loop:
;        call    ReadEEPROMByte
;        movwi   FSR0++
;        decfsz  LoopCtr0, F
;        bra     _ReadEEPBlock_Loop
;        retlw   0


;------------------------------------------------------------------------------


;WriteEEPROMBlock:
;; In: W = Number of bytes to write to EEPROM (0 means none)
;;     FSR0 = Source in RAM
;;     EEPROMPtr = Destination in EEPROM
;; Out: EEPROMPtr and PSR0 incremented
;
;        movf    WREG, W
;        btfsc   STATUS, Z
;        retlw   0
;        movwf   LoopCtr0
;_WriteEEPBlock_Loop:
;        moviw   FSR0++
;        call    WriteEEPROMByte
;        decfsz  LoopCtr0, F
;        bra     _WriteEEPBlock_Loop
;        retlw   0


;------------------------------------------------------------------------------
; EEPROM wear-levelling functions
;------------------------------------------------------------------------------


FindCurrentEWLIndex:
; Find slot index of the most recent entry written.
;
; The slot index of the most recent entry is the one whose sequence
; number x isn't followed (in the Status Ring) by a sequence number
; that is congruent to x + 1 modulo 256. (The search will always
; return the correct result for any ring length from 1 to 255 and
; will likely work consistently whatever the initial state of the
; EEPROM.)
;
; In: FSR0 = EWL access object, initialised (except for current index)
; Out: EWL access object updated with the current index.
;      W = EWLFind.EWLFind_Ix = Current index

        clrf    EWLFind + EWLFind_NextIx
        moviw   EWLAccess_StatusRing[FSR0]
        movwf   EEPROMPtr
        call    ReadEEPROMByte
        movwf   EWLFind + EWLFind_SeqNumber
FindCurrentEWLIndex_Loop:
        movf    EWLFind + EWLFind_NextIx, W
        movwf   EWLFind + EWLFind_Ix
        ; Look ahead and get the next sequence number.
        incf    EWLFind + EWLFind_Ix, W
        movwf   EWLFind + EWLFind_NextIx
        moviw   EWLAccess_Length[FSR0]
        subwf   EWLFind + EWLFind_NextIx, W
        btfsc   STATUS, C
        clrf    EWLFind + EWLFind_NextIx
        moviw   EWLAccess_StatusRing[FSR0]
        addwf   EWLFind + EWLFind_NextIx, W
        movwf   EEPROMPtr
        call    ReadEEPROMByte
        ; See if the next sequence number is a successor to the current one.
        incf    EWLFind + EWLFind_SeqNumber, F
        subwf   EWLFind + EWLFind_SeqNumber, W
        btfsc   STATUS, Z
        bra     FindCurrentEWLIndex_Loop
        movf    EWLFind + EWLFind_Ix, W
        movwi   EWLAccess_CurrentIndex[FSR0]
        return


;------------------------------------------------------------------------------


GetEWLDataAddress:
; Get the address in EEPROM of a particular data slot. If the given slot
; index is -1 (255 unsigned), the address of the last slot will be returned.
; If the slot index is beyond the last valid index (Length - 1), the address
; of the first slot will be returned. This makes it easy to fetch the next
; data slot in the ring when writing a new record.
;
; When saving a new entry, first write the new entry at the address
; returned by this function, without altering the current index,
; then call AdvanceEWLStatusRing. If a power loss occurs, the new
; (and likely incomplete) entry will be safely ignored.
;
; In: FSR0 = EWL access object
;      W = Slot index
; Out: EWLFind.EWLFind_DataSlotAddr = Data slot address for the given index
;      W = Validated slot index

        movwf   EWLFind + EWLFind_Ix
        moviw   EWLAccess_DataSlotSize[FSR0]
        movwf   Mult_u8_Multiplier
        incfsz  EWLFind + EWLFind_Ix, W
        bra     _GEWLDA_NotLast
        ; -1 => Last entry
        moviw   EWLAccess_Length[FSR0]
        movwf   EWLFind + EWLFind_Ix
        decf    EWLFind + EWLFind_Ix, F
        bra     _GEWLDA_HaveIx
_GEWLDA_NotLast:
        moviw   EWLAccess_Length[FSR0]
        subwf   EWLFind + EWLFind_Ix, W
        btfsc   STATUS, C
        clrf    EWLFind + EWLFind_Ix
_GEWLDA_HaveIx:
        movf    EWLFind + EWLFind_Ix, W
        call    Mult_u8
        moviw   EWLAccess_DataRing[FSR0]
        addwf   Mult_u8_ProductL, W
        movwf   EWLFind + EWLFind_DataSlotAddr
        movf    EWLFind + EWLFind_Ix, W
        return


;-------------------------------------------------------------------------------


AdvanceEWLStatusRing:
; Update the EEPROM Wear-Levelling status ring.
;
; After successfully writing a new entry at the Data slot address
; returned by GetEWLDataAddress called with an incremented slot index
; (without altering the current index), call this function to mark
; that slot as the current one.
;
; The marking is done by writing in the Status Ring at the next slot
; index (the one corresponding to the new entry), a sequence number
; which properly follows the current slot's sequence number.
;
; In: FSR0 = EWL access object
; Out: W = Updated slot index (already stored at FSR0)

        moviw   EWLAccess_CurrentIndex[FSR0]
        movwf   EWLFind + EWLFind_Ix
        moviw   EWLAccess_StatusRing[FSR0]
        addwf   EWLFind + EWLFind_Ix, W
        movwf   EEPROMPtr
        call    ReadEEPROMByte
        movwf   EWLFind + EWLFind_SeqNumber
        incf    EWLFind + EWLFind_SeqNumber, F
        incf    EWLFind + EWLFind_Ix, F
        moviw   EWLAccess_Length[FSR0]
        subwf   EWLFind + EWLFind_Ix, W
        btfsc   STATUS, C
        clrf    EWLFind + EWLFind_Ix
        ; Update the access object.
        movf    EWLFind + EWLFind_Ix, W
        movwi   EWLAccess_CurrentIndex[FSR0]
        ; Write the incremented sequence number.
        moviw   EWLAccess_StatusRing[FSR0]
        addwf   EWLFind + EWLFind_Ix, W
        movwf   EEPROMPtr
        movf    EWLFind + EWLFind_SeqNumber, W
        call    WriteEEPROMByte
        moviw   EWLAccess_CurrentIndex[FSR0]
        return


;------------------------------------------------------------------------------
; Pulse Width Modulation (PWM)
;
; The LEDs are driven with Pulse Width Modulation and the portion of the PWM
; cycle in which an LED is lit is determined by its Output Compare Register.
; During each PWM cycle, each LED's OCR is compared to the rising PWM counter
; and those which match cause the corresponding LED to switch off. At the
; beginning of the cycle, the LEDs with a non-zero OCR are switched on.
;
; The PWM implementation is double-buffered, though as the PWM cycle is in
; a busy loop, the double-buffering is not necessary to avoid PWM glitches.
;
; For a 16MHz clock (4MIPS), this routine takes 2.5ms, so a PWM frequency
; of 400Hz is achievable. Because this software PWM ties up the MCU, all the
; animation code has to execute in a blanking period.
;
;------------------------------------------------------------------------------


;PerformFastPWMCycle:
;
;        movlb   0x01
;        movf    LED3OCR_Red, W
;        btfss   STATUS, Z
;        bcf     TRIS_REGBIT_FR(UPB_LED3_RED)
;        nop
;        movf    LED3OCR_Green, W
;        btfss   STATUS, Z
;        bcf     TRIS_REGBIT_FR(UPB_LED3_GREEN)
;        nop
;        movf    LED3OCR_Blue, W
;        btfss   STATUS, Z
;        bcf     TRIS_REGBIT_FR(UPB_LED3_BLUE)
;        nop
;        movf    LED2OCR_Red, W
;        btfss   STATUS, Z
;        bcf     TRIS_REGBIT_FR(UPB_LED2_RED)
;        nop
;        movf    LED2OCR_Green, W
;        btfss   STATUS, Z
;        bcf     TRIS_REGBIT_FR(UPB_LED2_GREEN)
;        nop
;        movf    LED2OCR_Blue, W
;        btfss   STATUS, Z
;        bcf     TRIS_REGBIT_FR(UPB_LED2_BLUE)
;        nop
;        movf    LED1OCR_Red, W
;        btfss   STATUS, Z
;        bcf     TRIS_REGBIT_FR(UPB_LED1_RED)
;        nop
;        movf    LED1OCR_Green, W
;        btfss   STATUS, Z
;        bcf     TRIS_REGBIT_FR(UPB_LED1_GREEN)
;        nop
;        movf    LED1OCR_Blue, W
;        btfss   STATUS, Z
;        bcf     TRIS_REGBIT_FR(UPB_LED1_BLUE)
;        nop
;        movlw   1
;        movwf   PWMCounter
;        ; 38 cycles
;_PPWMC_Loop:
;        movf    PWMCounter, W
;        subwf   LED3OCR_Red, W
;        btfsc   STATUS, Z
;        bsf     TRIS_REGBIT_FR(UPB_LED3_RED)
;        movf    PWMCounter, W
;        subwf   LED3OCR_Green, W
;        btfsc   STATUS, Z
;        bsf     TRIS_REGBIT_FR(UPB_LED3_GREEN)
;        movf    PWMCounter, W
;        subwf   LED3OCR_Blue, W
;        btfsc   STATUS, Z
;        bsf     TRIS_REGBIT_FR(UPB_LED3_BLUE)
;        movf    PWMCounter, W
;        subwf   LED2OCR_Red, W
;        btfsc   STATUS, Z
;        bsf     TRIS_REGBIT_FR(UPB_LED2_RED)
;        movf    PWMCounter, W
;        subwf   LED2OCR_Green, W
;        btfsc   STATUS, Z
;        bsf     TRIS_REGBIT_FR(UPB_LED2_GREEN)
;        movf    PWMCounter, W
;        subwf   LED2OCR_Blue, W
;        btfsc   STATUS, Z
;        bsf     TRIS_REGBIT_FR(UPB_LED2_BLUE)
;        movf    PWMCounter, W
;        subwf   LED1OCR_Red, W
;        btfsc   STATUS, Z
;        bsf     TRIS_REGBIT_FR(UPB_LED1_RED)
;        movf    PWMCounter, W
;        subwf   LED1OCR_Green, W
;        btfsc   STATUS, Z
;        bsf     TRIS_REGBIT_FR(UPB_LED1_GREEN)
;        movf    PWMCounter, W
;        subwf   LED1OCR_Blue, W
;        btfsc   STATUS, Z
;        bsf     TRIS_REGBIT_FR(UPB_LED1_BLUE)
;        incfsz  PWMCounter, F
;        bra     _PPWMC_Loop  ; 39 cycles in loop
;        movlb   0x00
;        ; 9985 cycles
;        retlw   0
;
;        ; 9989 cycles, including call


;------------------------------------------------------------------------------
; Phase-correct Pulse Width Modulation (PWM)
;
; The LEDs are driven with Phase-correct Pulse Width Modulation and the
; portion of the PWM cycle in which an LED is lit is determined by its
; Output Compare Register (as named for the usual PWM schemes).
;
; The usual or "Fast" PWM is implemented with a rising counter that causes
; each LED to turn off when its OCR matches the counter. (Only the LEDs with
; non-zero levels begin the PWM cycle lit.)
;
; Phase-correct PWM is usually implemented with a counter rising and
; falling, bouncing at the upper and lower limits, causing PWM pulses
; to be centred within the cycle (or centred at the ends) at the cost
; of halving the PWM frequency if precision is to be maintained.
;
; This implementation runs a full speed, full precision PWM with (mostly)
; correct phase by running each half-cycle at half-precision and extending
; the first half by one unit if the output compare register has an odd
; value. This causes the phase to jitter by only half the PWM counter
; tick period.
;
; For each LED, separate counters are used for the first and second halves
; of the cycle to make use of the INCFSZ and DECFSZ instructions. (There is
; no INCFSNZ and DECFSNZ instruction, so accumulating masks are used to
; provide the same effect.)
;
; The PWM implementation is effectively double-buffered, though as the PWM
; cycle is in a busy loop, the double-buffering is not necessary to avoid
; PWM glitches.
;
; For a 16MHz clock (4MIPS), this routine takes 2.256ms, so a PWM frequency
; of 443Hz is achievable. Because this software PWM ties up the MCU, all the
; animation code has to execute in a blanking period.
;
;------------------------------------------------------------------------------


PerformPCPWMCycle:
; The behaviour of this Phase-Correct Pulse-Width Modulation implementation
; is described with a simplified illustration of a 3-bit PCPWM scheme.
; various values of the output level x in the range 0..7 are shown on the
; horizontal axis and time advances down the vertical axis. The numbers
; in the table indicate the counter values for the first and second phase
; of the PWM cycle. In the first phase, the output is activated the moment
; the incrementing counter overflows to zero. In the second phase, the
; output is deactivated the moment the decrementing counter reaches zero.
;
; x: 0 1 2 3 4 5 6 7     0 1 2 3 4 5 6 7
;
;   _3_4_4_5_5_6_6_7_   _________________ Ctr0 = (x>>1) + MSBVal - 1 + (x & 1)
;    4 5 5 6 6 7 7 0                   *
;    5 6 6 7 7 0 0 1               * # #
;    6 7 7 0 0 1 1 2           * # # # #
;   _7_0_0_1_1_2_2_3_   ___*_#_#_#_#_#_#_
;
;   _1_1_2_2_3_3_4_4_   _________________ Ctr1 = (x >> 1) + 1
;    0 0 1 1 2 2 3 3         # # # # # #
;    7 7 0 0 1 1 2 2             # # # #
;    6 6 7 7 0 0 1 1                 # #
;   _5_5_6_6_7_7_0_0_   _________________
;
;    x: Output Compare Register (as used for Fast PWM)
;    0: marks state change
;    #: LED lit
;    *: LED lit due to a bit 1 in the LSB of x
;
; Each LED (3 in each RGB LED) has its own pair of counters.

        ; If the last PWM left the power sagging so badly that
        ; the Sense pin has gone low, halt until the Sense pin
        ; is high again.
        btfss   PORT_REGBIT_FR(UPB_SENSE)
        bra     $ - 1

        ; Set up the counters for the first and second half of the PC-PWM.

        lsrf    LED3OCR_Red, W
        movwf   PCPWM9State + PCPWM9_LED3_Red_Ctr0
        movwf   PCPWM9State + PCPWM9_LED3_Red_Ctr1
        movlw   127
        addwfc  PCPWM9State + PCPWM9_LED3_Red_Ctr0, F
        incf    PCPWM9State + PCPWM9_LED3_Red_Ctr1, F
        lsrf    LED3OCR_Green, W
        movwf   PCPWM9State + PCPWM9_LED3_Green_Ctr0
        movwf   PCPWM9State + PCPWM9_LED3_Green_Ctr1
        movlw   127
        addwfc  PCPWM9State + PCPWM9_LED3_Green_Ctr0, F
        incf    PCPWM9State + PCPWM9_LED3_Green_Ctr1, F
        lsrf    LED3OCR_Blue, W
        movwf   PCPWM9State + PCPWM9_LED3_Blue_Ctr0
        movwf   PCPWM9State + PCPWM9_LED3_Blue_Ctr1
        movlw   127
        addwfc  PCPWM9State + PCPWM9_LED3_Blue_Ctr0, F
        incf    PCPWM9State + PCPWM9_LED3_Blue_Ctr1, F

        lsrf    LED2OCR_Red, W
        movwf   PCPWM9State + PCPWM9_LED2_Red_Ctr0
        movwf   PCPWM9State + PCPWM9_LED2_Red_Ctr1
        movlw   127
        addwfc  PCPWM9State + PCPWM9_LED2_Red_Ctr0, F
        incf    PCPWM9State + PCPWM9_LED2_Red_Ctr1, F
        lsrf    LED2OCR_Green, W
        movwf   PCPWM9State + PCPWM9_LED2_Green_Ctr0
        movwf   PCPWM9State + PCPWM9_LED2_Green_Ctr1
        movlw   127
        addwfc  PCPWM9State + PCPWM9_LED2_Green_Ctr0, F
        incf    PCPWM9State + PCPWM9_LED2_Green_Ctr1, F
        lsrf    LED2OCR_Blue, W
        movwf   PCPWM9State + PCPWM9_LED2_Blue_Ctr0
        movwf   PCPWM9State + PCPWM9_LED2_Blue_Ctr1
        movlw   127
        addwfc  PCPWM9State + PCPWM9_LED2_Blue_Ctr0, F
        incf    PCPWM9State + PCPWM9_LED2_Blue_Ctr1, F

        lsrf    LED1OCR_Red, W
        movwf   PCPWM9State + PCPWM9_LED1_Red_Ctr0
        movwf   PCPWM9State + PCPWM9_LED1_Red_Ctr1
        movlw   127
        addwfc  PCPWM9State + PCPWM9_LED1_Red_Ctr0, F
        incf    PCPWM9State + PCPWM9_LED1_Red_Ctr1, F
        lsrf    LED1OCR_Green, W
        movwf   PCPWM9State + PCPWM9_LED1_Green_Ctr0
        movwf   PCPWM9State + PCPWM9_LED1_Green_Ctr1
        movlw   127
        addwfc  PCPWM9State + PCPWM9_LED1_Green_Ctr0, F
        incf    PCPWM9State + PCPWM9_LED1_Green_Ctr1, F
        lsrf    LED1OCR_Blue, W
        movwf   PCPWM9State + PCPWM9_LED1_Blue_Ctr0
        movwf   PCPWM9State + PCPWM9_LED1_Blue_Ctr1
        movlw   127
        addwfc  PCPWM9State + PCPWM9_LED1_Blue_Ctr0, F
        incf    PCPWM9State + PCPWM9_LED1_Blue_Ctr1, F

        ; 56 cycles

        movlw   128
        movwf   LoopCtr0
        movwf   LoopCtr1

sfrm = ~(1 << UPB_LATCH)
        movlw   PAMASK_UPM(sfrm)
        movwf   Scratch0
        movlw   PBMASK_UPM(sfrm)
        movwf   Scratch1

        ; 63 cycles

; A macro makes the code to select a virtual PORTx delta or accumulating
; register easy to read.
#define PCPWMMaskAddrPB(bf, upb) PCPWM9State + (bf) + ((upb) >> 3), PBIT(upb)

        ; First half: Increment each LED's counter register and switch
        ; on the LED when that counter overflows to zero.

_PPCPWMC_Loop0:
sfrm = ~UPM_ALL_LEDS
        movlw   PAMASK_UPM(sfrm)
        movwf   PCPWM9State + PCPWM9_IORegA_Delta
        movlw   PBMASK_UPM(sfrm)
        movwf   PCPWM9State + PCPWM9_IORegB_Delta
        incfsz  PCPWM9State + PCPWM9_LED3_Red_Ctr0, F
        bsf     PCPWMMaskAddrPB(PCPWM9_IORegA_Delta, UPB_LED3_RED)
        incfsz  PCPWM9State + PCPWM9_LED3_Green_Ctr0, F
        bsf     PCPWMMaskAddrPB(PCPWM9_IORegA_Delta, UPB_LED3_GREEN)
        incfsz  PCPWM9State + PCPWM9_LED3_Blue_Ctr0, F
        bsf     PCPWMMaskAddrPB(PCPWM9_IORegA_Delta, UPB_LED3_BLUE)
        incfsz  PCPWM9State + PCPWM9_LED2_Red_Ctr0, F
        bsf     PCPWMMaskAddrPB(PCPWM9_IORegA_Delta, UPB_LED2_RED)
        incfsz  PCPWM9State + PCPWM9_LED2_Green_Ctr0, F
        bsf     PCPWMMaskAddrPB(PCPWM9_IORegA_Delta, UPB_LED2_GREEN)
        incfsz  PCPWM9State + PCPWM9_LED2_Blue_Ctr0, F
        bsf     PCPWMMaskAddrPB(PCPWM9_IORegA_Delta, UPB_LED2_BLUE)
        incfsz  PCPWM9State + PCPWM9_LED1_Red_Ctr0, F
        bsf     PCPWMMaskAddrPB(PCPWM9_IORegA_Delta, UPB_LED1_RED)
        incfsz  PCPWM9State + PCPWM9_LED1_Green_Ctr0, F
        bsf     PCPWMMaskAddrPB(PCPWM9_IORegA_Delta, UPB_LED1_GREEN)
        incfsz  PCPWM9State + PCPWM9_LED1_Blue_Ctr0, F
        bsf     PCPWMMaskAddrPB(PCPWM9_IORegA_Delta, UPB_LED1_BLUE)
        movf    PCPWM9State + PCPWM9_IORegA_Delta, W
        andwf   Scratch0, F
        movf    PCPWM9State + PCPWM9_IORegB_Delta, W
        andwf   Scratch1, F
        banksel TRISA
        movf    Scratch0, W
        movwf   SFR_FR(TRISA)
        movf    Scratch1, W
        movwf   SFR_FR(TRISB)
        banksel PORTA
        decfsz  LoopCtr0, F
        bra     _PPCPWMC_Loop0  ; 35 cycles in loop, iterated 128 times
        nop

        ; 4543 cycles

        ; Second half: Decrement each LED's counter register and switch
        ; off the LED when that counter reaches zero.

_PPCPWMC_Loop1:
sfrm = UPM_ALL_LEDS
        movlw   PAMASK_UPM(sfrm)
        movwf   PCPWM9State + PCPWM9_IORegA_Delta
        movlw   PBMASK_UPM(sfrm)
        movwf   PCPWM9State + PCPWM9_IORegB_Delta
        decfsz  PCPWM9State + PCPWM9_LED3_Red_Ctr1, F
        bcf     PCPWMMaskAddrPB(PCPWM9_IORegA_Delta, UPB_LED3_RED)
        decfsz  PCPWM9State + PCPWM9_LED3_Green_Ctr1, F
        bcf     PCPWMMaskAddrPB(PCPWM9_IORegA_Delta, UPB_LED3_GREEN)
        decfsz  PCPWM9State + PCPWM9_LED3_Blue_Ctr1, F
        bcf     PCPWMMaskAddrPB(PCPWM9_IORegA_Delta, UPB_LED3_BLUE)
        decfsz  PCPWM9State + PCPWM9_LED2_Red_Ctr1, F
        bcf     PCPWMMaskAddrPB(PCPWM9_IORegA_Delta, UPB_LED2_RED)
        decfsz  PCPWM9State + PCPWM9_LED2_Green_Ctr1, F
        bcf     PCPWMMaskAddrPB(PCPWM9_IORegA_Delta, UPB_LED2_GREEN)
        decfsz  PCPWM9State + PCPWM9_LED2_Blue_Ctr1, F
        bcf     PCPWMMaskAddrPB(PCPWM9_IORegA_Delta, UPB_LED2_BLUE)
        decfsz  PCPWM9State + PCPWM9_LED1_Red_Ctr1, F
        bcf     PCPWMMaskAddrPB(PCPWM9_IORegA_Delta, UPB_LED1_RED)
        decfsz  PCPWM9State + PCPWM9_LED1_Green_Ctr1, F
        bcf     PCPWMMaskAddrPB(PCPWM9_IORegA_Delta, UPB_LED1_GREEN)
        decfsz  PCPWM9State + PCPWM9_LED1_Blue_Ctr1, F
        bcf     PCPWMMaskAddrPB(PCPWM9_IORegA_Delta, UPB_LED1_BLUE)
        movf    PCPWM9State + PCPWM9_IORegA_Delta, W
        iorwf   Scratch0, F
        movf    PCPWM9State + PCPWM9_IORegB_Delta, W
        iorwf   Scratch1, F
        banksel TRISA
        movf    Scratch0, W
        movwf   SFR_FR(TRISA)
        movf    Scratch1, W
        movwf   SFR_FR(TRISB)
        banksel PORTA
        decfsz  LoopCtr1, F
        bra     _PPCPWMC_Loop1  ; 35 cycles in loop, iterated 128 times

        ; 9022 cycles

        retlw   0

        ; 9026 cycles, including call (99.29% in wave generation)


;------------------------------------------------------------------------------
; Intensity control
;------------------------------------------------------------------------------


LoadLEDOCRs_FullIntensity:
; Transfers the LED Levels data maintained by the pattern player) to the
; Output Compare Registers to be submitted to the Pulse-Width Modulation
; stage. The OCRs may be freely manipulated to provide post-processing
; effects like intensity control. (They are not actually OCRs: They are
; just named in the manner of datasheets for MCUs with PWM peripherals.)

        movf    LED3Level_Red, W
        movwf   LED3OCR_Red
        movf    LED3Level_Green, W
        movwf   LED3OCR_Green
        movf    LED3Level_Blue, W
        movwf   LED3OCR_Blue
        movf    LED2Level_Red, W
        movwf   LED2OCR_Red
        movf    LED2Level_Green, W
        movwf   LED2OCR_Green
        movf    LED2Level_Blue, W
        movwf   LED2OCR_Blue
        movf    LED1Level_Red, W
        movwf   LED1OCR_Red
        movf    LED1Level_Green, W
        movwf   LED1OCR_Green
        movf    LED1Level_Blue, W
        movwf   LED1OCR_Blue
        retlw   0

        ; 22 cycles, including call


;------------------------------------------------------------------------------


LoadLEDOCRs_HalfIntensity:
; Transfer the LED levels data to the OCRs at roughly half intensity.
; Ideally a round half-to-even method would be used,
;
;        lsrf    {Level}, W
;        mowf    {OCR}
;        clrw
;        addwfc  {OCR)
;
; but that would require more space for little gain.

        lsrf    LED3Level_Red, W
        movwf   LED3OCR_Red
        lsrf    LED3Level_Green, W
        movwf   LED3OCR_Green
        lsrf    LED3Level_Blue, W
        movwf   LED3OCR_Blue
        lsrf    LED2Level_Red, W
        movwf   LED2OCR_Red
        lsrf    LED2Level_Green, W
        movwf   LED2OCR_Green
        lsrf    LED2Level_Blue, W
        movwf   LED2OCR_Blue
        lsrf    LED1Level_Red, W
        movwf   LED1OCR_Red
        lsrf    LED1Level_Green, W
        movwf   LED1OCR_Green
        lsrf    LED1Level_Blue, W
        movwf   LED1OCR_Blue
        retlw   0

        ; 22 cycles, including call


;------------------------------------------------------------------------------


LoadLEDOCRs_SixteenthIntensity:
; A rough up-or-down rounding is used for the 16th intensity transfer,
; which doesn't have to be high-performance. The rounding is contrived
; so that a Level value of at least 0x08 will yield a non-zero OCR value,
; suitable for conditional flickering in UI feedback cues and menus.

        movlw   LOW(LED3Level_Red)
        movwf   FSR0L
        movlw   HIGH(LED3Level_Red)
        movwf   FSR0H
        movlw   LOW(LED3OCR_Red)
        movwf   FSR1L
        movlw   HIGH(LED3OCR_Red)
        movwf   FSR1H
        movlw   9
        movwf   LoopCtr0
LoadLEDOCRs_16th_Loop:
        moviw   FSR0++
        addlw   8
        btfsc   STATUS, C
        movlw   255
        swapf   WREG, F
        andlw   0x0F
        movwi   FSR1++
        decfsz  LoopCtr0, F
        bra     LoadLEDOCRs_16th_Loop
        retlw   0

        ; 103 cycles, including call


;------------------------------------------------------------------------------


DimLEDOCRs:
; Halve the intensity of the LED Output Compare Registers by right-shifting.
; Calling this function again will result in something like quarter intensity,
; though the downward rounding used is horribly imprecise.

        lsrf    LED3OCR_Red, F
        lsrf    LED3OCR_Green, F
        lsrf    LED3OCR_Blue, F
        lsrf    LED2OCR_Red, F
        lsrf    LED2OCR_Green, F
        lsrf    LED2OCR_Blue, F
        lsrf    LED1OCR_Red, F
        lsrf    LED1OCR_Green, F
        lsrf    LED1OCR_Blue, F
        retlw   0

        ; 13 cycles, including call


;------------------------------------------------------------------------------


LoadLEDOCRs:
; Transfer the LED Levels to the OCRs according to IntensityIx.
;
; IntensityIx is zero for full intensity. Each increment of IntensityIx
; roughly halves the PWM on-time and ideally, halves the LED intensity.
; (The limited power of the battery or the DC-DC boost converter may cause
; colour shifts and a flattening of the brighter ranges. The non-linear
; nature of the human perception of brightness is a different issue.)

        movf    IntensityIx, W
        andlw   7
        brw
        goto    LoadLEDOCRs_FullIntensity
        goto    LoadLEDOCRs_HalfIntensity
        bra     _LoadLEDOCRs_IIx2
        bra     _LoadLEDOCRs_IIx3
        goto    LoadLEDOCRs_SixteenthIntensity
        goto    LoadLEDOCRs_SixteenthIntensity
        goto    LoadLEDOCRs_SixteenthIntensity
        goto    LoadLEDOCRs_SixteenthIntensity
_LoadLEDOCRs_IIx3:
        call    LoadLEDOCRs_HalfIntensity
        call    DimLEDOCRs
        goto    DimLEDOCRs
_LoadLEDOCRs_IIx2:
        call    LoadLEDOCRs_HalfIntensity
        goto    DimLEDOCRs

        ; 6 cycles, including call, plus specific LoadLEDOCR time


;------------------------------------------------------------------------------
; Pattern player - Low level
;------------------------------------------------------------------------------


LoadMappedPalette:
; Copy selected entries of a 24-bit palette to SRAM.
;
; Palette mapping allows patterns with just a few colours to use compact
; frame formats, addressing entries in a temporary palette containing only
; colours selected from a larger palette.
;
; If the map pointer is null, the destination will be a copy of the source,
; up to the indicated number of entries. The source palette and the palette
; map may each either be in BAPM format or the plain low-8-bits-per-word
; format (which also works for SRAM).
;
; In: MapWS

        ; There are three pointers involved so there will be some swapping.
        ; The destination pointer gets to stay in the FSR1 register.
        movf    MapWS + MapWS_DestPtr + 0, W
        movwf   FSR1L
        movf    MapWS + MapWS_DestPtr + 1, W
        movwf   FSR1H

        movf    MapWS + MapWS_MapPtr + 0, W
        iorwf   MapWS + MapWS_MapPtr + 1, W
        btfsc   STATUS, Z
        bra     _LoadMPal_Direct

        ; FSR0 is shared by the palette pointer and the advancing pointer
        ; in the map's colour indices.
_LoadMPal_Loop:
        ; Switch to the Map Pointer.
        movf    MapWS + MapWS_MapPtr + 0, W
        movwf   FSR0L
        movf    MapWS + MapWS_MapPtr + 1, W
        movwf   FSR0H
        movf    MapWS + MapWS_MapBAPMStatus, W
        ; Clearing the Cached flag is important since the previous
        ; read from the palette would have dirtied the cache.
        bcf     WREG, BAPM_BIT_CACHED
        movwf   BAPMRec + BAPM_Status
        ; Get the next palette colour index from the map.
        call    ReadBAPMByte
        ; Multiply by three to get the offset to the palette entry.
        movwf   Div7Rec + Div7_Dividend + 0
        clrf    Div7Rec + Div7_Dividend + 1
        lslf    Div7Rec + Div7_Dividend + 0, F
        rlf     Div7Rec + Div7_Dividend + 1, F
        addwf   Div7Rec + Div7_Dividend + 0, F
        movlw   0
        addwfc  Div7Rec + Div7_Dividend + 1, F
        ; Store the incremented map pointer.
        movf    FSR0L, W
        movwf   MapWS + MapWS_MapPtr + 0
        movf    FSR0H, W
        movwf   MapWS + MapWS_MapPtr + 1
        movf    BAPMRec + BAPM_Status, W
        movwf   MapWS + MapWS_MapBAPMStatus
        ; Fetch the source palette start address.
        movf    MapWS + MapWS_SourcePtr + 0, W
        movwf   FSR0L
        movf    MapWS + MapWS_SourcePtr + 1, W
        movwf   FSR0H
        movf    MapWS + MapWS_SourceBAPMStatus, W
        movwf   BAPMRec + BAPM_Status
        ; Add the offset (in Byte-Addressed Program Memory).
        call    AddBAPMOffset  ; (Cached flag is cleared)
        ; Read the 24-bit RGB value (3 bytes).
        call    ReadBAPMByte
        movwi   FSR1++
        call    ReadBAPMByte
        movwi   FSR1++
        call    ReadBAPMByte
        movwi   FSR1++
        decfsz  MapWS + MapWS_NumEntries, F
        bra     _LoadMPal_Loop
        bra     _LoadMPal_ClearCaches

_LoadMPal_Direct:
        movf    MapWS + MapWS_SourcePtr + 0, W
        movwf   FSR0L
        movf    MapWS + MapWS_SourcePtr + 1, W
        movwf   FSR0H
        movf    MapWS + MapWS_SourceBAPMStatus, W
        movwf   BAPMRec + BAPM_Status
_LoadMPal_DLoop:
        call    ReadBAPMByte
        movwi   FSR1++
        call    ReadBAPMByte
        movwi   FSR1++
        call    ReadBAPMByte
        movwi   FSR1++
        decfsz  MapWS + MapWS_NumEntries, F
        bra     _LoadMPal_DLoop
        ; Fall through...

_LoadMPal_ClearCaches:
        bcf     MapWS + MapWS_MapBAPMStatus, BAPM_BIT_CACHED
        bcf     MapWS + MapWS_SourceBAPMStatus, BAPM_BIT_CACHED
        retlw   0


;------------------------------------------------------------------------------


FetchAnimFrame_Common:
; All the display formats share the same code for fetching the current
; frame's address (in SRAM or Program Memory). No frame advance or timing
; occurs here: Calling this function or a specific-format fetch-frame
; function repeatedly is harmless aside from wasting time.
;
; Frame timing and advancement is handled in a separate function.

        btfsc   GAState + GAS_AnimControlFlags, ACF_BIT_FRAME_LOADED
        bra     _FAFCom_SameFrame
_FAFCom_NewFrame:
        ; 2 cycles
        movf    GAState + GAS_FrameIx, F
        btfsc   STATUS, Z
        bra     _FAFCom_LoadFrame0  ; 6 cycles
_FAFCom_LoadFrame1Plus:
        ; 5 cycles
        movf    GAState + GAS_FrameRecordStride, W
        addwf   GAState + GAS_ExpPatFramePtr + 0, F
        movlw   0
        addwfc  GAState + GAS_ExpPatFramePtr + 1, F
_FAFCom_LoadFSR0:
        ; 9 cycles
        movf    GAState + GAS_ExpPatFramePtr + 0, W
        movwf   FSR0L
        movf    GAState + GAS_ExpPatFramePtr + 1, W
        movwf   FSR0H
        ; 13 cycles
        retlw   0
_FAFCom_LoadFrame0:
        ; 6 cycles
        movf    GAState + GAS_ExpPatPtr + 0, W
        movwf   GAState + GAS_ExpPatFramePtr + 0
        movwf   FSR0L
        movf    GAState + GAS_ExpPatPtr + 1, W
        movwf   GAState + GAS_ExpPatFramePtr + 1
        movwf   FSR0H
        nop
        ; 13 cycles
        retlw   0
_FAFCom_SameFrame:
        ; 3 cycles
        bra     $ + 1
        bra     $ + 1
        bra     _FAFCom_LoadFSR0  ; 9 cycles

        ; 17 cycles, including call


;------------------------------------------------------------------------------


FetchAnimFrame_Delta_24b:
; This is the most extravagant display format: Three bytes per RGB LED, one
; byte for the frame period and one for the Frame Control Flags Byte (eleven
; bytes per frame).
;
; Depending on the FCF_BIT_DELTA flag in the FCF byte of the frame, the frame
; will either directly load new RGB values to the LED Level registers or add
; signed deltas to the LED levels registers instead. No clipping is performed.
;
; In Delta mode, the frame's delta is applied at the beginning of each frame
; unit period. The effect is that the number of deltas applied is the same
; number as the frame period in frame unit periods. For small delta sizes,
; the frame unit period, specified in the pattern header, is important for
; controlling the rate of change of intensity.
;
; Frame format (11 bytes):
;    Period:     0..254 => 1..255 frame unit periods
;    Flags:      Bit 7 = FCF_BIT_DELTA | Bits 6:0 unused
;    LED3Red:    0x00..0xFF
;    LED3Green:  0x00..0xFF
;    LED3Blue:   0x00..0xFF
;    LED2Red:    0x00..0xFF
;    LED2Green:  0x00..0xFF
;    LED2Blue:   0x00..0xFF
;    LED1Red:    0x00..0xFF
;    LED1Green:  0x00..0xFF
;    LED1Blue:   0x00..0xFF

        call    FetchAnimFrame_Common
        ; 17 cycles
        moviw   FSR0++
        movwf   GAState + GAS_FramePeriod
        incf    GAState + GAS_FramePeriod, F
        btfss   INDF0, FCF_BIT_DELTA
        bra     _FAFD24_Load  ; 23 cycles
_FAFD24_Delta:
        ; 22 cycles
        movf    GAState + GAS_FramePWMCycleCounter, W
        btfss   STATUS, Z
        bra     _FAFD24_NullDelta  ; 26 cycles
        addfsr  FSR0, 1
        ; 26 cycles
        moviw   FSR0++
        addwf   LED3Level_Red, F
        moviw   FSR0++
        addwf   LED3Level_Green, F
        moviw   FSR0++
        addwf   LED3Level_Blue, F
        moviw   FSR0++
        addwf   LED2Level_Red, F
        moviw   FSR0++
        addwf   LED2Level_Green, F
        moviw   FSR0++
        addwf   LED2Level_Blue, F
        moviw   FSR0++
        addwf   LED1Level_Red, F
        moviw   FSR0++
        addwf   LED1Level_Green, F
        moviw   FSR0++
        addwf   LED1Level_Blue, F
        ; 44 cycles
        bsf     GAState + GAS_AnimControlFlags, ACF_BIT_FRAME_LOADED
        ; 45 cycles
        retlw   0
_FAFD24_Load:
        ; 23 cycles
        bra     $ + 1
        addfsr  FSR0, 1
        ; 26 cycles
        moviw   FSR0++
        movwf   LED3Level_Red
        moviw   FSR0++
        movwf   LED3Level_Green
        moviw   FSR0++
        movwf   LED3Level_Blue
        moviw   FSR0++
        movwf   LED2Level_Red
        moviw   FSR0++
        movwf   LED2Level_Green
        moviw   FSR0++
        movwf   LED2Level_Blue
        moviw   FSR0++
        movwf   LED1Level_Red
        moviw   FSR0++
        movwf   LED1Level_Green
        moviw   FSR0++
        movwf   LED1Level_Blue
        ; 46 cycles
        bsf     GAState + GAS_AnimControlFlags, ACF_BIT_FRAME_LOADED
        ; 47 cycles
        retlw   0
_FAFD24_NullDelta:
        ; 26 cycles
        movlw   6
        decfsz  WREG, F
        bra     $ - 1
        ; 44 cycles
        bsf     GAState + GAS_AnimControlFlags, ACF_BIT_FRAME_LOADED
        ; 45 cycles
        retlw   0

        ; 49 cycles, including call


;------------------------------------------------------------------------------


INCLUDE_DF12B equ 1

  if INCLUDE_DF12B
FetchAnimFrame_Delta_12b:
; This direct colour display format uses 12-bit colour (so that each RGB LED
; can be one of 4096 possible colours). Each 4-bit colour channel entry is
; multiplied by 17 (= 0x11) as it goes into the LED Level register.
;
; Depending on the FCF_BIT_DELTA flag in the FCF byte of the frame, the frame
; will either directly load new RGB values to the LED Level registers or add
; signed deltas to the LED levels registers instead. No clipping is performed.
;
; In Delta mode, the frame's delta is applied at the beginning of each frame
; unit period. The effect is that the number of deltas applied is the same
; number as the frame period in frame unit periods. For small delta sizes,
; the frame unit period, specified in the pattern header, is important for
; controlling the rate of change of intensity.
;
; Frame format (6 bytes):
;   Period:   0..254 => 1..255 frame unit periods
;   dxxxRRRR: Delta mode bit and LED3 Red
;   GGGGBBBB: LED3 Green and Blue
;   RRRRGGGG: LED2 Red and Green
;   BBBBRRRR: LED2 Blue and LED1 Red
;   GGGGBBBB: LED1 Green and Red

        call    FetchAnimFrame_Common
        ; 17 cycles
        moviw   FSR0++
        movwf   GAState + GAS_FramePeriod
        incf    GAState + GAS_FramePeriod, F
        btfss   INDF0, FCF_BIT_DELTA
        bra     _FAFD12_Load  ; 23 cycles
_FAFD12_Delta:
        ; 22 cycles
        movf    GAState + GAS_FramePWMCycleCounter, W
        btfss   STATUS, Z
        bra     _FAFD12_NullDelta  ; 26 cycles
        ; 25 cycles
        ; [00:R3] G3:B3  R2:G2  B2:R1  G1:B1
        moviw   FSR0++
        swapf   WREG, W
        andlw   0xF0
        addwf   LED3Level_Red, W
        andlw   0xF0
        movwf   Scratch0
        swapf   Scratch0, F
        iorwf   Scratch0, W
        movwf   LED3Level_Red
        ;  00:R3 [G3:B3] R2:G2  B2:R1  G1:B1
        swapf   INDF0, W
        andlw   0xF0
        addwf   LED3Level_Blue, W
        andlw   0xF0
        movwf   Scratch0
        swapf   Scratch0, F
        iorwf   Scratch0, W
        movwf   LED3Level_Blue
        moviw   FSR0++
        andlw   0xF0
        addwf   LED3Level_Green, W
        andlw   0xF0
        movwf   Scratch0
        swapf   Scratch0, F
        iorwf   Scratch0, W
        movwf   LED3Level_Green
        ;  00:R3  G3:B3 [R2:G2] B2:R1  G1:B1
        swapf   INDF0, W
        andlw   0xF0
        addwf   LED2Level_Green, W
        andlw   0xF0
        movwf   Scratch0
        swapf   Scratch0, F
        iorwf   Scratch0, W
        movwf   LED2Level_Green
        moviw   FSR0++
        andlw   0xF0
        addwf   LED2Level_Red, W
        andlw   0xF0
        movwf   Scratch0
        swapf   Scratch0, F
        iorwf   Scratch0, W
        movwf   LED2Level_Red
        ;  00:R3  G3:B3  R2:G2 [B2:R1] G1:B1
        swapf   INDF0, W
        andlw   0xF0
        addwf   LED1Level_Red, W
        andlw   0xF0
        movwf   Scratch0
        swapf   Scratch0, F
        iorwf   Scratch0, W
        movwf   LED1Level_Red
        moviw   FSR0++
        andlw   0xF0
        addwf   LED2Level_Blue, W
        andlw   0xF0
        movwf   Scratch0
        swapf   Scratch0, F
        iorwf   Scratch0, W
        movwf   LED2Level_Blue
        ;  00:R3  G3:B3  R2:G2  B2:R1 [G1:B1]
        swapf   INDF0, W
        andlw   0xF0
        addwf   LED1Level_Blue, W
        andlw   0xF0
        movwf   Scratch0
        swapf   Scratch0, F
        iorwf   Scratch0, W
        movwf   LED1Level_Blue
        moviw   FSR0++
        andlw   0xF0
        addwf   LED1Level_Green, W
        andlw   0xF0
        movwf   Scratch0
        swapf   Scratch0, F
        iorwf   Scratch0, W
        movwf   LED1Level_Green
        ; 98 cycles
        bsf     GAState + GAS_AnimControlFlags, ACF_BIT_FRAME_LOADED
        ; 99 cycles
        retlw   0
_FAFD12_Load:
        ; 23 cycles
        movlw   7
        decfsz  WREG, F
        bra     $ - 1
        ; 44 cycles
        moviw   FSR0++
        andlw   0x0F
        movwf   Scratch0
        swapf   Scratch0, F
        iorwf   Scratch0, W
        movwf   LED3Level_Red
        movf    INDF0, W
        andlw   0xF0
        movwf   Scratch0
        swapf   Scratch0, F
        iorwf   Scratch0, W
        movwf   LED3Level_Green
        moviw   FSR0++
        andlw   0x0F
        movwf   Scratch0
        swapf   Scratch0, F
        iorwf   Scratch0, W
        movwf   LED3Level_Blue
        movf    INDF0, W
        andlw   0xF0
        movwf   Scratch0
        swapf   Scratch0, F
        iorwf   Scratch0, W
        movwf   LED2Level_Red
        moviw   FSR0++
        andlw   0x0F
        movwf   Scratch0
        swapf   Scratch0, F
        iorwf   Scratch0, W
        movwf   LED2Level_Green
        movf    INDF0, W
        andlw   0xF0
        movwf   Scratch0
        swapf   Scratch0, F
        iorwf   Scratch0, W
        movwf   LED2Level_Blue
        moviw   FSR0++
        andlw   0x0F
        movwf   Scratch0
        swapf   Scratch0, F
        iorwf   Scratch0, W
        movwf   LED1Level_Red
        movf    INDF0, W
        andlw   0xF0
        movwf   Scratch0
        swapf   Scratch0, F
        iorwf   Scratch0, W
        movwf   LED1Level_Green
        moviw   FSR0++
        andlw   0x0F
        movwf   Scratch0
        swapf   Scratch0, F
        iorwf   Scratch0, W
        movwf   LED1Level_Blue
        ; 98 cycles
        bsf     GAState + GAS_AnimControlFlags, ACF_BIT_FRAME_LOADED
        ; 99 cycles
        retlw   0
_FAFD12_NullDelta:
        ; 26 cycles
        movlw   24
        decfsz  WREG, F
        bra     $ - 1
        ; 98 cycles
        bsf     GAState + GAS_AnimControlFlags, ACF_BIT_FRAME_LOADED
        ; 99 cycles
        retlw   0

        ; 103 cycles, including call
  endif


;------------------------------------------------------------------------------


FetchAnimFrame_Paletted_256c:
; This moderately compact indexed colour (paletted) display format uses four
; bytes per frame, including a full byte for the frame period.
;
; Format (4 bytes):
;   Period:   0..254 => 1..255 frame unit periods
;   LED3 colour index
;   LED2 colour index
;   LED1 colour index

        call    FetchAnimFrame_Common
        ; 17 cycles
        moviw   FSR0++
        movwf   GAState + GAS_FramePeriod
        incf    GAState + GAS_FramePeriod, F
        moviw   FSR0++
        movwf   LED3Level_Red
        moviw   FSR0++
        movwf   LED2Level_Red
        moviw   FSR0++
        movwf   LED1Level_Red
        ; 26 cycles
        bra     FetchAnimFrame_Paletted_Common

        ; 82 cycles, including call


;------------------------------------------------------------------------------


FetchAnimFrame_Paletted_16c:
; This 4-bit (16 colour) indexed colour (paletted) display format uses two
; bytes per frame. The frame period can only be in the range 1..16 (stored
; in the frame as 0..15).
;
; Format (2 bytes):
;   pppp3333: Period 0..15 => 1..16 frame unit periods, LED3 colour index
;   22221111: LED2 colour index and LED1 colour index

        call    FetchAnimFrame_Common
        ; 17 cycles
        moviw   FSR0++
        movwf   LED3Level_Red
        swapf   WREG, F
        movwf   GAState + GAS_FramePeriod
        moviw   FSR0++
        movwf   LED1Level_Red
        swapf   WREG, F
        movwf   LED2Level_Red
        movlw   0x0F
        andwf   LED3Level_Red, F
        andwf   LED2Level_Red, F
        andwf   LED1Level_Red, F
        andwf   GAState + GAS_FramePeriod, F
        incf    GAState + GAS_FramePeriod, F
        ; 31 cycles
        bra     FetchAnimFrame_Paletted_Common

        ; 87 cycles, including call


;------------------------------------------------------------------------------


FetchAnimFrame_Paletted_4c:
; This 2-bit (4 colour) indexed colour (paletted) display format uses only
; one byte per frame. The frame period can only be in the range 1..4 (stored
; in the frame as 0..3).
;
; Format (2 bytes):
;   pp332211: Period 0..3 => 1..4 frame unit periods, LED3 colour index

        call    FetchAnimFrame_Common
        ; 17 cycles
        moviw   FSR0++
        movwf   LED1Level_Red
        swapf   WREG, F
        movwf   LED3Level_Red
        lsrf    WREG, F
        lsrf    WREG, F
        movwf   GAState + GAS_FramePeriod
        swapf   WREG, F
        movwf   LED2Level_Red
        movlw   0x03
        andwf   LED3Level_Red, F
        andwf   LED2Level_Red, F
        andwf   LED1Level_Red, F
        andwf   GAState + GAS_FramePeriod, F
        incf    GAState + GAS_FramePeriod, F
        ; 32 cycles
        bra     FetchAnimFrame_Paletted_Common

        ; 88 cycles, including call


;------------------------------------------------------------------------------


FetchAnimFrame_Invalid:
; This is called in the case of an unrecognised display format that wasn't
; detected and reported properly.

        ; Just make a mess!
        incf    LED3Level_Green, F
        decf    LED2Level_Green, F
        incf    LED1Level_Green, F
        retlw   0


;------------------------------------------------------------------------------


FetchAnimFrame_Paletted_Common:
; Use the colour indices stuffed into the red channels to fetch 24-bit colours.

        ; LED3
        clrf    FSR1H
        lslf    LED3Level_Red, W
        rlf     FSR1H, F
        addwf   LED3Level_Red, W
        movwf   FSR1L
        movlw   0
        addwfc  FSR1H, F
        movf    GAState + GAS_PalettePtr + 0, W
        addwf   FSR1L, F
        movf    GAState + GAS_PalettePtr + 1, W
        addwfc  FSR1H, F
        moviw   FSR1++
        movwf   LED3Level_Red
        moviw   FSR1++
        movwf   LED3Level_Green
        moviw   FSR1++
        movwf   LED3Level_Blue
        ; 17 cycles
        ; LED2
        clrf    FSR1H
        lslf    LED2Level_Red, W
        rlf     FSR1H, F
        addwf   LED2Level_Red, W
        movwf   FSR1L
        movlw   0
        addwfc  FSR1H, F
        movf    GAState + GAS_PalettePtr + 0, W
        addwf   FSR1L, F
        movf    GAState + GAS_PalettePtr + 1, W
        addwfc  FSR1H, F
        moviw   FSR1++
        movwf   LED2Level_Red
        moviw   FSR1++
        movwf   LED2Level_Green
        moviw   FSR1++
        movwf   LED2Level_Blue
        ; 34 cycles
        ; LED1
        clrf    FSR1H
        lslf    LED1Level_Red, W
        rlf     FSR1H, F
        addwf   LED1Level_Red, W
        movwf   FSR1L
        movlw   0
        addwfc  FSR1H, F
        movf    GAState + GAS_PalettePtr + 0, W
        addwf   FSR1L, F
        movf    GAState + GAS_PalettePtr + 1, W
        addwfc  FSR1H, F
        moviw   FSR1++
        movwf   LED1Level_Red
        moviw   FSR1++
        movwf   LED1Level_Green
        moviw   FSR1++
        movwf   LED1Level_Blue
        ; 51 cycles
        bsf     GAState + GAS_AnimControlFlags, ACF_BIT_FRAME_LOADED
        ; 52 cycles
        retlw   0

        ; 56 cycles, including jump


;------------------------------------------------------------------------------


ReadyPatternForLoading:
; Partially initialise the General Animation State for a pattern,
;
; The partial initialisation allows the number of bytes of SRAM required
; by the palette and frame data to be determined and checked before any
; unpacking is performed by FinishPatternLoading.
;
; In: FSR0 = Pattern in Program Memory
;     GAState.GAS_ExpPatPtr = Destination address of expanded pattern
; Out: GAState partially initialised
;      Mult_u8_Product = Bytes required by frames and palette
;      FSR0 preserved.

        ; Set the default address of the copied or mapped palette to
        ; be same as the start address of the list of expanded frames.
        ; The palette will be bumped to a higher address if the frames
        ; have to be unpacked to SRAM.
        movf    GAState + GAS_ExpPatPtr + 0, W
        movwf    GAState + GAS_PalettePtr + 0
        movf    GAState + GAS_ExpPatPtr + 1, W
        movwf   GAState + GAS_PalettePtr + 1
        ; Swizzle the pattern header argument, repurposing a GAS field.
        movf    FSR0L, W
        movwf   GAState + GAS_ExpPatFramePtr + 0
        movf    FSR0H, W
        movwf   GAState + GAS_ExpPatFramePtr + 1
        ; Get the display format and the small stuff.
        call    ReadPMHighBits  ; Display format
        andlw   ACF_FORMAT_MASK
        movwf   GAState + GAS_AnimControlFlags
        call    _RP4L_LookUpStride
        movwf   GAState + GAS_FrameRecordStride
        moviw   FSR0++  ; Number of colours
        movwf   GAState + GAS_FrameIx  ; Repurposed
        call    ReadPMHighBits  ; Storage format flags
        movwf   GAState + GAS_FrameTCounter  ; Repurposed
        moviw   FSR0++
        movwf   GAState + GAS_FramePeriodUnit
        moviw   FSR0++
        movwf   GAState + GAS_NumFrames
        ; Repurpose the frame period field to store flags to indicate
        ; whether frames need to be unpacked into SRAM and whether a
        ; palette is to be unpacked or mapped into SRAM.
        clrf    GAState + GAS_FramePeriod
        ; Calculate the memory required for animation frames.
        ; (No SRAM will be required if the frames in Program
        ; Memory are directly usable.)
        clrf    Mult_u8_ProductL
        clrf    Mult_u8_ProductH
        btfss   GAState + GAS_FrameTCounter, PAT_BAPM_BIT_FRAMES  ; Repurposed
        bra     _RP4L_HavePalettePtrReady
        ; Frames will need to be copied to SRAM.
        bsf     GAState + GAS_FramePeriod, PAT_BAPM_BIT_FRAMES  ; Repurposed
        movf    GAState + GAS_NumFrames, W
        btfss   STATUS, Z
        bra     _RP4L_Sub256Frames
_RP4L_256Frames:
        movf    GAState + GAS_FrameRecordStride, W
        movwf   Mult_u8_ProductH
        bra     _RP4L_HaveFrameListSize
_RP4L_Sub256Frames:
        movwf   Mult_u8_Multiplier
        movf    GAState + GAS_FrameRecordStride, W
        call    Mult_u8
_RP4L_HaveFrameListSize:
        movf    Mult_u8_ProductL, W
        addwf   GAState + GAS_PalettePtr + 0, F
        movf    Mult_u8_ProductH, W
        addwfc  GAState + GAS_PalettePtr + 1, F
_RP4L_HavePalettePtrReady:
        ; Calculate the memory required for the palette.
        ; (SRAM will be required only for a display mode
        ; which uses a palette and only if palette mapping
        ; is required or the palette is stored in a format
        ; which needs unpacking to SRAM.)
        movf    GAState + GAS_AnimControlFlags, W
        sublw   2 - 1  ; Warning: Performs W <- argument - W.
        btfsc   STATUS, C
        bra     _RP4L_HavePatSize  ; Display format 0 (24-bit) or 1 (12-bit)
        ; Indexed colour mode (paletted)
        addfsr  FSR0, 1  ; Advance to the map pointer
        call    ReadPMHighBits
        iorwf   INDF0, W
        addfsr  FSR0, -1  ; Back to the palette pointer
        btfss   STATUS, Z
        bra     _RP4L_MappingReq
        btfss   GAState + GAS_FrameTCounter, PAT_BAPM_BIT_PALETTE
        bra     _RP4L_HavePatSize
_RP4L_PalReq:
        bsf     GAState + GAS_FramePeriod, PAT_BAPM_BIT_PALETTE  ; Repurposed
        clrf    Scratch0
        movf    GAState + GAS_FrameIx, W
        btfss   STATUS, Z
        bra     _RP4L_Sub256PalEntries
_RP4L_256PalEntries:
        movlw   3
        addwf   Mult_u8_ProductH, F
        bra     _RP4L_HavePatSize
_RP4L_MappingReq:
        bsf     GAState + GAS_FramePeriod, PAT_BAPM_BIT_MAP  ; Repurposed
        bra     _RP4L_PalReq
_RP4L_Sub256PalEntries:
        lslf    GAState + GAS_FrameIx, W
        rlf     Scratch0, F
        addwf   Mult_u8_ProductL, F
        movf    Scratch0, W
        addwfc  Mult_u8_ProductH, F
        movf    GAState + GAS_FrameIx, W
        addwf   Mult_u8_ProductL, F
        movlw   0
        addwfc  Mult_u8_ProductH, F
_RP4L_HavePatSize:
        addfsr  FSR0, -3
        retlw   0
_RP4L_LookUpStride:
        andlw   ACF_FORMAT_MASK
        ;sublw   8 - 1  ; Warning: Performs W <- argument - W.
        ;btfss   STATUS, C
        ;bra     _RP4L_LookUpStride_Bad
        ;sublw   8 - 1 ; W' = x-(x-W) = W
        brw
        retlw   11
        retlw   6
        retlw   4
        retlw   2
        retlw   1
        retlw   0
        retlw   0
_RP4L_LookUpStride_Bad:
        retlw   0


;------------------------------------------------------------------------------


FinishPatternLoading:
; Finish loading the pattern analysed by ReadyPatternForLoading.
;
; In: GAState partially initialised with ReadyPatternForLoading
; Out: GAState fully initialised
;      Frame and palette data copied to SRAM
;      W = Error code:
;        0: Success
;        1: SYSPAT_ERR_INVALID_FORMAT (Invalid display format)
;        2: SYSPAT_ERR_NULL_PALETTE (for a format requiring a palette)

        movf    GAState + GAS_FrameRecordStride, W
        btfsc   STATUS, Z
        bra     _FPL_InvalidDisplayFmt

        movf    GAState + GAS_ExpPatFramePtr + 0, W
        movwf   FSR0L
        movf    GAState + GAS_ExpPatFramePtr + 1, W
        movwf   FSR0H

        ; Skip to the Palette, Map and Frames pointers
        addfsr  FSR0, 3

        ; See if the display format is paletted.
        movf    GAState + GAS_AnimControlFlags, W
        sublw   2 - 1  ; Warning: Performs W <- argument - W.
        btfsc   STATUS, C
        bra     _FPL_PalDone  ; Display format 0 (24-bit) or 1 (12-bit)
        ; Indexed colour mode (paletted)
        call    ReadPMHighBits
        movwf   Scratch0
        iorwf   INDF0, W
        btfsc   STATUS, Z
        bra     _FPL_BadPalPtr
        btfsc   GAState + GAS_FramePeriod, PAT_BAPM_BIT_PALETTE  ; Repurposed
        bra     _FPL_CopyOrMap
_FPL_DirectPalPtr:
        movf    INDF0, W
        movwf   GAState + GAS_PalettePtr + 0
        movf    Scratch0, W
        iorlw   0x80  ; Program Memory
        movwf   GAState + GAS_PalettePtr + 1
        bra     _FPL_PalDone
_FPL_CopyOrMap:
        ; Palette source
        clrf    MapWS + MapWS_SourceBAPMStatus
        moviw   FSR0++
        movwf   MapWS + MapWS_SourcePtr + 0
        movf    Scratch0, W
        iorlw   0x80  ; Program Memory
        movwf   MapWS + MapWS_SourcePtr + 1
        btfss   GAState + GAS_FrameTCounter, PAT_BAPM_BIT_PALETTE
        bsf     MapWS + MapWS_SourceBAPMStatus, BAPM_BIT_LOW_BYTES_ONLY
        ; Palette mapping
        clrf    MapWS + MapWS_MapBAPMStatus
        call    ReadPMHighBits
        movwf   MapWS + MapWS_MapPtr + 1
        movf    INDF0, W
        movwf   MapWS + MapWS_MapPtr + 0
        iorwf   MapWS + MapWS_MapPtr + 1, W
        btfss   STATUS, Z
        bsf     MapWS + MapWS_MapPtr + 1, 7  ; Program Memory
        btfss   GAState + GAS_FrameTCounter, PAT_BAPM_BIT_MAP
        bsf     MapWS + MapWS_MapBAPMStatus, BAPM_BIT_LOW_BYTES_ONLY
        ; Destination
        movf    GAState + GAS_PalettePtr + 0, W
        movwf   MapWS + MapWS_DestPtr + 0
        movf    GAState + GAS_PalettePtr + 1, W
        movwf   MapWS + MapWS_DestPtr + 1
        ; Number of colours (0 means 256)
        movf    GAState + GAS_FrameIx, W
        movwf   MapWS + MapWS_NumEntries
        ; Build or copy the palette.
        call    LoadMappedPalette
        ; Restore FSR0 to the Palette, Map and Frames pointers
        movf    GAState + GAS_ExpPatFramePtr + 0, W
        movwf   FSR0L
        movf    GAState + GAS_ExpPatFramePtr + 1, W
        movwf   FSR0H
        addfsr  FSR0, 3
_FPL_PalDone:

        ; Advance to the Frames source pointer
        addfsr  FSR0, 2
        call    ReadPMHighBits
        movwf   FSR1H
        moviw   FSR0++
        movwf   FSR1L
        iorwf   FSR1H, W
        btfss   STATUS, Z
        bra     _FPL_HaveFrPtr
        ; A null frames pointer means that the frames immediately
        ; follow the pattern header in Program Memory.
        movf    FSR0L, W
        movwf   FSR1L
        movf    FSR0H, W
        movwf   FSR1H
_FPL_HaveFrPtr:
        bsf     FSR1H, 7  ; Program Memory
        btfsc   GAState + GAS_FrameTCounter, PAT_BAPM_BIT_FRAMES
        bra     _FPL_XfrFrames
        ; The supplied pointer to frames is directly usable.
        movf    FSR1L, W
        movwf   GAState + GAS_ExpPatPtr + 0
        movf    FSR1H, W
        movwf   GAState + GAS_ExpPatPtr + 1
        bra     _FPL_FramesDone
_FPL_XfrFrames:
        movf    FSR1L, W
        movwf   FSR0L
        movf    FSR1H, W
        movwf   FSR0H
        movf    GAState + GAS_ExpPatPtr + 0, W
        movwf   FSR1L
        movf    GAState + GAS_ExpPatPtr + 1, W
        movwf   FSR1H
        clrf    BAPMRec + BAPM_Status
        movf    GAState + GAS_NumFrames, W
        movwf   LoopCtr1
_FPL_XfrFrLoop:
        movf    GAState + GAS_FrameRecordStride, W
        movwf   LoopCtr0
_FPL_XfrFrRowLoop:
        call    ReadBAPMByte
        movwi   FSR1++
        decfsz  LoopCtr0, F
        bra     _FPL_XfrFrRowLoop
        decfsz  LoopCtr1, F
        bra     _FPL_XfrFrLoop
_FPL_FramesDone:

_FPL_FinishFields:
        ; Finish the initialisation of the General Animation State.
        clrf    GAState + GAS_FrameIx
        clrf    GAState + GAS_FrameTCounter
        clrf    GAState + GAS_FramePWMCycleCounter
        ; Not strictly necessary, but makes simulation easy to follow.
        clrf    GAState + GAS_FramePeriod
        movf    GAState + GAS_ExpPatPtr + 0, W
        movwf   GAState + GAS_ExpPatFramePtr + 0
        movf    GAState + GAS_ExpPatPtr + 1, W
        movwf   GAState + GAS_ExpPatFramePtr + 1
        retlw   0

_FPL_InvalidDisplayFmt:
        retlw   SYSPAT_ERR_INVALID_FORMAT

_FPL_BadPalPtr:
        retlw   SYSPAT_ERR_NULL_PALETTE


;------------------------------------------------------------------------------
; Pattern player - High level
;------------------------------------------------------------------------------


GetEnumeratedSysPatAddr:
; Fetch a system pattern's address via an 8-bit index into a table.
;
; In: W = Index into PM_SysPatTable
; Out: FSR0 = Pattern header address
;      W preserved

        movwf   Scratch1
        movlw   LOW(PM_SysPatTable)
        movwf   FSR0L
        movlw   HIGH(PM_SysPatTable)
        movwf   FSR0H
        movf    Scratch1, W
        call    FetchPMWordFromTable
        bsf     FSR0H, 7
        movf    Scratch1, W
        return


;------------------------------------------------------------------------------


LoadPattern:
; Load a pattern from the pattern header at FSR0.
;
; If the loading fails, an error pattern will be loaded and an error code
; will be returned.
;
; In: FSR0 = Pattern header in Program Memory
; Out: GAState initialised
;      W = Error code:
;        0: Success
;        1: SYSPAT_ERR_INVALID_FORMAT (Invalid display format)
;        2: SYSPAT_ERR_NULL_PALETTE (for a format requiring a palette)
;        3: SYSPAT_ERR_PAT_TOO_LARGE (Pattern too large to unpack to SRAM)

        movlw   LOW(DEFAULT_PATTERN_ADDR)
        movwf   GAState + GAS_ExpPatPtr + 0
        movlw   HIGH(DEFAULT_PATTERN_ADDR)
        movwf   GAState + GAS_ExpPatPtr + 1
        call    ReadyPatternForLoading
        movlw   LOW(MAX_PATTERN_SIZE + 1)
        subwf   Mult_u8_ProductL, F
        movlw   HIGH(MAX_PATTERN_SIZE + 1)
        subwfb  Mult_u8_ProductH, F
        movlw   SYSPAT_ERR_PATTERN_TOO_LARGE
        btfss   STATUS, C
        call    FinishPatternLoading
        movf    WREG, F
        btfsc   STATUS, Z
        return
        ; An error occurred!
        andlw   0x03
        call    GetEnumeratedSysPatAddr
        movwf   Scratch1
        call    ReadyPatternForLoading
        call    FinishPatternLoading
        movf    Scratch1, W
        return


;------------------------------------------------------------------------------


JumpToAnimFrame:
; The pattern player is not really designed for random access. The simple
; submenus which use and abuse the pattern player require an arbitrary
; start frame, representing the currently selected option.
;
; In: W = Frame index

        movwf   GAState + GAS_FrameIx
        movf    GAState + GAS_NumFrames, W
        subwf   GAState + GAS_FrameIx, W
        btfsc   STATUS, C
        clrf    GAState + GAS_FrameIx
        bcf     GAState + GAS_AnimControlFlags, ACF_BIT_FRAME_LOADED
        movf    GAState + GAS_FrameIx, W
        btfsc   STATUS, Z
        bra     _JTAFr_First
        decf    WREG, F
        movwf   Mult_u8_Multiplier
        movf    GAState + GAS_FrameRecordStride, W
        call    Mult_u8
        movf    Mult_u8_ProductL, W
        addwf   GAState + GAS_ExpPatPtr + 0, W
        movwf   GAState + GAS_ExpPatFramePtr + 0
        movf    Mult_u8_ProductH, W
        addwfc  GAState + GAS_ExpPatPtr + 1, W
        movwf   GAState + GAS_ExpPatFramePtr + 1
        retlw   0
_JTAFr_First:
        retlw   0


;------------------------------------------------------------------------------


FetchAnimFrame:
; Fetch the current animation frame, loading the LED Level registers according
; to the format specified by the pattern.
;
; In: Format index in GAState.GAS_AnimControlFlags

        movf    GAState + GAS_AnimControlFlags, W
        andlw   ACF_FORMAT_MASK
        brw
        goto    FetchAnimFrame_Delta_24b
      if INCLUDE_DF12B
        goto    FetchAnimFrame_Delta_12b
      else
        goto    FetchAnimFrame_Invalid
      endif
        goto    FetchAnimFrame_Paletted_256c
        goto    FetchAnimFrame_Paletted_16c
        goto    FetchAnimFrame_Paletted_4c
        goto    FetchAnimFrame_Invalid
        goto    FetchAnimFrame_Invalid
        goto    FetchAnimFrame_Invalid

        ; 6 cycles, including call, plus the specific FAF time


;------------------------------------------------------------------------------


AdvanceAnimCounters:
; Update the timing counters and frame index of the standard pattern player.
;
; Each "frame" in the pattern player has is own period in multiples of the
; unit frame period in PWM cycles specified in the pattern's header. The
; value of the stored frame period byte (or nibble or crumb) is one less
; than the number of PWM cycles it spans.

        incf    GAState + GAS_FramePWMCycleCounter, F
        movf    GAState + GAS_FramePeriodUnit, W
        subwf   GAState + GAS_FramePWMCycleCounter, W
        movlw   0
        btfsc   STATUS, C
        movwf   GAState + GAS_FramePWMCycleCounter
        btfsc   STATUS, C
        incf    GAState + GAS_FrameTCounter, F

        movf    GAState + GAS_FramePeriod, W
        subwf   GAState + GAS_FrameTCounter, W
        ; C = GAS_FrameTCounter >= GAS_FramePeriod (Borrow Flag convention)
        btfss   STATUS, C
        bra     _AAC_SameFrame  ; 13 cycles
        ; 12 cycles
        clrf    GAState + GAS_FrameTCounter
        incf    GAState + GAS_FrameIx, F
        bcf     GAState + GAS_AnimControlFlags, ACF_BIT_FRAME_LOADED
        movf    GAState + GAS_NumFrames, W
        subwf   GAState + GAS_FrameIx, W
        btfsc   STATUS, C
        bsf     GAState + GAS_AnimControlFlags, ACF_BIT_LOOP_COMPLETED
        btfsc   STATUS, C
        clrf    GAState + GAS_FrameIx
        ; 21 cycles
        retlw   0
_AAC_SameFrame:
        ; 13 cycles
        movlw   2
        decfsz  WREG, F
        bra     $ - 1
        bra     $ + 1
        ; 21 cycles
        retlw   0

        ; 25 cycles, including call


;------------------------------------------------------------------------------
; Bank and Pattern-specific
;------------------------------------------------------------------------------


GetPatternFromBank_Tramp0:
        pagesel GetPatternFromBank_Page1
        call    GetPatternFromBank_Page1
        movlp   0
        return


;------------------------------------------------------------------------------


PatIsModified:
; Indicate whether the currently selected pattern has been "modified".
; The pattern records are not truly modified. Rather, a modified pattern
; is displayed in place of the pattern with fav/bank/pattern indicies
; matching the one that was last loaded into the editor (whether or not
; any changes were made).
;
; Out: W = 0 if the pattern (possibly a Favourite) is definitely unmodified.
;      W = 1 if the pattern has been in the editor.

        movf    ModFavouriteIx, W
        xorwf   FavouriteIx, W
        movwf   Scratch0
        movf    ModBankIx, W
        xorwf   BankIx, W
        iorwf   Scratch0, F
        movf    ModPatternIx, W
        xorwf   PatternIx, W
        iorwf   Scratch0, F
        movlw   0
        btfsc   STATUS, Z
        movlw   1
        return


;------------------------------------------------------------------------------


UpdateEffectivePatMod:
; The effective tempo and ramp indices modifies the palette of the pattern
; marked as being up for modification. A pattern that has been subject to
; Pattern Adjustment Mode will have the User Pattern Modification applied
; while it is displayed.
;
; In: FavouriteIx, ModeFlags
;     UserPatMod, FavRecPatMod, DefaultPatMod
; Out: EffectivePatMod

        movlw   LOW(EffectivePatMod)
        movwf   FSR1L
        movlw   HIGH(EffectivePatMod)
        movwf   FSR1H
        movlw   LOW(DefaultPatMod)
        movwf   FSR0L
        movlw   HIGH(DefaultPatMod)
        movwf   FSR0H
        call    PatIsModified
        btfss   WREG, 0
        bra     _UpdateERIxs_Unmodified
_UpdateERIxs_Modified:
        addfsr  FSR0, UserPatMod - DefaultPatMod
        bra     _UpdateERIxs_Copy
_UpdateERIxs_Unmodified:
        incfsz  FavouriteIx, W
        addfsr  FSR0, FavRecPatMod - DefaultPatMod
_UpdateERIxs_Copy:
        ; TempoIx can be modified even if there are no user-settable ramps.
        moviw   FSR0++
        movwi   FSR1++
        btfsc   ModeFlags, MF_BIT_EXTERNALRAMPS
        bra     _UpdateERIxs_CopyRamps
        movlw   LOW(DefaultPatMod + PatMod_RampIxA)
        movwf   FSR0L
        movlw   HIGH(DefaultPatMod + PatMod_RampIxA)
        movwf   FSR0H
_UpdateERIxs_CopyRamps:
        movlw   PatModSize - 1
        movwf   LoopCtr0
_UpdateERIxs_CopyRampsLoop:
        moviw   FSR0++
        movwi   FSR1++
        decfsz  LoopCtr0, F
        bra     _UpdateERIxs_CopyRampsLoop
        retlw   0


;------------------------------------------------------------------------------


ModifyPattern_Tramp0:
        pagesel ModifyPattern_Page1
        call    ModifyPattern_Page1
        movlp   0
        return


;------------------------------------------------------------------------------


ForbidAllBlackPatMod_Tramp0:
        pagesel ForbidAllBlackPatMod_Page1
        call    ForbidAllBlackPatMod_Page1
        movlp   0
        return


;------------------------------------------------------------------------------
; Last Used Pattern
;------------------------------------------------------------------------------


SetEWLAccessForLUP:
; Prepare the EWL access object to access the Last Used Pattern in EEPROM.
;
; Every time the Mode or Pattern button is pressed (and updating of
; the Last Used Pattern memory is enabled), the newly selected Bank
; and Pattern indices are saved to EEPROM. EEPROM memory cells are
; typically good for 100,000 writes. To avoid burning a hole in EEPROM
; memory with frequently updated data, both the data and its location
; is spread over many bytes in EEPROM.
;
; Out: FSR0 = EWLAccess
;      EWLAccess configured

        movlw   LOW(EWLAccess)
        movwf   FSR0L
        movlw   HIGH(EWLAccess)
        movwf   FSR0H
        movf    LUPEWLSlotIx, W
        movwi   EWLAccess_CurrentIndex[FSR0]
        movlw   LUPEWL_NUM_SLOTS
        movwi   EWLAccess_Length[FSR0]
        movlw   EEPROM_LUPStatusRing
        movwi   EWLAccess_StatusRing[FSR0]
        movlw   EEPROM_LUPDataRing
        movwi   EWLAccess_DataRing[FSR0]
        movlw   LUPRecSize
        movwi   EWLAccess_DataSlotSize[FSR0]
        retlw   0


;------------------------------------------------------------------------------


GetLastUsedPattern:
; Set BankIx, PatternIx and FavoriteIx to the LUP recorded in EEPROM.

        call    SetEWLAccessForLUP
        call    FindCurrentEWLIndex
        movwf   LUPEWLSlotIx
        call    GetEWLDataAddress
        movf    EWLFind + EWLFind_DataSlotAddr, W
        movwf   EEPROMPtr
        call    ReadEEPROMByte
        movwf   BankIx
        call    ReadEEPROMByte
        movwf   PatternIx
        ; Interpret 255, 255 as unprogrammed EEPROM to be read as 0, 0.
        ; (Single 255s are to be read literally.)
        comf    BankIx, W
        movwf   Scratch0
        comf    PatternIx, W
        iorwf   Scratch0, W
        btfss   STATUS, Z
        bra     _GetLUP_SkipUnpgmd
_GetLUP_Reset:
        clrf    BankIx
        clrf    PatternIx
_GetLUP_SkipUnpgmd:
        clrf    FavouriteIx
        comf    FavouriteIx, F
        incfsz  BankIx, W
        retlw   0
        ; This LUP is a reference to a Favourites slot.
        movf    PatternIx, W
        movwf   FavouriteIx
        clrf    BankIx
        clrf    PatternIx
        call    ClampOrInvalidateFavIx
        incfsz  FavouriteIx, W
        goto    GetFavourite
        retlw   0


;------------------------------------------------------------------------------


SaveLastUsedPattern:
; Save the Last Used Pattern via the EEPROM Wear-Levelling system.
;
; If is a pattern is playing from the Favourites list, the LUP record will
; refer to the entry in the Favourites list. If the LUP-updating feature is
; disabled by the user, it is possible for an invalid entry to be referenced
; when the list is modified. The effect will be harmless, though.

        call    SetEWLAccessForLUP
        movf    LUPEWLSlotIx, W
        incf    WREG, F
        call    GetEWLDataAddress
        movf    EWLFind + EWLFind_DataSlotAddr, W
        movwf   EEPROMPtr
        incfsz  FavouriteIx, W
        bra     _SaveLUP_Fav
_SaveLUP_Std:
        movf    BankIx, W
        call    WriteEEPROMByte
        movf    PatternIx, W
        call    WriteEEPROMByte
        call    AdvanceEWLStatusRing
        movwf   LUPEWLSlotIx
        retlw   0
_SaveLUP_Fav:
        movlw   255
        call    WriteEEPROMByte
        movf    FavouriteIx, W
        call    WriteEEPROMByte
        call    AdvanceEWLStatusRing
        movwf   LUPEWLSlotIx
        retlw   0


;------------------------------------------------------------------------------
; Favourites
;------------------------------------------------------------------------------


SetEWLAccessForFav:
; Prepare the EWL access object to access Favorites in EEPROM.
;
; The EEPROM Wear-Levelling feature primarily used for storing the Last Used
; Pattern is also used for storing Favourite patterns (in a separate set of
; Status and Data rings).
;
; Out: FSR0 = EWLAccess
;      EWLAccess configured

        movlw   HIGH(EWLAccess)
        movwf   FSR0H
        movlw   LOW(EWLAccess)
        movwf   FSR0L
        movf    FavHeadEWLSlotIx, W
        movwi   EWLAccess_CurrentIndex[FSR0]
        movlw   FAVOURITES_NUM_SLOTS
        movwi   EWLAccess_Length[FSR0]
        movlw   EEPROM_FavStatusRing
        movwi   EWLAccess_StatusRing[FSR0]
        movlw   EEPROM_FavDataRing
        movwi   EWLAccess_DataRing[FSR0]
        movlw   FavRecSize
        movwi   EWLAccess_DataSlotSize[FSR0]
        retlw   0


;------------------------------------------------------------------------------


GetFavMetrics:
; Find the most recently saved Favourite Pattern and find how many favourite
; patterns there are.
;
; In this application, the most recently saved Favourite, the head, exists
; in the EWL rings at slot index s = FavHeadEWLSlotIx and is assigned the
; Favourites Index of zero. Increasing Favourites indices refer to decreasing
; EWL slot indices (minding wrap-around).
;
; A Favourites entry that is all bit ones (0xFF, 0xFF) is used in a ring that
; is less than full to mark the end of the Favourites list.
;
; Out: NumFavs
;      FavHeadEWLSlotIx

        call    SetEWLAccessForFav
        call    FindCurrentEWLIndex
        movwf   FavHeadEWLSlotIx
        movwf   Scratch1
        call    GetEWLDataAddress
        moviw   EWLAccess_Length[FSR0]
        movwf   LoopCtr0
        clrf    NumFavs
        movf    EWLFind + EWLFind_DataSlotAddr, W
        movwf   EEPROMPtr
_GetFavMets_Loop:
        call    ReadEEPROMByte
        movwf   Scratch0
        call    _GetFavMets_AndByte
        call    _GetFavMets_And2Bytes  ; One step forward...
        comf    Scratch0, F
        btfsc   STATUS, Z
        bra     _GetFavMets_HaveNumFavs
        incf    NumFavs, F
        movlw   2 * FavRecSize  ; ...Two steps backward.
        subwf   EEPROMPtr, F
        movlw   1
        subwf   Scratch1, F
        btfsc   STATUS, C
        bra     _GetFavMets_SkipWrap
        movlw   FAVOURITES_NUM_SLOTS * FavRecSize
        addwf   EEPROMPtr, F
        movlw   FAVOURITES_NUM_SLOTS
        addwf   Scratch1, F
_GetFavMets_SkipWrap:
        decfsz  LoopCtr0, F
        bra     _GetFavMets_Loop
_GetFavMets_HaveNumFavs:
        retlw   0
_GetFavMets_And2Bytes:
        call    _GetFavMets_AndByte
        ; Fall trhough...
_GetFavMets_AndByte:
        call    ReadEEPROMByte
        andwf   Scratch0, F
        retlw   0


;------------------------------------------------------------------------------


GetFavouriteDataSlotIx:
; In: W = Favourite Pattern index (0 is most recently saved)
; Out: W = EWL slot index

        movf    NumFavs, W
        subwf   FavouriteIx, W
        btfsc   STATUS, C
        clrf    FavouriteIx
        movf    FavouriteIx, W
        subwf   FavHeadEWLSlotIx, W
        btfss   STATUS, C
        addlw   FAVOURITES_NUM_SLOTS
        return


;------------------------------------------------------------------------------


GetFavourite:
; Fetch the most recently saved Favoutite Pattern from EEPROM.
;
;                 bit
;              76543210  ; b = Bank index
;              --------  ; p = Pattern index
; First byte:  TTTbbbbb  ; T = Tempo index
; Second byte: CCpppppp  ; A = RampIxA
; Third byte:  CCAAAAAA  ; B = RampIxB
; Fourth byte: CCBBBBBB  ; C = RampIxC
;
; Out: FavouriteIx, BankIx, PatternIx
;      FavRecPatMod + PatMod_RampIxA, FavRecPatMod + PatMod_RampIxB, FavPatRampIxMidC

        call    SetEWLAccessForFav
        call    GetFavouriteDataSlotIx
        call    GetEWLDataAddress
        movf    EWLFind + EWLFind_DataSlotAddr, W
        movwf   EEPROMPtr
        call    ReadEEPROMByte
        movwf   BankIx
        movwf   FavRecPatMod + PatMod_TempoIx
        swapf   FavRecPatMod + PatMod_TempoIx, F
        lsrf    FavRecPatMod + PatMod_TempoIx, F
        movlw   0x1F
        andwf   BankIx, F
        movlw   0x07
        andwf   FavRecPatMod + PatMod_TempoIx, F
        call    ReadEEPROMByte
        movwf   PatternIx
        call    ReadEEPROMByte
        movwf   FavRecPatMod + PatMod_RampIxA
        call    ReadEEPROMByte
        movwf   FavRecPatMod + PatMod_RampIxB
        clrf    FavRecPatMod + PatMod_RampIxC
        btfsc   PatternIx, 6
        bsf     FavRecPatMod + PatMod_RampIxC, 0
        btfsc   PatternIx, 7
        bsf     FavRecPatMod + PatMod_RampIxC, 1
        btfsc   FavRecPatMod + PatMod_RampIxA, 6
        bsf     FavRecPatMod + PatMod_RampIxC, 2
        btfsc   FavRecPatMod + PatMod_RampIxA, 7
        bsf     FavRecPatMod + PatMod_RampIxC, 3
        btfsc   FavRecPatMod + PatMod_RampIxB, 6
        bsf     FavRecPatMod + PatMod_RampIxC, 4
        btfsc   FavRecPatMod + PatMod_RampIxB, 7
        bsf     FavRecPatMod + PatMod_RampIxC, 5
        movlw   0x3F
        andwf   PatternIx, F
        andwf   FavRecPatMod + PatMod_RampIxA, F
        andwf   FavRecPatMod + PatMod_RampIxB, F
        retlw   0


;------------------------------------------------------------------------------


SaveFavourite:
; Save the current Bank and Pattern to the Favourites list in EEPROM.
;
; The currently effective ramp indices used to modify a pattern's palette
; are saved within the individual Favourite Pattern record. When a Favourite
; Pattern is recalled, its ramp indices will be given priority.
;
;                 bit
;              76543210  ; b = Bank index
;              --------  ; p = Pattern index
; First byte:  TTTbbbbb  ; T = Tempo index
; Second byte: CCpppppp  ; A = RampIxA
; Third byte:  CCAAAAAA  ; B = RampIxB
; Fourth byte: CCBBBBBB  ; C = RampIxC
;
; In:  FavouriteIx, BankIx, PatternIx
;      EffectivePatRampIxA, FavRecPatMod + PatMod_RampIxB, FavPatRampIxMidC
;
; It is possible to have one pattern appear multiple times in the list.

        call    SetEWLAccessForFav
SaveFavourite_HaveEWLAccess:
        movf    FavHeadEWLSlotIx, W
        incf    WREG, F
        call    GetEWLDataAddress
        movf    EWLFind + EWLFind_DataSlotAddr, W
        movwf   EEPROMPtr
        ; Write the record.
        swapf   EffectivePatMod + PatMod_TempoIx, W
        lslf    WREG, F
        iorwf   BankIx, W
        call    WriteEEPROMByte_Smart
        movf    PatternIx, W
        btfsc   EffectivePatMod + PatMod_RampIxC, 0
        bsf     WREG, 6
        btfsc   EffectivePatMod + PatMod_RampIxC, 1
        bsf     WREG, 7
        call    WriteEEPROMByte_Smart
        movf    EffectivePatMod + PatMod_RampIxA, W
        btfsc   EffectivePatMod + PatMod_RampIxC, 2
        bsf     WREG, 6
        btfsc   EffectivePatMod + PatMod_RampIxC, 3
        bsf     WREG, 7
        call    WriteEEPROMByte_Smart
        movf    EffectivePatMod + PatMod_RampIxB, W
        btfsc   EffectivePatMod + PatMod_RampIxC, 4
        bsf     WREG, 6
        btfsc   EffectivePatMod + PatMod_RampIxC, 5
        bsf     WREG, 7
        call    WriteEEPROMByte_Smart
        ; Mark this record as complete.
        call    AdvanceEWLStatusRing
        movwf   FavHeadEWLSlotIx
        incf    NumFavs, W
        sublw   FAVOURITES_NUM_SLOTS
        movlw   0
        addwfc  NumFavs, F
        retlw   0


;------------------------------------------------------------------------------


ClampOrInvalidateFavIx:
; Clamp the Favourite Pattern index to 0..NumFavs-1 or 255 if that fails.
; A Favourite Pattern index of 255 means that no Favourite Pattern is playing.
;
; Bonus feature: If there are no Favourites at all and the index wasn't
; already 255, the Bank and Pattern indices will be reset.

        incf    FavouriteIx, W
        btfsc   STATUS, Z
        retlw   0  ; Already safely invalidated
        movf    NumFavs, W
        btfsc   STATUS, Z
        bra     _CoIFIx_Invalidate
        subwf   FavouriteIx, W
        btfss   STATUS, Z
        bra     _CoIFIx_Done
        decf    NumFavs, W
        movwf   FavouriteIx
_CoIFIx_Done:
        retlw   0
_CoIFIx_Invalidate:
        movlw   255
        movwf   FavouriteIx
        clrf    BankIx
        clrf    PatternIx
        retlw   0


;------------------------------------------------------------------------------


PromoteOrDelFavourite_Common:
; Delete a Favourite pattern entry by moving all older entries up one slot.
;
; This is rather complicated due to the wrap-around nature of the EWL rings.
; In any case, the slot corresponding to the oldest entry is filled with
; (0xFF, 0xFF, 0xFF, 0xFF).
;
; This is a low-level routine used by PromoteFavourite and DeleteFavourite,
; higher-level routines which properly update the full state.
;
; In: FavouriteIx and other stuff

        movf    NumFavs, W
        btfsc   STATUS, Z
        retlw   0
        call    SetEWLAccessForFav
        call    GetFavouriteDataSlotIx
        call    GetEWLDataAddress
        ; The number of tail entries to be moved closer to the head is
        ;   n = NumFavs - 1 - FavouriteIx
        incf    FavouriteIx, W
        subwf   NumFavs, W
        btfsc   STATUS, Z
        bra     _DelFav_TailShifted
        movwf   LoopCtr0
        incf    EWLFind + EWLFind_Ix, F  ; So Zero Flag detects underflow
_DelFav_Loop:
        movf    EWLFind + EWLFind_DataSlotAddr, W
        addlw   -FavRecSize
        movwf   EWLFind + EWLFind_NextIx  ; Repurposed as source address.
        movlw   FAVOURITES_NUM_SLOTS * FavRecSize
        decf    EWLFind + EWLFind_Ix, F
        btfsc   STATUS, Z
        addwf   EWLFind + EWLFind_NextIx, F
        call    _DelFav_Copy2Bytes
        call    _DelFav_Copy2Bytes
        movlw   -FavRecSize
        addwf   EWLFind + EWLFind_NextIx, F
        addwf   EWLFind + EWLFind_DataSlotAddr, F
        ; Switch to the next target address
        movf    EWLFind + EWLFind_NextIx, W
        movwf   EWLFind + EWLFind_DataSlotAddr
        decfsz  LoopCtr0, F
        bra     _DelFav_Loop
_DelFav_TailShifted:
        movf    EWLFind + EWLFind_DataSlotAddr, W
        movwf   EEPROMPtr
        call    _DelFav_Unprogram2Bytes
        call    _DelFav_Unprogram2Bytes
        decf    NumFavs, F
        ; Do not validate FavouriteIx! This deletion may be a part of
        ; a procedure to promote the removed item to the head of the list.
        retlw   0
_DelFav_Unprogram2Bytes:
        call    _DelFav_UnprogramByte
        ; Fall through...
_DelFav_UnprogramByte:
        movlw   255
        goto    WriteEEPROMByte
_DelFav_Copy2Bytes:
        call    _DelFav_CopyByte
        ; Fall through...
_DelFav_CopyByte:
        movf    EWLFind + EWLFind_NextIx, W
        movwf   EEPROMPtr
        call    ReadEEPROMByte
        movwf   Scratch0
        movf    EWLFind + EWLFind_DataSlotAddr, W
        movwf   EEPROMPtr
        incf    EWLFind + EWLFind_NextIx, F
        incf    EWLFind + EWLFind_DataSlotAddr, F
        movf    Scratch0, W
        goto    WriteEEPROMByte_Smart


;------------------------------------------------------------------------------


PromoteFavourite:
; Move a Favourite Pattern to the front of the list.
;
; The currently effective ramps index will be saved into the pattern
; that is promoted to the front of the list. This is not a problem
; because a Favourite Pattern that is being promoted is also the pattern
; that is currently active, so the effective ramps index will be either
; correct or harmless rubbish.
;
; In: FavouriteIx and other stuff

        call    ClampOrInvalidateFavIx
        incf    FavouriteIx, W
        lsrf    WREG, F
        btfsc   STATUS, Z
        retlw   0  ; FavouriteIx is either 255 or 0
        call    PromoteOrDelFavourite_Common
        goto    SaveFavourite_HaveEWLAccess


;------------------------------------------------------------------------------


DeleteFavourite:
; Remove a pattern from the Favourite Pattern list.
;
; Older patterns will each be moved up to fill the hole. If the list becomes
; empty, the Favourite Index will be invalidated, forcing the first standard
; pattern to begin playing.

        call    ClampOrInvalidateFavIx
        incf    FavouriteIx, W
        btfsc   STATUS, Z
        retlw   0
        call    PromoteOrDelFavourite_Common
        goto    ClampOrInvalidateFavIx


;------------------------------------------------------------------------------
; Simple settings in EPROM
;------------------------------------------------------------------------------


LoadRestrictionsFromEEPROM:
; Fetch the Mode Flags related to restrictions from EEPROM.
;
; Out: ModeFlags

        movlw   EEPROM_LockBits
        movwf   EEPROMPtr
        call    ReadEEPROMByte
        andlw   MF_MASK_RESTRICTIONS
        movwf   ModeFlags
        return


;------------------------------------------------------------------------------


SaveRestrictionsToEEPROM:
; Save the Mode Flags related to restrictions to EEPROM, if they have changed.
;
; In: ModeFlags

        movlw   EEPROM_LockBits
        movwf   EEPROMPtr
        movf    ModeFlags, W
        andlw   MF_MASK_RESTRICTIONS
        goto    WriteEEPROMByte_Smart


;------------------------------------------------------------------------------


LoadIntensityIxFromEEPROM:
; Fetch the Intensity Index from EEPROM.
;
; Out: IntensityIx

        movlw   EEPROM_Brightness
        movwf   EEPROMPtr
        call    ReadEEPROMByte
        andlw   7
        movwf   IntensityIx
        return


;------------------------------------------------------------------------------


SaveIntensityIxToEEPROM:
; Save the Intensity Index to EEPROM, if it has changed.
;
; In: IntensityIx

        movlw   EEPROM_Brightness
        movwf   EEPROMPtr
        movf    IntensityIx, W
        goto    WriteEEPROMByte_Smart


;------------------------------------------------------------------------------
; Input and UI functions
;------------------------------------------------------------------------------


WaitForReleaseOfAllButtons:
; Busy-wait until the Mode and Pattern buttons have been released for
; the debounce release time.

        ; Given that the system clock is set to 16MHz (implying 4MIPS),
        ; LoopCtr1:LoopCtr0 will be set to the required release debounce
        ; time in milliseconds.
_WfRoABs_Restart:
        movlw   LOW(DEBOUNCE_RELEASE_TIME_X8)
        movwf   LoopCtr0
        movlw   HIGH(DEBOUNCE_RELEASE_TIME_X8)
        movwf   LoopCtr1
        incf    LoopCtr1, F
_WfRoABs_Loop:
        btfsc   PORT_REGBIT_FR(UPB_MODE_BUTTON)
        bra     _WfRoABs_Restart
        btfsc   PORT_REGBIT_FR(UPB_PAT_BUTTON)
        bra     _WfRoABs_Restart
        nop
        nop
        movlw   163  ; Set loop delay 11 + 3n cycles
_WfRoABs_InnerDelayLoop:
        decfsz  WREG, F
        bra     _WfRoABs_InnerDelayLoop
        decfsz  LoopCtr0, F
        bra     $ + 2
        decfsz  LoopCtr1, F
        bra     _WfRoABs_Loop  ; 500 cycles per iteration
        ; Any buttons that were pressed are now definitely released.
        retlw   0


;------------------------------------------------------------------------------


RegisterButtonPress:
; Find the debounced state of the buttons.
;
; This function is usually called right after a button activation, indicating
; that a button is likely being pressed. (The activation might merely be a
; transient, though.)
;
; Out: W = Debounced input bits (0 if no buttons pressed)

        clrf    Scratch0
        btfsc   PORT_REGBIT_FR(UPB_MODE_BUTTON)
        bsf     Scratch0, INPUT_BIT_MODE
        btfsc   PORT_REGBIT_FR(UPB_PAT_BUTTON)
        bsf     Scratch0, INPUT_BIT_PATTERN
        movlw   LOW(DEBOUNCE_PRESS_TIME_X8)
        movwf   LoopCtr0
        movlw   HIGH(DEBOUNCE_PRESS_TIME_X8)
        movwf   LoopCtr1
        incf    LoopCtr1, F
_RegBP_DBPressLoop:
        btfss   PORT_REGBIT_FR(UPB_MODE_BUTTON)
        bcf     Scratch0, INPUT_BIT_MODE
        btfss   PORT_REGBIT_FR(UPB_PAT_BUTTON)
        bcf     Scratch0, INPUT_BIT_PATTERN
        movf    Scratch0, W
        btfsc   STATUS, Z
        bra     _RegBP_Done  ; Transient
        movlw   122  ; Set loop delay 12 + 4n cycles
_RegBP_InnerDelayLoop:
        nop
        decfsz  WREG, F
        bra     _RegBP_InnerDelayLoop
        decfsz  LoopCtr0, F
        bra     $ + 2
        decfsz  LoopCtr1, F
        bra     _RegBP_DBPressLoop  ; 500 cycles per iteration
        movf    Scratch0, W
_RegBP_Done:
        return


;------------------------------------------------------------------------------


AnimateLongPress:
; Animate a simple menu for as long as a button remains pressed.
;
; Once the start of a button press has been positively identified, call
; this function to play a special Long Press Cue Animation for as long
; as a (debounced) button is held. The return value is the animation
; frame index at the time of release. That value can then be transformed
; into a menu item index.
;
; Odd frame periods indicate flickering is required. The dimming
; of the LEDs in the flickering animation frames occurs when the
; (zero-based) frame index is even.
;
; :  T=4  :     T=7     :    T=6    :
;  ________  _   _   _   ___________  Normal intensity
; |       |_| |_| |_| |_|           |
;                                     Dimmed
; : : : : : : : : : : : : : : : : : :
;
; In: W = System Pattern ID
; Out: W = Frame index where button was released.

        call    GetEnumeratedSysPatAddr
        call    LoadPattern
        clrf    PWMCycleCounter
_AnimLP_Loop:
        call    FetchAnimFrame
        call    LoadLEDOCRs
        btfss   GAState + GAS_FramePeriod, 0
        bra     _AnimLP__SkipFlicker
        btfsc   GAState + GAS_FrameTCounter, 0
        bra     _AnimLP__SkipFlicker
        call    DimLEDOCRs
        ;call    DimLEDOCRs
_AnimLP__SkipFlicker:
        call    PerformPCPWMCycle
        decf    GAState + GAS_NumFrames, W
        subwf   GAState + GAS_FrameIx, W
        btfss   STATUS, C
        call    AdvanceAnimCounters
        incf    PWMCycleCounter, F
        btfsc   PORT_REGBIT_FR(UPB_MODE_BUTTON)
        clrf    PWMCycleCounter
        btfsc   PORT_REGBIT_FR(UPB_PAT_BUTTON)
        clrf    PWMCycleCounter
        movlw   DB_RELEASE_TIME_PWM_CYCLES
        subwf   PWMCycleCounter, W
        btfss   STATUS, C
        bra     _AnimLP_Loop
        movf    GAState + GAS_FrameIx, W
        return


;------------------------------------------------------------------------------


AnimateBootMenuOption_Tramp0:
        pagesel AnimateBootMenuOption_Page1
        call    AnimateBootMenuOption_Page1
        movlp   0
        return


;------------------------------------------------------------------------------
; Main functions
;------------------------------------------------------------------------------


Main:

        call    MashTheSnoozeButton
        pagesel ClearMemAndInitHardware_Page1
        call    ClearMemAndInitHardware_Page1
        movlp   0

        call    GetFavMetrics
        movlw   255
        movwf   FavouriteIx
        movwf   ModFavouriteIx
        movwf   ModBankIx
        movwf   ModPatternIx

        call    LoadIntensityIxFromEEPROM
        call    LoadRestrictionsFromEEPROM

        movlw   SYSPAT_LPMENU_POWER_ON
        call    AnimateLongPress
        lsrf    WREG, W
        andlw   7
        brw
        bra     _Main_SkipBootMenu
        bra     _Main_ChooseIntensity
        bra     _Main_ChooseLUPMode
        bra     _Main_ChooseRestrictions
        goto    ShutDown
        goto    ShutDown
        ;goto    ShutDown
        ;goto    ShutDown

_Main_ChooseIntensity:
        movlw   SYSPAT_MENU_INTENSITY
        call    GetEnumeratedSysPatAddr
        clrw
        movwf   IntensityIx
        call    AnimateBootMenuOption_Tramp0
        sublw   5 - 1
        andlw   7
        movwf   IntensityIx
        call    SaveIntensityIxToEEPROM
        bra     _Main_SkipBootMenu

_Main_ChooseLUPMode:
        movlw   SYSPAT_MENU_LOCK_LUP
        call    GetEnumeratedSysPatAddr
        clrw
        btfsc   ModeFlags, MF_BIT_LUPLOCKED
        bsf     WREG, 0
        call    AnimateBootMenuOption_Tramp0
        bcf     ModeFlags, MF_BIT_LUPLOCKED
        btfsc   WREG, 0
        bsf     ModeFlags, MF_BIT_LUPLOCKED
        call    SaveRestrictionsToEEPROM
        bra     _Main_SkipBootMenu

_Main_ChooseRestrictions:
        movlw   SYSPAT_MENU_RESTRICTIONS
        call    GetEnumeratedSysPatAddr
        movf    ModeFlags, W
        call    _Main_ChooseR_Bits2Ix
        call    AnimateBootMenuOption_Tramp0
        call    _Main_ChooseR_Ix2Bits
        bcf     ModeFlags, MF_BIT_NOFAVSPILL
        bcf     ModeFlags, MF_BIT_FAVPROTECT
        bcf     ModeFlags, MF_BIT_RESTRICTED
        iorwf   ModeFlags, F
        call    SaveRestrictionsToEEPROM
        bra     _Main_SkipBootMenu
_Main_ChooseR_Bits2Ix:
        btfsc   WREG, MF_BIT_RESTRICTED
        retlw   3
        btfsc   WREG, MF_BIT_FAVPROTECT
        retlw   2
        btfsc   WREG, MF_BIT_NOFAVSPILL
        retlw   1
        retlw   0
_Main_ChooseR_Ix2Bits:
        andlw   0x03
        brw
x = 0
        retlw   x
x |= (1 << MF_BIT_NOFAVSPILL)
        retlw   x
x |= (1 << MF_BIT_FAVPROTECT)
        retlw   x
x |= (1 << MF_BIT_RESTRICTED)
        retlw   x

_Main_SkipBootMenu:

        call    LoadIntensityIxFromEEPROM
        call    LoadRestrictionsFromEEPROM
        call    GetLastUsedPattern
        bra     _Main_ProgSetBaP

_Main_Loop:

        btfsc   PORT_REGBIT_FR(UPB_MODE_BUTTON)
        bra     _Main_ButtonPress
        btfss   PORT_REGBIT_FR(UPB_PAT_BUTTON)
        bra     _Main_SkipButtonPress
_Main_ButtonPress:
        bcf     ModeFlags, MF_BIT_CUE
        call    RegisterButtonPress
        btfsc   WREG, INPUT_BIT_MODE
        bra     _Main_ModeBtnPressed
        btfss   WREG, INPUT_BIT_PATTERN
        bra     _Main_SkipButtonPress
_Main_PatBtnPressed:
        call    _Main_LoadSlideshowCtr
        btfsc   ModeFlags, MF_BIT_SLIDESHOW
        bra     _Main_PatBtnPressed_SS
        incfsz  FavouriteIx, W
        bra     _Main_PatBtnPressed_Fav
_Main_PatBtnPressed_Std:
        btfsc   ModeFlags, MF_BIT_RESTRICTED
        bra     _Main_PatBP_Std_Restricted
        btfsc   ModeFlags, MF_BIT_FAVPROTECT
        bra     _Main_PatBP_Std_Restricted
        btfss   ModeFlags, MF_BIT_NOFAVSPILL
        bra     _Main_PatBP_Std_SavePermitted
        movlw   FAVOURITES_NUM_SLOTS
        subwf   NumFavs, W
        btfss   STATUS, C
        bra     _Main_PatBP_Std_SavePermitted
_Main_PatBP_Std_FavsFull:
        movlw   SYSPAT_LPMENU_FAVS_FULL
        call    AnimateLongPress
        sublw   4 - 3
        btfss   STATUS, C
        bra     _Main_ProgSetBaP
        sublw   4 - 3  ; Restore W
        bra     _Main_PatBP_Std_PreLookup
_Main_PatBP_Std_SavePermitted:
        movlw   SYSPAT_LPMENU_SAVE_FAV
        call    AnimateLongPress
_Main_PatBP_Std_PreLookup:
        lsrf    WREG, F
        andlw   0x07
_Main_PatBP_Std_Lookup:
        brw
        bra     _Main_NextPat
        bra     _Main_StartSS1
        bra     _Main_StartSS2
        bra     _Main_StartSS3
        bra     _Main_SaveFav
        bra     _Main_ProgSetBaP
        bra     _Main_ProgSetBaP
        bra     _Main_ProgSetBaP
_Main_PatBP_Fav_Restricted:  ; Same as for Standard Patterns
_Main_PatBP_Std_Restricted:
        movlw   SYSPAT_LPMENU_SLIDESHOW_ONLY
        call    AnimateLongPress
        lsrf    WREG, F
        sublw   4 - 1
        btfss   STATUS, C
        bra     _Main_ProgSetBaP
        sublw   4 - 1
        bra     _Main_PatBP_Std_Lookup
_Main_PatBtnPressed_Fav:
        btfsc   ModeFlags, MF_BIT_RESTRICTED
        bra     _Main_PatBP_Fav_Restricted
        btfsc   ModeFlags, MF_BIT_FAVPROTECT
        bra     _Main_PatBP_Fav_Restricted
        movlw   SYSPAT_LPMENU_MOVE_OR_DEL_FAV
        call    AnimateLongPress
        lsrf    WREG, W
        andlw   0x07
        brw
        bra     _Main_NextPat
        bra     _Main_StartSS1
        bra     _Main_StartSS2
        bra     _Main_StartSS3
        bra     _Main_PromoteFav
        bra     _Main_DeleteFav
        bra     _Main_ProgSetBaP
        bra     _Main_ProgSetBaP
_Main_PatBtnPressed_SS:
        btfsc   ModeFlags, MF_BIT_RESTRICTED
        bra     _Main_PatBP_SS_Restricted
        incfsz  FavouriteIx, W
        bra     _Main_PatBP_SS_Restricted  ; Favoutes are not to be edited.
        movlw   SYSPAT_LPMENU_STOP_SLIDESHOW
        call    AnimateLongPress
        lsrf    WREG, W
        btfsc   STATUS, Z
        bra     _Main_PrevPat
        bcf     ModeFlags, MF_BIT_SLIDESHOW
        xorlw   2
        btfsc   STATUS, Z
        bra     _Main_AdjPat
        bra     _Main_ManuallySetBaP
_Main_PatBP_SS_Restricted:
        movlw   SYSPAT_LPMENU_STOP_SSHOW_ONLY
        call    AnimateLongPress
        lsrf    WREG, W
        btfsc   STATUS, Z
        bra     _Main_PrevPat
        bcf     ModeFlags, MF_BIT_SLIDESHOW
        bra     _Main_ManuallySetBaP
_Main_AdjPat:
        pagesel AnimatePatAdjMode_Page1
        call    AnimatePatAdjMode_Page1
        movlp   0
        decfsz  WREG, F
        bra     _Main_ProgSetBaP
        goto    ShutDown
_Main_PrevPat:
        incfsz  FavouriteIx, W
        bra     _Main_PrevPat_Fav
_Main_PrevPat_Std:
        movlw   1
        subwf   PatternIx, F
        btfsc   STATUS, C
        bra     _Main_ManuallySetBaP
        call    _Main_CueWraparound
        bra     _Main_ManuallySetBaP
_Main_PrevPat_Fav:
        movlw   1
        subwf   FavouriteIx, F
        btfsc   STATUS, C
        bra     _Main_ManuallySetBaP
        decf    NumFavs, W
        movwf   FavouriteIx
        call    _Main_CueWraparound
        bra     _Main_ManuallySetBaP
_Main_NextPat:
        incfsz  FavouriteIx, W
        bra     _Main_NextPat_Fav
_Main_NextPat_Std:
        incf    PatternIx, F
        call    GetPatternFromBank_Tramp0  ; Validate PatternIx
        movf    PatternIx, W
        btfsc   STATUS, Z
        call    _Main_CueWraparound
        bra     _Main_ManuallySetBaP
_Main_NextPat_Fav:
        incf    FavouriteIx, F
        movf    NumFavs, W
        subwf   FavouriteIx, W
        btfss   STATUS, C
        bra     _Main_ManuallySetBaP
        clrf    FavouriteIx
        call    _Main_CueWraparound
        bra     _Main_ManuallySetBaP
_Main_SaveFav:
        call    SaveFavourite
        bra     _Main_ProgSetBaP
_Main_PromoteFav:
        call    PromoteFavourite
        bra     _Main_ProgSetBaP
_Main_DeleteFav:
        call    DeleteFavourite
        movf    NumFavs, W
        btfsc   STATUS, Z
        bra     _Main_AdvBank_Fav
        bra     _Main_ProgSetBaP
_Main_StartSS1:
_Main_StartSS2:
_Main_StartSS3:
        bsf     ModeFlags, MF_BIT_SLIDESHOW
        movwf   SlideshowIntervalIx
        call    _Main_LoadSlideshowCtr
        bra     _Main_ProgSetBaP
_Main_ModeBtnPressed:
        call    _Main_LoadSlideshowCtr
        movlw   SYSPAT_LPMENU_MODE
        call    AnimateLongPress
        lsrf    WREG, W
        btfsc   STATUS, Z
        bra     _Main_AdvBank
        decfsz  WREG, W
        goto    ShutDown
_Main_JumpToFavs:
        clrf    PatternIx
        clrf    BankIx
        bra     _Main_FinishJumpToFavsOrBank0
_Main_AdvBank:
        incfsz  FavouriteIx, W
        bra     _Main_AdvBank_Fav
_Main_AdvBank_Std:
        clrf    PatternIx
        btfsc   ModeFlags, MF_BIT_RESTRICTED
        bra     _Main_ProgSetBaP
        incf    BankIx, F
        call    GetPatternFromBank_Tramp0  ; Validate BankIx
        movf    BankIx, W
        btfss   STATUS, Z
        bra     _Main_ManuallySetBaP
_Main_FinishJumpToFavsOrBank0:
        movf    NumFavs, W
        btfsc   STATUS, Z
        bra     _Main_ManuallySetBaP_CueStds
        clrf    FavouriteIx
        call    _Main_CueEnterFavs
        bra     _Main_ManuallySetBaP
_Main_AdvBank_Fav:
        clrf    FavouriteIx
        btfsc   ModeFlags, MF_BIT_RESTRICTED
        bra     _Main_ProgSetBaP
        comf    FavouriteIx, F
        clrf    BankIx
        clrf    PatternIx
_Main_ManuallySetBaP_CueStds:
        call    _Main_CueEnterStds
        bra     _Main_ManuallySetBaP
_Main_ManuallySetBaP_CueWrap:
        call    _Main_CueWraparound
_Main_ManuallySetBaP:
        btfss   ModeFlags, MF_BIT_LUPLOCKED
        call    SaveLastUsedPattern
_Main_ProgSetBaP:
        call    _Main_SetBankPatRampStuff
_Main_BankAndPatternSet:
        btfss   ModeFlags, MF_BIT_CUE
        bra     _Main_SkipCueInit
        ; Swap the Cue pattern pointer and FSR0.
        movf    FSR0L, W
        xorwf   PatternAddr + 0, F
        xorwf   PatternAddr + 0, W
        movwf   FSR0L
        xorwf   PatternAddr + 0, F
        movf    FSR0H, W
        xorwf   PatternAddr + 1, F
        xorwf   PatternAddr + 1, W
        movwf   FSR0H
        xorwf   PatternAddr + 1, F
        call    LoadPattern
        bra     _Main_SkipButtonPress
_Main_SkipCueInit:
        call    LoadPattern
        call    UpdateEffectivePatMod
        call    ModifyPattern_Tramp0
        call    ForbidAllBlackPatMod_Tramp0
_Main_SkipButtonPress:

        ; The normal animation playing happens here.
        ; 5 cycles

        btfsc   ModeFlags, MF_BIT_CUE
        bra     _Main_AnimateCue
        ; 7 cycles
        call    FetchAnimFrame  ;  6 + (49|103|82|87|88) cycles
        call    LoadLEDOCRs  ; 6 + 22 cyles nominal
        call    PerformPCPWMCycle  ; 9026 cycles (67 blank)
        call    AdvanceAnimCounters  ; 25 cycles

        btfss   ModeFlags, MF_BIT_SLIDESHOW
        bra     _Main_Loop  ; 2 cycles
        ; Slideshow mode
        movlw   1
        subwf   SlideshowCounter + 0, F
        movlw   0
        subwfb  SlideshowCounter + 1, F
        movf    SlideshowCounter + 0, W
        iorwf   SlideshowCounter + 1, W
        btfss   STATUS, Z
        bra     _Main_Loop
        ; Next entry in slideshow!
        bcf     ModeFlags, MF_BIT_CUE
        incfsz  FavouriteIx, W
        bra     _Main_SSAdv_Fav
_Main_SSAdv_Std:
        movf    PatternIx, W
        movwf   Scratch2
        incf    PatternIx, F
        call    GetPatternFromBank_Tramp0
        movf    PatternIx, W
        xorwf   Scratch2, W
        btfsc   STATUS, Z
        bra     _Main_Loop  ; Do not restart a one-pattern slideshow!
        bra     _Main_SSAdv_Common
_Main_SSAdv_Fav:
        decf    NumFavs, W
        btfsc   STATUS, Z
        bra     _Main_Loop  ; Do not restart a one-pattern slideshow!
        incf    FavouriteIx, F
        movf    NumFavs, W
        subwf   FavouriteIx, W
        btfsc   STATUS, C
        clrf    FavouriteIx
        call    GetFavourite
        call    GetPatternFromBank_Tramp0
_Main_SSAdv_Common:
        call    LoadPattern
        call    UpdateEffectivePatMod
        call    ModifyPattern_Tramp0
        call    ForbidAllBlackPatMod_Tramp0
        call    _Main_LoadSlideshowCtr
        bra     _Main_Loop

_Main_AnimateCue:
        pagesel AnimateCue_Page1
        call    AnimateCue_Page1
        movlp   0
        btfsc   ModeFlags, MF_BIT_CUE
        bra     _Main_Loop
        call    _Main_SetBankPatRampStuff
        ;movf    PatternAddr + 0, W
        ;movwf   FSR0L
        ;movf    PatternAddr + 1, W
        ;movwf   FSR0H
        call    LoadPattern
        call    UpdateEffectivePatMod
        call    ModifyPattern_Tramp0
        call    ForbidAllBlackPatMod_Tramp0
        bra     _Main_Loop

_Main_SetBankPatRampStuff:
        incfsz  FavouriteIx, W
        call    GetFavourite
        call    GetPatternFromBank_Tramp0
        retlw   0

_Main_CueWraparound:
        movlw   SYSPAT_CUE_WRAP
        bra     _Main_Cue_Common
_Main_CueEnterFavs:
        movlw   SYSPAT_CUE_ENTER_FAVS
        bra     _Main_Cue_Common
_Main_CueEnterStds:
        movlw   SYSPAT_CUE_ENTER_STDS
        ; Fall through
_Main_Cue_Common:
        call    GetEnumeratedSysPatAddr
        movf    FSR0L, W
        movwf   PatternAddr + 0
        movf    FSR0H, W
        movwf   PatternAddr + 1
        bsf     ModeFlags, MF_BIT_CUE
        retlw   0

_Main_LoadSlideshowCtr:
        movlp   0x00
        movlw   LOW(PM_SlideshowIntvTable)
        movwf   FSR0L
        movlw   HIGH(PM_SlideshowIntvTable)
        movwf   FSR0H
        movf    SlideshowIntervalIx, W
        andlw   0x03
        call    FetchPMWordFromTable
        movf    FSR0L, W
        movwf   SlideshowCounter + 0
        movf    FSR0H, W
        movwf   SlideshowCounter + 1
        retlw   0


;------------------------------------------------------------------------------

Page0CodeEnd:
Page0WordsUsed = Page0CodeEnd - 0x0000
Page0WordsFree = 0x0800 - Page0CodeEnd

;==============================================================================
; Page 1 (0x0800..0x0FFF)
;==============================================================================

        ORG     0x0800

;------------------------------------------------------------------------------
; Basic system initialisation
;------------------------------------------------------------------------------


ClearLinearMemory_Page1:
; The linear memory excludes the 16 bytes of common RAM at 0x70..0x7F, which
; would appear between 0x2049 and 0x2050 in linear address space.

        movlw   LOW(LINEAR_RAM_START)
        movwf   FSR0L
        movlw   HIGH(LINEAR_RAM_START)
        movwf   FSR0H
        movlw   LOW(LINEAR_RAM_SIZE)
        movwf   FSR1L
        movlw   HIGH(LINEAR_RAM_SIZE) + 1  ; +1 because of loop counter quirk
        movwf   FSR1H
        clrw
_ClearLMem_Loop:
        movwi   FSR0++
        decfsz  FSR1L, F
        bra     _ClearLMem_Loop
        decfsz  FSR1H, F
        bra     _ClearLMem_Loop
        retlw   0


;------------------------------------------------------------------------------


ClearCommonMemory_Page1:
; There are 16 bytes of Common (S)RAM at 0x70..0x7F, accessible from any
; bank in Traditional addressing and excluded from the Linear addressing.

        movlw   0x70
        movwf   FSR0L
        movlw   0
        movwf   FSR0H
        movlw   16
        movwf   FSR1L
        movlw   0
_ClearCMem_Loop:
        movwi   FSR0++
        decfsz  FSR1L, F
        bra     _ClearCMem_Loop
        retlw   0


;------------------------------------------------------------------------------


ConditionalyInitEEPROM_Page1:
; If a particular poi configuration byte in EEPROM looks unprogrammed,
; assume the whole EEPROM is unprogrammed and prepare a sensible starting
; configuration.

        movlp   0x00
        movlw   EEPROM_Brightness
        movwf   EEPROMPtr
        call    ReadEEPROMByte
        incf    WREG, F
        btfss   STATUS, Z
        retlw   0
        ; 255 is not a sensible value for this byte.
        ; Assume the EEPROM is full of 255s.
        ; (The Favourites EWL Data Ring can be left as all 255s)
        movlw   LOW(_CondInitE_BAPM_Pokes)
        movwf   FSR0L
        movlw   HIGH(_CondInitE_BAPM_Pokes)
        movwf   FSR0H
        clrf    BAPMRec + BAPM_Status
        movlw   _CondInitE_NUM_POKES
        movwf   LoopCtr0
_CondInitE_InitLoop:
        call    ReadBAPMByte
        movwf   EEPROMPtr
        call    ReadBAPMByte
        call    WriteEEPROMByte
        decfsz  LoopCtr0, F
        bra     _CondInitE_InitLoop
        retlw   0
_CondInitE_BAPM_Pokes:
        bablock
        dba2b   EEPROM_Brightness, DEFAULT_INTENSITY_IX
        dba2b   EEPROM_LockBits, DEFAULT_LOCK_BITS
        dba2b   EEPROM_LUPStatusRing, 0
        dba2b   EEPROM_LUPDataRing + 0, 0
        dba2b   EEPROM_LUPDataRing + 1, 0
_CondInitE_NUM_POKES equ BAPM_BYTE_OFFSET / 2
        endbab


;------------------------------------------------------------------------------


ClearMemAndInitHardware_Page1:
; Perform basic power-on initialisation.

        call    ClearLinearMemory_Page1
        call    ClearCommonMemory_Page1
        call    ConditionalyInitEEPROM_Page1
        movlp   0x00
        goto    ConfigureHardware


;------------------------------------------------------------------------------
; Additional UI stuff
;------------------------------------------------------------------------------


Add4WBAPMOffset_Page1:
; Add 4*W to the current Byte-Addressable Program Memory address.
;
; The current BAPM address is in FSR0 and BAPMRec.

        movwf   Div7Rec + Div7_Dividend + 0
        clrf    Div7Rec + Div7_Dividend + 1
        lslf    Div7Rec + Div7_Dividend + 0, F
        rlf     Div7Rec + Div7_Dividend + 1, F
        lslf    Div7Rec + Div7_Dividend + 0, F
        rlf     Div7Rec + Div7_Dividend + 1, F
        pagesel AddBAPMOffset
        call    AddBAPMOffset
        retlw   0


;------------------------------------------------------------------------------


GetPatternFromBank_Page1:
; Load the default pattern modifications and return the pattern header address.
;
; In: BankIx, PatternIx
; Out: FSR0 = Pattern header address in Program Memory
;      PatternFUPScaler = Frame Unit Period scaler: 0..3 => (1, 2, 8, 32)
;      DefaultPatMod
;      ModeFlags[MF_BIT_EXTERNALRAMPS]
;      ModeFlags[MF_BIT_RAMP16]
;      BankIx, PatternIx validated

        movlp   0
        movlw   LOW(PM_BankTable)
        movwf   FSR0L
        movlw   HIGH(PM_BankTable)
        movwf   FSR0H
        moviw   FSR0++
        subwf   BankIx, W
        btfsc   STATUS, C
        clrf    BankIx
        movf    BankIx, W
        call    FetchPMWordFromTable
        bsf     FSR0H, 7
        ; FSR0 now points the pattern table for the current bank.
        ; The lower 8 bits of the first word of that table is the
        ; number of patterns in that bank.
        incfsz  PatternIx, W
        bra     _GetPA_PatIxZeroPlus
        decf    INDF0, W
        movwf   PatternIx
_GetPA_PatIxZeroPlus:
        moviw   FSR0++
        subwf   PatternIx, W
        btfsc   STATUS, C
        clrf    PatternIx

        ; Bank table pattern records are four BAPM bytes each.
        clrf    BAPMRec + BAPM_Status
        movf    PatternIx, W
        pagesel Add4WBAPMOffset_Page1
        call    Add4WBAPMOffset_Page1
        movlp   0

        ; Bank pattern entry format:
        ;
        ;                  bit      p = Enumerated pattern index (9 bits)
        ;               76543210    e = External ramps mode
        ;               --------    s = FUP scaler: 0..3 => (1, 2, 8, 32)
        ; First Byte:   pppppppp    T = Frame Unit Period - 1
        ; Second Byte:  eTAAAAAA    A = Colour ramp index A
        ; Third Byte:   TTBBBBBB    B = Colour ramp index B
        ; Fourth Byte:  ssCCCCCC    C = Colour ramp index C

        call    ReadBAPMByte
        movwf   Scratch1  ; Temporarily store enum pattern index here.
        call    ReadBAPMByte
        movwf   DefaultPatMod + PatMod_RampIxA
        call    ReadBAPMByte
        movwf   DefaultPatMod + PatMod_RampIxB
        call    ReadBAPMByte
        movwf   DefaultPatMod + PatMod_RampIxC
        clrf    PatternFUPScaler
        btfsc   DefaultPatMod + PatMod_RampIxC, 6
        bsf     PatternFUPScaler, 0
        btfsc   DefaultPatMod + PatMod_RampIxC, 7
        bsf     PatternFUPScaler, 1
        clrf    DefaultPatMod + PatMod_TempoIx
        btfsc   DefaultPatMod + PatMod_RampIxB, 6
        bsf     DefaultPatMod + PatMod_TempoIx, 0
        btfsc   DefaultPatMod + PatMod_RampIxB, 7
        bsf     DefaultPatMod + PatMod_TempoIx, 1
        btfsc   DefaultPatMod + PatMod_RampIxA, 6
        bsf     DefaultPatMod + PatMod_TempoIx, 2
        bcf     ModeFlags, MF_BIT_EXTERNALRAMPS
        btfsc   DefaultPatMod + PatMod_RampIxA, 7
        bsf     ModeFlags, MF_BIT_EXTERNALRAMPS
        movlw   0x3F
        andwf   DefaultPatMod + PatMod_RampIxA, F
        andwf   DefaultPatMod + PatMod_RampIxB, F
        andwf   DefaultPatMod + PatMod_RampIxC, F
        ; Fetch the pattern header address.
        movlw   LOW(PM_PatAddrTable)
        movwf   FSR0L
        movlw   HIGH(PM_PatAddrTable)
        movwf   FSR0H
        movf    Scratch1, W
        call    FetchPMWordFromTable
        bsf     FSR0H, 7
        ; Now that we have the pattern header address, we can
        ; see if it is the sort which uses a special 16-colour
        ; ramp.
        bcf     ModeFlags, MF_BIT_RAMP16
        movlw   NUM_RAMP16_PALETTES
        movwf   LoopCtr0
        movf    LoopCtr0, F
        btfsc   STATUS, Z
        bra     _GetPA_SkipR16Test
        ; There is at least one of those special 16-colour palettes.
        movlp   0
        addfsr  FSR0, 3
        call    ReadPMHighBits
        bsf     WREG, 7
        movwf   Scratch1
        movf    INDF0, W
        movwf   Scratch0
        addfsr  FSR0, -3
        ; Stash the pattern header address somewhere safe.
        movf    FSR0L, W
        movwf   FSR1L
        movf    FSR0H, W
        movwf   FSR1H
        movlw   LOW(PM_Ramp16AddrTable)
        movwf   FSR0L
        movlw   HIGH(PM_Ramp16AddrTable)
        movwf   FSR0H
_GetPA_R16_loop:
        call    ReadPMHighBits
        bsf     WREG, 7
        xorwf   Scratch1, W
        btfss   STATUS, Z
        bra     _GetPA_R16_Next
        movf    INDF0, W
        xorwf   Scratch0, W
        btfss   STATUS, Z
        bra     _GetPA_R16_Next
        ; Found a match!
        bsf     ModeFlags, MF_BIT_RAMP16
        bra     _GetPA_R16_RestorePatPtr
_GetPA_R16_Next:
        decfsz  LoopCtr0, F
        bra     _GetPA_R16_loop
_GetPA_R16_RestorePatPtr:
        movf    FSR1L, W
        movwf   FSR0L
        movf    FSR1H, W
        movwf   FSR0H
        pagesel GetPatternFromBank_Page1
_GetPA_SkipR16Test:
        retlw   0


;------------------------------------------------------------------------------


ModifyPattern_Page1:
; Overwrite a pattern's expanded palette with colour ramps and a custom
; frame unit period.
;
; The expanded pattern's display format must be paletted and the pattern
; must have am appropriately sized palette loaded to SRAM.
;
; In: GAState configured by LoadPattern, say
;     PatternFUPScaler
;     EffectivePatMod

        incf    EffectivePatMod + PatMod_TempoIx, W
        movwf   GAState + GAS_FramePeriodUnit
        comf    PatternFUPScaler, W
        andlw   3
        lslf    WREG, F
        brw
        lslf    GAState + GAS_FramePeriodUnit, F
        lslf    GAState + GAS_FramePeriodUnit, F
        lslf    GAState + GAS_FramePeriodUnit, F
        lslf    GAState + GAS_FramePeriodUnit, F
        lslf    GAState + GAS_FramePeriodUnit, F  ; Overflow may happen here...
        nop
        movlw   255
        btfsc   STATUS, C
        movwf   GAState + GAS_FramePeriodUnit     ; ...but is handled here.

        ; Check that the pattern accepts external ramps.
        btfss   ModeFlags, MF_BIT_EXTERNALRAMPS
        retlw   0
        movf    GAState + GAS_AnimControlFlags, W
        andlw   ACF_FORMAT_MASK
        sublw   2 - 1
        btfsc   STATUS, C
        retlw   0  ; A direct colour mode: No palette to modify!

        ; Set up the palette mapping destination.
        movf    GAState + GAS_PalettePtr + 0, W
        movwf   MapWS + MapWS_DestPtr + 0
        movf    GAState + GAS_PalettePtr + 1, W
        movwf   MapWS + MapWS_DestPtr + 1

        btfsc   ModeFlags, MF_BIT_RAMP16
        bra     _ModPat_Ramp16

        ; Map from the standard palette.
        clrf    MapWS + MapWS_SourceBAPMStatus
        movlw   LOW(BAPM_Pal_Basic)
        movwf   MapWS + MapWS_SourcePtr + 0
        movlw   HIGH(BAPM_Pal_Basic)
        movwf   MapWS + MapWS_SourcePtr + 1

        pagesel ModifyPattern_Page1

        movf    EffectivePatMod + PatMod_RampIxA, W
        call    _ModPat_GetRampAddr
        movlw   4
        movwf   MapWS + MapWS_NumEntries
        call    _ModPat_LoadPaletteSection
        movf    GAState + GAS_AnimControlFlags, W
        andlw   ACF_FORMAT_MASK
        xorlw   PATDF_4C
        btfsc   STATUS, Z
        retlw   0

        movf    EffectivePatMod + PatMod_RampIxB, W
        call    _ModPat_GetRampAddr
        call    _ModPat_SkipMapByte
        movlw   3
        movwf   MapWS + MapWS_NumEntries
        call    _ModPat_LoadPaletteSection

        movf    EffectivePatMod + PatMod_RampIxC, W
        call    _ModPat_GetRampAddr
        call    _ModPat_SkipMapByte
        movlw   3
        movwf   MapWS + MapWS_NumEntries
        goto    _ModPat_LoadPaletteSection

_ModPat_Ramp16:
        ; A special palette is used, one that is ordered for a single ramp.
        ; Set the palette mapper to copy without mapping.
        movlw   LOW(PM_Ramp16AddrTable)
        movwf   FSR0L
        movlw   HIGH(PM_Ramp16AddrTable)
        movwf   FSR0H
        movlw   NUM_RAMP16_PALETTES
        subwf   EffectivePatMod + PatMod_RampIxA, W
        btfsc   STATUS, C
        clrf    EffectivePatMod + PatMod_RampIxA
        movf    EffectivePatMod + PatMod_RampIxA, W
        movlp   0
        call    FetchPMWordFromTable
        bsf     FSR0H, 7
        movf    FSR0L, W
        movwf   MapWS + MapWS_SourcePtr + 0
        movf    FSR0H, W
        movwf   MapWS + MapWS_SourcePtr + 1
        clrf    MapWS + MapWS_SourceBAPMStatus
        clrf    MapWS + MapWS_MapPtr + 0
        clrf    MapWS + MapWS_MapPtr + 1
        clrf    MapWS + MapWS_MapBAPMStatus
        movlw   16
        movwf   MapWS + MapWS_NumEntries
        call    LoadMappedPalette
        pagesel ModifyPattern_Page1
        retlw   0

_ModPat_GetRampAddr:
        movwf   Scratch0
        movlw   LOW(BAPM_ColourRamps)
        movwf   FSR0L
        movlw   HIGH(BAPM_ColourRamps)
        movwf   FSR0H
        clrf    BAPMRec + BAPM_Status
        movf    Scratch0, W
        sublw   NUM_RAMPS - 1
        btfss   STATUS, C
        movlw   NUM_RAMPS - 1
        sublw   NUM_RAMPS - 1
        call    Add4WBAPMOffset_Page1
        pagesel ModifyPattern_Page1
        bra     _ModPat_LoadMapPtr
_ModPat_SkipMapByte:
        movf    MapWS + MapWS_MapPtr + 0, W
        movwf   FSR0L
        movf    MapWS + MapWS_MapPtr + 1, W
        movwf   FSR0H
        movf    MapWS + MapWS_MapBAPMStatus, W
        movwf   BAPMRec + BAPM_Status
        pagesel ReadBAPMByte
        call    ReadBAPMByte
        pagesel ModifyPattern_Page1
        ; Fall through...
_ModPat_LoadMapPtr:
        movf    FSR0L, W
        movwf   MapWS + MapWS_MapPtr + 0
        movf    FSR0H, W
        movwf   MapWS + MapWS_MapPtr + 1
        movf    BAPMRec + BAPM_Status, W
        bcf     WREG, BAPM_BIT_CACHED
        movwf   MapWS + MapWS_MapBAPMStatus
        retlw   0
_ModPat_LoadPaletteSection:
        pagesel LoadMappedPalette
        call    LoadMappedPalette
        pagesel ModifyPattern_Page1
        movf    FSR1L, W
        movwf   MapWS + MapWS_DestPtr + 0
        movf    FSR1H, W
        movwf   MapWS + MapWS_DestPtr + 1
        retlw   0


;------------------------------------------------------------------------------


ForbidAllBlackPatMod_Page1:
; Keep the colour ramps from causing the pattern to be entirely black.

        call    AnalysePaletteUse_Page1
        btfss   PaletteUsageMetrics + PUM_IsPaletted, 0
        retlw   0
        btfss   ModeFlags, MF_BIT_EXTERNALRAMPS
        retlw   0
        btfsc   ModeFlags, MF_BIT_RAMP16
        retlw   0

        movf    EffectivePatMod + PatMod_RampIxA, W
        xorlw   R_BLK
        movwf   Scratch0
        movlw   1
        subwf   PaletteUsageMetrics + PUM_NumUserRamps, W
        btfss   STATUS, C
        retlw   0   ; No ramps
        btfsc   STATUS, Z
        bra     _FABPM_CheckMask  ; Just Ramp A
        movf    EffectivePatMod + PatMod_RampIxB, W
        xorlw   R_BLK
        iorwf   Scratch0, F
        btfss   PaletteUsageMetrics + PUM_NumUserRamps, 0
        bra     _FABPM_CheckMask  ; Ramps A and B
        movf    EffectivePatMod + PatMod_RampIxC, W
        xorlw   R_BLK
        iorwf   Scratch0, F
        ; Ramps A, B and C
_FABPM_CheckMask:
        movf    Scratch0, W
        btfss   STATUS, Z
        retlw   0
        movlw   LOW(PM_Pat_HaveYouSeenMyPoi)
        movwf   FSR0L
        movlw   HIGH(PM_Pat_HaveYouSeenMyPoi)
        movwf   FSR0H
        movlp   0
        call    LoadPattern
        pagesel ForbidAllBlackPatMod_Page1
        retlw   0


;------------------------------------------------------------------------------


AnimateBootMenuOption_Page1:
; Animate a simple tap-to-cycle menu until the Mode button is pressed.
;
; Each animation frame corresponds to one menu item. The unit frame
; period must be 1 and each frame must has a period of 1.
;
; In: FSR0 = Simple menu pattern header in Program Memory
;     W = Staring frame index
; Out: W = Frame index where button was released.

        movlp   0

        movwf   PWMCycleCounter  ; Repurposed
        call    LoadPattern
        movf    PWMCycleCounter, W
        call    JumpToAnimFrame
_AnimBMO_Loop:
        call    FetchAnimFrame
        call    LoadLEDOCRs
        call    PerformPCPWMCycle
        btfsc   PORT_REGBIT_FR(UPB_MODE_BUTTON)
        bra     _AnimBMO_ButtonPress
        btfss   PORT_REGBIT_FR(UPB_PAT_BUTTON)
        bra     _AnimBMO_Loop
_AnimBMO_ButtonPress:
        call    RegisterButtonPress
        btfsc   WREG, INPUT_BIT_MODE
        bra     _AnimBMO_ModeBtnPressed
        btfss   WREG, INPUT_BIT_PATTERN
        bra     _AnimBMO_Loop
_AnimBMO_PatBtnPressed:
        call    WaitForReleaseOfAllButtons
        call    AdvanceAnimCounters
        bra     _AnimBMO_Loop
_AnimBMO_ModeBtnPressed:
        call    WaitForReleaseOfAllButtons
        movf    GAState + GAS_FrameIx, W
        return


;------------------------------------------------------------------------------


AnimateCue_Page1:
; If a system cue is playing, animate a frame of that.
;
; System cues such as "end of list" are set up to play without disturbing
; the normal pattern to be played.

        movlp   0x00
        ; See if a system cue pattern is playing.
        btfss   ModeFlags, MF_BIT_CUE
        retlw   0
        call    FetchAnimFrame
        call    LoadLEDOCRs
        call    PerformPCPWMCycle
        call    AdvanceAnimCounters
        btfsc   GAState + GAS_AnimControlFlags, ACF_BIT_LOOP_COMPLETED
        bcf     ModeFlags, MF_BIT_CUE
        retlw   0


;------------------------------------------------------------------------------


AnimatePatAdjMode_Page1:
; Run the pattern editor and update the User Pattern Modification record.
;
; The indices indicating which pattern is to be displayed with UserPatMod
; applied are also updated.

        movlp   0

        movf    EffectivePatMod + PatMod_RampIxA, W
        movwf   UserPatMod + PatMod_RampIxA
        movf    EffectivePatMod + PatMod_RampIxB, W
        movwf   UserPatMod + PatMod_RampIxB
        movf    EffectivePatMod + PatMod_RampIxC, W
        movwf   UserPatMod + PatMod_RampIxC
        movf    EffectivePatMod + PatMod_TempoIx, W
        movwf   UserPatMod + PatMod_TempoIx
        movf    FavouriteIx, W
        movwf   ModFavouriteIx
        movf    BankIx, W
        movwf   ModBankIx
        movf    PatternIx, W
        movwf   ModPatternIx
        clrf    PatAdjItemIx
        bcf     ModeFlags, MF_BIT_CUE

        incfsz  FavouriteIx, W
        call    GetFavourite
        call    GetPatternFromBank_Tramp0
        call    LoadPattern
        pagesel AnalysePaletteUse_Page1
        call    AnalysePaletteUse_Page1
        movlp   0
        call    UpdateEffectivePatMod
        call    ModifyPattern_Tramp0
        call    ForbidAllBlackPatMod_Tramp0

_APAM_Loop:
        btfsc   PORT_REGBIT_FR(UPB_MODE_BUTTON)
        bra     _APAM_ButtonPress
        btfss   PORT_REGBIT_FR(UPB_PAT_BUTTON)
        bra     _APAM_SkipButtonPress
_APAM_ButtonPress:
        bcf     ModeFlags, MF_BIT_CUE
        call    RegisterButtonPress
        btfsc   WREG, INPUT_BIT_MODE
        bra     _APAM_ModeBtnPressed
        btfss   WREG, INPUT_BIT_PATTERN
        bra     _APAM_SkipButtonPress

_APAM_PatBtnPressed:
        call    WaitForReleaseOfAllButtons
        ; Increment the tempo index or a ramp index of UserPatMod.
        movlw   LOW(UserPatMod)
        movwf   FSR0L
        movlw   HIGH(UserPatMod)
        movwf   FSR0H
        movf    PatAdjItemIx, W
        andlw   3
        btfsc   ModeFlags, MF_BIT_RAMP16
        andlw   1
        addwf   FSR0L, F
        movlw   0
        addwfc  FSR0H, F
        incf    INDF0, F
        movlw   NUM_RAMPS
        btfsc   ModeFlags, MF_BIT_RAMP16
        movlw   NUM_RAMP16_PALETTES
        movf    PatAdjItemIx, F
        btfsc   STATUS, Z
        movlw   8
        subwf   INDF0, W
        btfsc   STATUS, C
        clrf    INDF0
        bra     _APAM_ShowValue

_APAM_ModeBtnPressed:
        movlw   SYSPAT_LPMENU_PAM_MODE
        call    AnimateLongPress
        lsrf    WREG, W
        btfsc   STATUS, Z
        bra     _APAM_NextItemToAdjust
        decfsz  WREG, W
        bra    _APAM_SignalShutdown
        bra     _APAM_Done
_APAM_NextItemToAdjust:
        btfss   ModeFlags, MF_BIT_EXTERNALRAMPS
        bra     _APAM_NextItem_SkipR16
        btfss   ModeFlags, MF_BIT_RAMP16
        bra     _APAM_NextItem_SkipR16
        incf    PatAdjItemIx, F
        movlw   1
        andwf   PatAdjItemIx, F
        bra     _APAM_NextItem_Selected
_APAM_NextItem_SkipR16:
        movf    PaletteUsageMetrics + PUM_NumUserRamps, W
        subwf   PatAdjItemIx, W
        btfsc   STATUS, C
        bra     _APAM_NextItem_BackToTempo
        incf    PatAdjItemIx, F
        movlw   3
        btfss   ModeFlags, MF_BIT_EXTERNALRAMPS
_APAM_NextItem_BackToTempo:
        clrw
        andwf   PatAdjItemIx, F
_APAM_NextItem_Selected:
        ; Fall through to _APAM_ShowValue

_APAM_ShowValue:
        btfss   ModeFlags, MF_BIT_EXTERNALRAMPS
        bra     _APAM_ShowV_UserRampsOrTempo
        btfss   ModeFlags, MF_BIT_RAMP16
        bra     _APAM_ShowV_UserRampsOrTempo
        movf    PatAdjItemIx, W
        btfsc   STATUS, Z
        bra     _APAM_ShowV_UserRampsOrTempo
_APAM_ShowV_Ramp16:
        movlw   SYSPAT_PAM_RAMP16IX_CUE
        call    GetEnumeratedSysPatAddr
        call    LoadPattern
        call    UpdateEffectivePatMod
        call    ModifyPattern_Tramp0
        bra     _APAM_ShowValue_HaveCue
_APAM_ShowV_UserRampsOrTempo:
        clrf    Scratch2
        btfsc   ModeFlags, MF_BIT_RAMP16
        bsf     Scratch2, 0
        bcf     ModeFlags, MF_BIT_RAMP16
        movlw   SYSPAT_PAM_VALUE_CUE
        call    GetEnumeratedSysPatAddr
        call    LoadPattern
        call    UpdateEffectivePatMod
        call    ModifyPattern_Tramp0
        btfsc   Scratch2, 0
        bsf     ModeFlags, MF_BIT_RAMP16
        movf    PatAdjItemIx, W
        btfsc   STATUS, Z
        bra     _APAM_ShowValue_TempoIx
_APAM_ShowValue_RampIx:
        addlw   8
        bra     _APAM_ShowValue_HaveFrIx
_APAM_ShowValue_TempoIx:
        movf    UserPatMod + PatMod_TempoIx, W
        andlw   7
_APAM_ShowValue_HaveFrIx:
        call    JumpToAnimFrame
_APAM_ShowValue_HaveCue:
        bsf     ModeFlags, MF_BIT_CUE
        movlw   EDITOR_CUE_TIME_PWM_CYCLES
        movwf   Scratch2
_APAM_ShowValue_Done:

_APAM_SkipButtonPress:

        btfsc   ModeFlags, MF_BIT_CUE
        bra     _APAM_AnimCue
        ; Normal pattern, currently being modified
        call    FetchAnimFrame
        call    LoadLEDOCRs
        call    PerformPCPWMCycle
        call    AdvanceAnimCounters
        bra     _APAM_Loop
_APAM_AnimCue:
        call    FetchAnimFrame
        call    LoadLEDOCRs
        call    PerformPCPWMCycle
        decfsz  Scratch2, F
        bra     _APAM_Loop
        bcf     ModeFlags, MF_BIT_CUE
        ; Fall through to _APAM_Loop_LoadPat

_APAM_Loop_LoadPat:
        incfsz  FavouriteIx, W
        call    GetFavourite
        call    GetPatternFromBank_Tramp0
        call    LoadPattern
        call    UpdateEffectivePatMod
        call    ModifyPattern_Tramp0
        call    ForbidAllBlackPatMod_Tramp0
        bra     _APAM_Loop

_APAM_Done:
        bcf     ModeFlags, MF_BIT_CUE
        retlw   0

_APAM_SignalShutdown:
        bcf     ModeFlags, MF_BIT_CUE
        retlw   1


;------------------------------------------------------------------------------


AnalysePaletteUse_Page1:
; Examine the colour indices used by a pattern.
;
; The highest numbered user ramp that can be accessed by the pattern editor
; is determined by the highest colour index in the pattern that falls within
; the range of indices spaned by the user ramps.
;
; In: GAState loaded with a pattern
; Out: PaletteUsageMetrics.PUM_IsPaletted
;      PaletteUsageMetrics.PUM_HighestPalIx
;      PaletteUsageMetrics.PUM_NumUserRamps

        clrf    PaletteUsageMetrics + PUM_IsPaletted
        clrf    PaletteUsageMetrics + PUM_HighestPalIx
        clrf    PaletteUsageMetrics + PUM_NumUserRamps
        movf    GAState + GAS_ExpPatPtr + 0, W
        movwf   FSR0L
        movf    GAState + GAS_ExpPatPtr + 1, W
        movwf   FSR0H
        movf    GAState + GAS_NumFrames, W
        btfsc   STATUS, Z
        retlw   0
        movwf   LoopCtr0
_APU_Loop:
        movf    GAState + GAS_AnimControlFlags, W
        andlw   ACF_FORMAT_MASK
        brw
        bra     _APU_Invalid
        bra     _APU_Invalid
        bra     _APU_256c
        bra     _APU_16c
        bra     _APU_4c
        bra     _APU_Invalid
        bra     _APU_Invalid
        bra     _APU_Invalid
_APU_256c:
        moviw   FSR0++  ; Skip the frame period byte.
        moviw   FSR0++
        call    _APU_Accumulate
        moviw   FSR0++
        call    _APU_Accumulate
        moviw   FSR0++
        call    _APU_Accumulate
        bra     _APU_Next
_APU_16c:
        moviw   FSR0++
        andlw   0x0F
        call    _APU_Accumulate
        swapf   INDF0, W
        andlw   0x0F
        call    _APU_Accumulate
        moviw   FSR0++
        andlw   0x0F
        call    _APU_Accumulate
        bra     _APU_Next
_APU_4c:
        swapf   INDF0, W
        andlw   0x03
        call    _APU_Accumulate
        lsrf    INDF0, W
        lsrf    WREG, F
        andlw   0x03
        call    _APU_Accumulate
        moviw   FSR0++
        andlw   0x03
        call    _APU_Accumulate
        bra     _APU_Next
_APU_Invalid:
        movf    GAState + GAS_FrameRecordStride, W
        addwf   FSR0L, F
        movlw   0
        addwfc  FSR0H, F
_APU_Next:
        decfsz  LoopCtr0, F
        bra     _APU_Loop
        btfss   PaletteUsageMetrics + PUM_IsPaletted, 0
        retlw   0
        movf    PaletteUsageMetrics + PUM_NumUserRamps, W
        sublw   8 - 1
        btfss   STATUS, C
        clrw
        sublw   8 - 1
        call    _APU_NRampsFromHUIx
        movwf   PaletteUsageMetrics + PUM_NumUserRamps
        return
_APU_NRampsFromHUIx:
        brw
        retlw   1  ;0 (Background is a part of Ramp A)
        retlw   1  ;1
        retlw   1  ;2
        retlw   1  ;3
        retlw   2  ;4 (Beginning of Ramp B)
        retlw   2  ;5
        retlw   2  ;6
        retlw   3  ;7 (Beginning of Ramp C)
_APU_Accumulate:
        bsf     PaletteUsageMetrics + PUM_IsPaletted, 0
        movwf   Scratch0
        subwf   PaletteUsageMetrics + PUM_HighestPalIx, W
        btfsc   STATUS, C
        bra     _APU_Acc_SkipNewHighPalIx
        movf    Scratch0, W
        movwf   PaletteUsageMetrics + PUM_HighestPalIx
_APU_Acc_SkipNewHighPalIx:
        btfss   ModeFlags, MF_BIT_EXTERNALRAMPS
        retlw   0
        movf    Scratch0, W
        sublw   10 - 1
        btfss   STATUS, C
        bra     _APU_Acc_NoNewHighURPalIx
        ; A colour index in range of the user colour ramps!
        movf    Scratch0, W
        subwf   PaletteUsageMetrics + PUM_NumUserRamps, W
        btfsc   STATUS, C
        bra     _APU_Acc_NoNewHighURPalIx
        movf    Scratch0, W
        movwf   PaletteUsageMetrics + PUM_NumUserRamps
_APU_Acc_NoNewHighURPalIx:
        retlw   0


;------------------------------------------------------------------------------
; Palette for system cues and menus
;------------------------------------------------------------------------------


BAPM_Pal_System:
  resetenum
  bablock
  enumdat SYS_BLK,    dbargb, 0x000000  ; Black
  enumdat SYS_DGRY,   dbargb, 0x0B0808  ; Dark Grey
  enumdat SYS_RED,    dbargb, 0x2F0000  ; Red
  enumdat SYS_BLZ,    dbargb, 0x440E00  ; Orange
  enumdat SYS_YEL,    dbargb, 0x321400  ; Bogan Yellow
  enumdat SYS_GRN,    dbargb, 0x22CC00  ; Bright Green
  enumdat SYS_SGR,    dbargb, 0x009C2C  ; Sea Green
  enumdat SYS_BLU,    dbargb, 0x000035  ; Blue
  enumdat SYS_MAG,    dbargb, 0x770061  ; Magenta
  enumdat SYS_XWHT,   dbargb, 0xFFFFFF  ; Full White
  enumdat SYS_XRED,   dbargb, 0xFF0000  ; Full Red
  enumdat SYS_GRN1,   dbargb, 0x005000  ; Medium Green
  enumdat SYS_VIO,    dbargb, 0x1F0038  ; Violet
  enumdat SYS_SKY,    dbargb, 0x00132F  ; Sky Blue
  enumdat SYS_SGR1,   dbargb, 0x00340D  ; Medium Sea Green
  enumdat SYS_IIX4,   dbargb, 0x0C0C0C  ; Test of Intensity Index 4
  enumdat SYS_IIX3,   dbargb, 0x191919  ; Test of Intensity Index 3
  enumdat SYS_IIX2,   dbargb, 0x333333  ; Test of Intensity Index 2
  enumdat SYS_IIX1,   dbargb, 0x666666  ; Test of Intensity Index 1
  enumdat SYS_IIX0,   dbargb, 0xCCCCCC  ; Test of Intensity Index 5
  endbab
SYS_PALETTE_LENGTH equ ENUMIX


;------------------------------------------------------------------------------
; System error patterns
;------------------------------------------------------------------------------


PM_Frs_Err_Flash1LED:
        bablock
        pf4c      1,   1,   0,   0
        pf4c      1,   0,   0,   0
        endbab

PM_Map_SpuriousError:
        bablock
        dba2b    0, SYS_GRN
        endbab
PM_Pat_SpuriousError:
        pattern PATDF_4C, 2, 100, PATSF_BAPM_PMF, 2
        dw  BAPM_Pal_System, PM_Map_SpuriousError, PM_Frs_Err_Flash1LED

PM_Map_Err_InvalidFormat:
        bablock
        dba2b    0, SYS_RED
        endbab
PM_Pat_Err_InvalidFormat:
        pattern PATDF_4C, 2, 100, PATSF_BAPM_PMF, 2
        dw  BAPM_Pal_System, PM_Map_Err_InvalidFormat, PM_Frs_Err_Flash1LED

PM_Map_Err_NullPalette:
        bablock
        dba2b    0, SYS_YEL
        endbab
PM_Pat_Err_NullPalette:
        pattern PATDF_4C, 2, 100, PATSF_BAPM_PMF, 2
        dw  BAPM_Pal_System, PM_Map_Err_NullPalette, PM_Frs_Err_Flash1LED

PM_Map_Err_PatternTooLarge:
        bablock
        dba2b    0, SYS_MAG
        endbab
PM_Pat_Err_PatternTooLarge:
        pattern PATDF_4C, 2, 100, PATSF_BAPM_PMF, 2
        dw  BAPM_Pal_System, PM_Map_Err_PatternTooLarge, PM_Frs_Err_Flash1LED


;------------------------------------------------------------------------------
; System cues and menus
;------------------------------------------------------------------------------


; The long press cue animations are animated by AnimateLongPress, which
; modifies the animation depending on the values of the frame periods.

PM_Pat_LPMenu_PowerOn:
        pattern PATDF_256C, SYS_PALETTE_LENGTH, 10, PATSF_BAPM_PF, 12
        dw  BAPM_Pal_System, 0, 0
        bablock
        ; Frames with odd period lengths are flickery.
        ; On
        pf256c   36, SYS_DGRY, SYS_DGRY, SYS_DGRY
        pf256c   15, SYS_DGRY, SYS_DGRY, SYS_DGRY
        ; Choose intensity
        pf256c   36, SYS_BLU,  SYS_BLU,  SYS_BLU
        pf256c   13, SYS_BLU,  SYS_BLU,  SYS_BLU
        ; Choose Last Used Pattern mode
        pf256c   36, SYS_YEL,  SYS_YEL,  SYS_YEL
        pf256c   13, SYS_YEL,  SYS_YEL,  SYS_YEL
        ; Choose Restrictions
        pf256c   36, SYS_RED,  SYS_RED,  SYS_RED
        pf256c   13, SYS_RED,  SYS_RED,  SYS_RED
        ; Off
        pf256c    2, SYS_XRED, SYS_XRED, SYS_XRED
        pf256c    2, SYS_RED,  SYS_XRED, SYS_RED
        pf256c    2,    0,     SYS_RED,     0
        pf256c    2,    0,        0,        0
        endbab

PM_Pat_LPMenu_Mode:
        pattern PATDF_256C, SYS_PALETTE_LENGTH, 10, PATSF_BAPM_PF, 8
        dw  BAPM_Pal_System, 0, 0
        bablock
        ; Frames with odd period lengths are flickery.
        ; Next Mode
        pf256c   16,    0,        0,        0
        pf256c    9, SYS_DGRY, SYS_DGRY, SYS_DGRY
        ; Jump to Favourites
        pf256c   16, SYS_BLU,  SYS_SGR1, SYS_BLU
        pf256c    9, SYS_BLU,  SYS_SGR1, SYS_BLU
        ; Off
        pf256c    2, SYS_XRED, SYS_XRED, SYS_XRED
        pf256c    2, SYS_RED,  SYS_XRED, SYS_RED
        pf256c    2,    0,     SYS_RED,     0
        pf256c    2,    0,        0,        0
        endbab

PM_Pat_LPMenu_SaveFav:
        pattern PATDF_256C, SYS_PALETTE_LENGTH, 10, PATSF_BAPM_PF, 11
        dw  BAPM_Pal_System, 0, 0
        bablock
        ; Frames with odd period lengths are flickery.
        ; Next Pattern
        pf256c   24,    0,        0,        0
        pf256c   13, SYS_DGRY, SYS_DGRY, SYS_DGRY
        ; Begin Slideshow
        pf256c   16, SYS_YEL,     0,        0
        pf256c    1, SYS_YEL,     0,        0
        pf256c   16, SYS_YEL,  SYS_YEL,     0
        pf256c    1, SYS_YEL,  SYS_YEL,     0
        pf256c   40, SYS_YEL,  SYS_YEL,  SYS_YEL
        pf256c   13, SYS_YEL,  SYS_YEL,  SYS_YEL
        ; Save to Favourites
        pf256c   30, SYS_SGR,  SYS_SGR,  SYS_SGR
        pf256c   13, SYS_SGR,  SYS_SGR,  SYS_SGR
        ; Abort
        pf256c    1,    0,     SYS_DGRY,    0
        endbab

PM_Pat_LPMenu_FavsFull:
        pattern PATDF_256C, SYS_PALETTE_LENGTH, 10, PATSF_BAPM_PF, 12
        dw  BAPM_Pal_System, 0, 0
        bablock
        ; Frames with odd period lengths are flickery.
        ; Next Pattern
        pf256c   24,    0,        0,        0
        pf256c   13, SYS_DGRY, SYS_DGRY, SYS_DGRY
        ; Begin Slideshow
        pf256c   16, SYS_YEL,     0,        0
        pf256c    1, SYS_YEL,     0,        0
        pf256c   16, SYS_YEL,  SYS_YEL,     0
        pf256c    1, SYS_YEL,  SYS_YEL,     0
        pf256c   40, SYS_YEL,  SYS_YEL,  SYS_YEL
        pf256c   13, SYS_YEL,  SYS_YEL,  SYS_YEL
        ; Favourites list is fall
        pf256c    4, SYS_SGR,  SYS_SGR,  SYS_SGR
        pf256c    2, SYS_YEL,  SYS_YEL,  SYS_YEL
        pf256c    2,    0,     SYS_BLZ,     0
        pf256c    1,    0,     SYS_MAG,     0
        endbab

PM_Pat_LPMenu_MoveOrDelFav:
        pattern PATDF_256C, SYS_PALETTE_LENGTH, 10, PATSF_BAPM_PF, 13
        dw  BAPM_Pal_System, 0, 0
        bablock
        ; Frames with odd period lengths are flickery.
        ; Next Favourite
        pf256c   24,    0,        0,        0
        pf256c   13, SYS_DGRY, SYS_DGRY, SYS_DGRY
        ; Begin Slideshow
        pf256c   16, SYS_YEL,     0,        0
        pf256c    1, SYS_YEL,     0,        0
        pf256c   16, SYS_YEL,  SYS_YEL,     0
        pf256c    1, SYS_YEL,  SYS_YEL,     0
        pf256c   16, SYS_YEL,  SYS_YEL,  SYS_YEL
        pf256c   13, SYS_YEL,  SYS_YEL,  SYS_YEL
        ; Promote
        pf256c   60, SYS_GRN,  SYS_GRN,  SYS_GRN
        pf256c   13, SYS_GRN,  SYS_GRN,  SYS_GRN
        ; Delete
        pf256c   30, SYS_RED,  SYS_RED,  SYS_RED
        pf256c   13, SYS_RED,  SYS_RED,  SYS_RED
        ; Abort
        pf256c    1,    0,     SYS_DGRY,    0
        endbab

PM_Pat_LPMenu_SlideshowOnly:
        pattern PATDF_256C, SYS_PALETTE_LENGTH, 10, PATSF_BAPM_PF, 9
        dw  BAPM_Pal_System, 0, 0
        bablock
        ; Frames with odd period lengths are flickery.
        ; Next Pattern
        pf256c   24,    0,        0,        0
        pf256c   13, SYS_DGRY, SYS_DGRY, SYS_DGRY
        ; Begin Slideshow
        pf256c   16, SYS_YEL,     0,        0
        pf256c    1, SYS_YEL,     0,        0
        pf256c   16, SYS_YEL,  SYS_YEL,     0
        pf256c    1, SYS_YEL,  SYS_YEL,     0
        pf256c   16, SYS_YEL,  SYS_YEL,  SYS_YEL
        pf256c   13, SYS_YEL,  SYS_YEL,  SYS_YEL
        ; Abort
        pf256c    1,    0,     SYS_DGRY,    0
        endbab

PM_Pat_LPMenu_StopSlideshow:
        pattern PATDF_256C, SYS_PALETTE_LENGTH, 10, PATSF_BAPM_PF, 7
        dw  BAPM_Pal_System, 0, 0
        bablock
        ; Frames with odd period lengths are flickery.
        ; Previous Pattern
        pf256c   16,    0,        0,        0
        pf256c   13, SYS_YEL,  SYS_YEL,  SYS_YEL
        ; Stop Slideshow
        pf256c   40, SYS_BLU,  SYS_BLU,  SYS_BLU
        pf256c   13, SYS_BLU,  SYS_BLU,  SYS_BLU
        ; Modify pattern
        pf256c   40, SYS_MAG,  SYS_MAG,  SYS_MAG
        pf256c   13, SYS_MAG,  SYS_MAG,  SYS_MAG
        ; Stop Slideshow
        pf256c    1, SYS_BLU,  SYS_BLU,  SYS_BLU
        endbab

PM_Pat_LPMenu_StopSlideshowOnly:
        pattern PATDF_256C, SYS_PALETTE_LENGTH, 10, PATSF_BAPM_PF, 3
        dw  BAPM_Pal_System, 0, 0
        bablock
        ; Frames with odd period lengths are flickery.
        ; Previous Pattern
        pf256c   16,    0,        0,        0
        pf256c   13, SYS_YEL,  SYS_YEL,  SYS_YEL
        ; Stop Slideshow
        pf256c    1, SYS_BLU,  SYS_BLU,  SYS_BLU
        endbab

PM_Pat_LPMenu_PatAdjMode_Mode:
        pattern PATDF_256C, SYS_PALETTE_LENGTH, 10, PATSF_BAPM_PF, 8
        dw  BAPM_Pal_System, 0, 0
        bablock
        ; Frames with odd period lengths are flickery.
        ; Next item to adjust
        pf256c   16,    0,        0,        0
        pf256c    9, SYS_DGRY, SYS_DGRY, SYS_DGRY
        ; Exit adjustment mode, keeping changes.
        pf256c   60, SYS_YEL,  SYS_YEL,  SYS_YEL
        pf256c   21, SYS_YEL,  SYS_YEL,  SYS_YEL
        ; Off
        pf256c    2, SYS_XRED, SYS_XRED, SYS_XRED
        pf256c    2, SYS_RED,  SYS_XRED, SYS_RED
        pf256c    2,    0,     SYS_RED,     0
        pf256c    2,    0,        0,        0
        endbab

; The submenus are manually advanced with button presses.

PM_Map_Menu_Intensity:
        bablock
        dba6b     0, SYS_IIX4, SYS_IIX3, SYS_IIX2, SYS_IIX1, SYS_IIX0
        endbab
PM_Pat_Menu_Intensity:
        pattern PATDF_16C, 6, 1, PATSF_BAPM_PMF, 5
        dw  BAPM_Pal_System, PM_Map_Menu_Intensity, 0
        bablock
        pf16c    1,   1,   0,   0
        pf16c    1,   2,   2,   0
        pf16c    1,   0,   3,   0
        pf16c    1,   0,   4,   4
        pf16c    1,   0,   0,   5
        endbab

PM_Map_Menu_LockLUP:
        bablock
        dba4b    0, SYS_RED, SYS_YEL, SYS_SGR
        endbab
PM_Pat_Menu_LockLUP:
        pattern PATDF_4C, 4, 1, PATSF_BAPM_PMF, 2
        dw  BAPM_Pal_System, PM_Map_Menu_LockLUP, 0
        bablock
        pf4c     1,   0,   3,   3
        pf4c     1,   1,   1,   0
        endbab

PM_Map_Menu_Restrictions:
        bablock
        dba5b    0, SYS_RED, SYS_YEL, SYS_SKY, SYS_GRN1
        endbab
PM_Pat_Menu_Restrictions:
        pattern PATDF_16C, 5, 1, PATSF_BAPM_PMF, 4
        dw  BAPM_Pal_System, PM_Map_Menu_Restrictions, 0
        bablock
        pf16c    1,   4,   4,   4
        pf16c    1,   3,   3,   3
        pf16c    1,   2,   2,   0
        pf16c    1,   1,   0,   0
        endbab

; UI feedback cues play for a short moment before the user-selected
; pattern begins. (The cues can be interrupted at any time.)

PM_Map_Cue_Wrap:
        bablock
        dba3b    0, SYS_DGRY, 8
        endbab
PM_Pat_Cue_Wrap:
        pattern PATDF_4C, 2, 12, PATSF_BAPM_PMF, 5
        dw  BAPM_Pal_System, PM_Map_Cue_Wrap, 0
        bablock
        pf4c     2,   0,   0,   0
        pf4c     1,   2,   2,   2
        pf4c     1,   1,   2,   1
        pf4c     1,   0,   1,   0
        pf4c     4,   0,   0,   0
        endbab

PM_Frs_Cue_EnterStdsOrFavs:
        bablock
        pf4c     1,   1,   1,   1
        pf4c     1,   0,   0,   0
        pf4c     1,   1,   1,   1
        pf4c     4,   0,   0,   0
        endbab

PM_Map_Cue_EnterFavs:
        bablock
        dba2b    0, SYS_SGR
        endbab
PM_Pat_Cue_EnterFavs:
        pattern PATDF_4C, 2, 25, PATSF_BAPM_PMF, 4
        dw  BAPM_Pal_System
        dw  PM_Map_Cue_EnterFavs
        dw  PM_Frs_Cue_EnterStdsOrFavs

PM_Map_Cue_EnterStds:
        bablock
        dba2b    0, SYS_BLU
        endbab
PM_Pat_Cue_EnterStds:
        pattern PATDF_4C, 2, 25, PATSF_BAPM_PMF, 4
        dw  BAPM_Pal_System
        dw  PM_Map_Cue_EnterStds
        dw  PM_Frs_Cue_EnterStdsOrFavs

BAPM_Map_PAM_ValueCue:
        bablock
        dba10b  BLK, RED, RED1, RED2, GRN, GRN1, GRN2, BLU, BLU1, BLU2
        dba4b   BLK, BLU2, ORA, SYS_SKY
        endbab
BAPM_Frs_PAM_ValueCue:
        bablock
        pf16c     1,  11, 11, 12
        pf16c     1,  11, 12, 11
        pf16c     1,  11, 12, 12
        pf16c     1,  12, 11, 11
        pf16c     1,  12, 11, 12
        pf16c     1,  12, 12, 11
        pf16c     1,  12, 12, 12
        pf16c     1,  13, 13, 13
        pf16c     1,   0,  0,  0
        pf16c     1,   1,  2,  3
        pf16c     1,   4,  5,  6
        pf16c     1,   7,  8,  9
        endbab
PM_Pat_PAM_ValueCue:
        pattern PATDF_16C, 14, 10, PATSF_BAPM_PMF, 12
        dw BAPM_Pal_Basic, BAPM_Map_PAM_ValueCue, BAPM_Frs_PAM_ValueCue

PM_Pat_PAM_Ramp16IxCue:
        pattern PATDF_16C, 16, 1, PATSF_BAPM_PMF, 1
        dw BAPM_Pal_Ramp16_Flame, 0, 0
        bablock
        pf16c     1,  15, 11,  7
        endbab


;------------------------------------------------------------------------------
; System pattern addresses, enumerated
;------------------------------------------------------------------------------


PM_SysPatTable:
  resetenum
  enumdat SYSPAT_SPURIOUS_ERROR,          dw, PM_Pat_SpuriousError
  enumdat SYSPAT_ERR_INVALID_FORMAT,      dw, PM_Pat_Err_InvalidFormat
  enumdat SYSPAT_ERR_NULL_PALETTE,        dw, PM_Pat_Err_NullPalette
  enumdat SYSPAT_ERR_PATTERN_TOO_LARGE,   dw, PM_Pat_Err_PatternTooLarge
  enumdat SYSPAT_LPMENU_POWER_ON,         dw, PM_Pat_LPMenu_PowerOn
  enumdat SYSPAT_LPMENU_MODE,             dw, PM_Pat_LPMenu_Mode
  enumdat SYSPAT_LPMENU_SAVE_FAV,         dw, PM_Pat_LPMenu_SaveFav
  enumdat SYSPAT_LPMENU_FAVS_FULL,        dw, PM_Pat_LPMenu_FavsFull
  enumdat SYSPAT_LPMENU_MOVE_OR_DEL_FAV,  dw, PM_Pat_LPMenu_MoveOrDelFav
  enumdat SYSPAT_LPMENU_SLIDESHOW_ONLY,   dw, PM_Pat_LPMenu_SlideshowOnly
  enumdat SYSPAT_LPMENU_STOP_SLIDESHOW,   dw, PM_Pat_LPMenu_StopSlideshow
  enumdat SYSPAT_LPMENU_STOP_SSHOW_ONLY,  dw, PM_Pat_LPMenu_StopSlideshowOnly
  enumdat SYSPAT_LPMENU_PAM_MODE,         dw, PM_Pat_LPMenu_PatAdjMode_Mode
  enumdat SYSPAT_MENU_INTENSITY,          dw, PM_Pat_Menu_Intensity
  enumdat SYSPAT_MENU_LOCK_LUP,           dw, PM_Pat_Menu_LockLUP
  enumdat SYSPAT_MENU_RESTRICTIONS,       dw, PM_Pat_Menu_Restrictions
  enumdat SYSPAT_CUE_WRAP,                dw, PM_Pat_Cue_Wrap
  enumdat SYSPAT_CUE_ENTER_FAVS,          dw, PM_Pat_Cue_EnterFavs
  enumdat SYSPAT_CUE_ENTER_STDS,          dw, PM_Pat_Cue_EnterStds
  enumdat SYSPAT_PAM_VALUE_CUE,           dw, PM_Pat_PAM_ValueCue
  enumdat SYSPAT_PAM_RAMP16IX_CUE,        dw, PM_Pat_PAM_Ramp16IxCue


;------------------------------------------------------------------------------
; Basic palette, enumerated
;------------------------------------------------------------------------------


BAPM_Pal_Basic:
  resetenum
  bablock
  ; Greyscale
  enumdat BLK,     dbargb, 0x000000  ;  0: Black
  enumdat WHT2,    dbargb, 0x121514  ;  1: Grey
  enumdat WHT1,    dbargb, 0x3F4845  ;  2: Silver
  enumdat WHT,     dbargb, 0xB5CEC5  ;  3: Standard white
  enumdat XEN,     dbargb, 0xCADEE4  ;  4: Xenon flash
  ; Main colours
  enumdat RED,     dbargb, 0xCC0000  ;  5: Red
  enumdat BLZ,     dbargb, 0xC81200  ;  6: Blaze Orange
  enumdat ORA,     dbargb, 0xE02D00  ;  7: Orange
  enumdat YEL,     dbargb, 0xEE6A00  ;  8: Yellow
  enumdat SNT,     dbargb, 0x559900  ;  9: Snot
  enumdat GRN,     dbargb, 0x00BB00  ; 10: Green
  enumdat SGR,     dbargb, 0x00AC2C  ; 11: Sea Green
  enumdat CYN,     dbargb, 0x008899  ; 12: Cyan
  enumdat SKY,     dbargb, 0x003799  ; 13: Sky Blue
  enumdat BLU,     dbargb, 0x0000BB  ; 14: Blue
  enumdat PUR,     dbargb, 0x4800CC  ; 15: Purple
  enumdat VIO,     dbargb, 0x7000C4  ; 16: Violet
  enumdat MAG,     dbargb, 0xAA0090  ; 17: Magenta
  enumdat CER,     dbargb, 0xD2002C  ; 18: Cerise
  enumdat PNK,     dbargb, 0xCC2235  ; 19: Pink
  ; Medium intensity colours
  enumdat RED1,    dbargb, 0x720000  ; 20: Medium Red
  enumdat BLZ1,    dbargb, 0x700A00  ; 21: Medium Blaze Orange
  enumdat ORA1,    dbargb, 0x7D1900  ; 22: Medium Orange
  enumdat YEL1,    dbargb, 0x853B00  ; 23: Medium Yellow
  enumdat SNT1,    dbargb, 0x305600  ; 24: Medium Snot
  enumdat GRN1,    dbargb, 0x006900  ; 25: Medium Green
  enumdat SGR1,    dbargb, 0x006019  ; 26: Medium Sea Green
  enumdat CYN1,    dbargb, 0x004C56  ; 27: Medium Cyan
  enumdat SKY1,    dbargb, 0x001F56  ; 28: Medium Sky Blue
  enumdat BLU1,    dbargb, 0x000069  ; 29: Medium Blue
  enumdat PUR1,    dbargb, 0x280072  ; 30: Medium Purple
  enumdat VIO1,    dbargb, 0x3F006E  ; 31: Medium Violet
  enumdat MAG1,    dbargb, 0x5F0051  ; 32: Medium Magenta
  enumdat CER1,    dbargb, 0x760019  ; 33: Medium Cerise
  enumdat PNK1,    dbargb, 0x72131E  ; 34: Medium Pink
  ; Low intensity colours
  enumdat RED2,    dbargb, 0x100000  ; 35: Dark Red
  enumdat BLZ2,    dbargb, 0x100100  ; 36: Dark Blaze Orange
  enumdat ORA2,    dbargb, 0x120400  ; 37: Dark Orange
  enumdat YEL2,    dbargb, 0x130800  ; 38: Dark Yellow
  enumdat SNT2,    dbargb, 0x070C00  ; 39: Dark Snot
  enumdat GRN2,    dbargb, 0x000F00  ; 40: Dark Green
  enumdat SGR2,    dbargb, 0x000E04  ; 41: Dark Sea Green
  enumdat CYN2,    dbargb, 0x000B0C  ; 42: Dark Cyan
  enumdat SKY2,    dbargb, 0x00040C  ; 43: Dark Sky Blue
  enumdat BLU2,    dbargb, 0x00000F  ; 44: Dark Blue
  enumdat PUR2,    dbargb, 0x060010  ; 45: Dark Purple
  enumdat VIO2,    dbargb, 0x090010  ; 46: Dark Violet
  enumdat MAG2,    dbargb, 0x0E000C  ; 47: Dark Magenta
  enumdat CER2,    dbargb, 0x110004  ; 48: Dark Cerise
  enumdat PNK2,    dbargb, 0x100304  ; 49: Dark Pink
  ; Special colours, including dark ones which risk rounding to black
  enumdat FLA,     dbargb, 0xCC3F09  ; 50: : Flame centre
  enumdat FLA1,    dbargb, 0x701602  ; 51: : Flame periphery
  enumdat FLA2,    dbargb, 0x150300  ; 52: : Flame tip
  enumdat FLA3,    dbargb, 0x050100  ; 53: : Flame background
  enumdat BLP1,    dbargb, 0x0C006B  ; 54: : Medium Blurple
  enumdat PSG,     dbargb, 0x2CCC48  ; 55: : Pale Sea Green
  enumdat RED3,    dbargb, 0x040000  ; 56: : Darkest Red
  enumdat ORA3,    dbargb, 0x040100  ; 57: : Darkest Orange
  enumdat GRN3,    dbargb, 0x000400  ; 58: : Darkest Green
  enumdat BLU3,    dbargb, 0x000004  ; 59: : Darkest Blue
  enumdat VIO3,    dbargb, 0x030005  ; 59: : Darkest Blue
  enumdat PUR3,    dbargb, 0x020005  ; 60: : Darkest Purple
  endbab
BASIC_PALETTE_LENGTH equ ENUMIX


;------------------------------------------------------------------------------
; Colour ramps (palette mappings to be applied to suitable patterns)
;------------------------------------------------------------------------------


#define RAMP4(c0, c1, c2, c3) ((c3)<<24)|((c2)<<16)|((c1)<<8)|(c0)

BAPM_ColourRamps:
  resetenum
  bablock
  ; Plain colours
  enumdat R_RED,        dbal, RAMP4(0, RED,  RED1, RED2)
  enumdat R_BLZ,        dbal, RAMP4(0, BLZ,  BLZ1, BLZ2)
  enumdat R_ORA,        dbal, RAMP4(0, ORA,  ORA1, ORA2)
  enumdat R_YEL,        dbal, RAMP4(0, YEL,  YEL1, YEL2)
  enumdat R_SNT,        dbal, RAMP4(0, SNT,  SNT1, SNT2)
  enumdat R_GRN,        dbal, RAMP4(0, GRN,  GRN1, GRN2)
  enumdat R_SGR,        dbal, RAMP4(0, SGR,  SGR1, SGR2)
  enumdat R_CYN,        dbal, RAMP4(0, CYN,  CYN1, CYN2)
  enumdat R_SKY,        dbal, RAMP4(0, SKY,  SKY1, SKY2)
  enumdat R_BLU,        dbal, RAMP4(0, BLU,  BLU1, BLU2)
  enumdat R_PUR,        dbal, RAMP4(0, PUR,  PUR1, PUR2)
  enumdat R_VIO,        dbal, RAMP4(0, VIO,  VIO1, VIO2)
  enumdat R_MAG,        dbal, RAMP4(0, MAG,  MAG1, MAG2)
  enumdat R_CER,        dbal, RAMP4(0, CER,  CER1, CER2)
  enumdat R_PNK,        dbal, RAMP4(0, PNK,  PNK1, PNK2)
  enumdat R_WHT,        dbal, RAMP4(0, WHT,  WHT1, WHT2)
  enumdat R_BLK,        dbal, RAMP4(0, BLK,  BLK,  BLK)
  enumdat R_RED1,       dbal, RAMP4(0, RED1, RED2, RED3)
  enumdat R_ORA1,       dbal, RAMP4(0, ORA1, ORA2, ORA3)
  enumdat R_GRN1,       dbal, RAMP4(0, GRN1, GRN2, GRN3)
  enumdat R_BLU1,       dbal, RAMP4(0, BLU1, BLU2, BLU3)
  enumdat R_VIO1,       dbal, RAMP4(0, VIO1, VIO2, VIO3)
  enumdat R_PUR1,       dbal, RAMP4(0, PUR1, PUR2, PUR3)
  ; Chromatic aberrations
  enumdat R_FIRE,       dbal, RAMP4(0, ORA,  BLZ1, RED2)
  enumdat R_ICE,        dbal, RAMP4(0, CYN,  SKY1, BLU2)
  enumdat R_BLURPLE,    dbal, RAMP4(0, PUR,  BLP1, BLU2)
  enumdat R_GOLD,       dbal, RAMP4(0, YEL,  ORA1, BLZ2)
  enumdat R_MINT,       dbal, RAMP4(0, PSG,  SGR1, GRN2)
  enumdat R_BUNSEN,     dbal, RAMP4(0, SKY,  BLU1, PUR2)
  enumdat R_FLAME,      dbal, RAMP4(0, FLA,  FLA1, FLA2)
  enumdat R_STRAWBRY,   dbal, RAMP4(0, PNK,  CER1, RED2)
  ; Non-black backgrounds
  enumdat R_BUNSEN_E,   dbal, RAMP4(PUR3, SKY,  BLU1, PUR2)
  enumdat R_FLAME_E,    dbal, RAMP4(FLA3, FLA,  FLA1, FLA2)
  enumdat R_PURHAZE_A,  dbal, RAMP4(MAG2, ORA,  BLZ,  CER1)
  enumdat R_PURHAZE_B,  dbal, RAMP4(MAG2, BLU,  PUR1, VIO1)
  enumdat R_TIEDYED_A,  dbal, RAMP4(BLZ2, SKY,  YEL,  CER1)
  enumdat R_TIEDYED_B,  dbal, RAMP4(BLZ2, BLZ,  GRN1, BLU1)
  enumdat R_RED_MIST,   dbal, RAMP4(RED3, BLZ,  RED,  RED2)
  endbab
NUM_RAMPS equ ENUMIX
  if NUM_RAMPS > 64
    error "Too many colour ramps! (64 is the absolute maximum.)"
    ; If NUM_BANKS = 16, NUM_RAMPS must be at most 63.
  endif


;------------------------------------------------------------------------------
; Ramp16 palettes
;------------------------------------------------------------------------------


BAPM_Pal_Ramp16_Flame:
; Realistic, slightly pastel orange flame
        bablock
        dbargb  0x000000
        dbargb  0x010000
        dbargb  0x010000
        dbargb  0x030000
        dbargb  0x060000
        dbargb  0x080000
        dbargb  0x0A0000
        dbargb  0x100100
        dbargb  0x210400
        dbargb  0x370800
        dbargb  0x4F0D01
        dbargb  0x691402
        dbargb  0x871F03
        dbargb  0xA32B05
        dbargb  0xBA3607
        dbargb  0xCC3F09
        endbab

BAPM_Pal_Ramp16_Blaze:
; Based on the standard Fire colour ramp
        bablock
        dbargb  0x000000
        dbargb  0x010000
        dbargb  0x010000
        dbargb  0x020000
        dbargb  0x050000
        dbargb  0x050000
        dbargb  0x040000
        dbargb  0x090000
        dbargb  0x1A0100
        dbargb  0x310200
        dbargb  0x490400
        dbargb  0x660800
        dbargb  0x891100
        dbargb  0xAC1B00
        dbargb  0xCA2500
        dbargb  0xE02D00
        endbab

BAPM_Pal_Ramp16_RedFire:
; Reddish fire based on Red Mist
        bablock
        dbargb  0x000000
        dbargb  0x010000
        dbargb  0x010000
        dbargb  0x020000
        dbargb  0x060000
        dbargb  0x090000
        dbargb  0x0B0000
        dbargb  0x130000
        dbargb  0x2D0000
        dbargb  0x500100
        dbargb  0x760200
        dbargb  0x980400
        dbargb  0xB50900
        dbargb  0xC70F00
        dbargb  0xD11500
        dbargb  0xDA1A00
        endbab

BAPM_Pal_Ramp16_Flurple:
; Blurple flame
        bablock
        dbargb  0x000000
        dbargb  0x000001
        dbargb  0x000001
        dbargb  0x000002
        dbargb  0x000005
        dbargb  0x000007
        dbargb  0x000009
        dbargb  0x00000F
        dbargb  0x01001F
        dbargb  0x020035
        dbargb  0x03004A
        dbargb  0x0A0064
        dbargb  0x1A0083
        dbargb  0x3000A0
        dbargb  0x4500B9
        dbargb  0x5500CC
        endbab

BAPM_Pal_Ramp16_Bunsen:
        bablock
        dbargb  0x000000
        dbargb  0x000001
        dbargb  0x000001
        dbargb  0x010003
        dbargb  0x010006
        dbargb  0x020008
        dbargb  0x03000A
        dbargb  0x030010
        dbargb  0x030121
        dbargb  0x02013A
        dbargb  0x010153
        dbargb  0x00066A
        dbargb  0x00117F
        dbargb  0x00208C
        dbargb  0x002F93
        dbargb  0x003A99
        endbab

BAPM_Pal_Ramp16_Strawberry:
        bablock
        dbargb  0x000000
        dbargb  0x010000
        dbargb  0x020000
        dbargb  0x030000
        dbargb  0x070000
        dbargb  0x0A0000
        dbargb  0x0B0000
        dbargb  0x120000
        dbargb  0x250002
        dbargb  0x400006
        dbargb  0x5C000B
        dbargb  0x790412
        dbargb  0x960E1E
        dbargb  0xAD1C2C
        dbargb  0xBE2939
        dbargb  0xCC3342
        endbab

BAPM_Pal_Ramp16_Acrid:
        bablock
        dbargb  0x000000
        dbargb  0x000100
        dbargb  0x000100
        dbargb  0x000200
        dbargb  0x000500
        dbargb  0x010800
        dbargb  0x010A00
        dbargb  0x040F00
        dbargb  0x0C1B00
        dbargb  0x152900
        dbargb  0x1F3900
        dbargb  0x2E4800
        dbargb  0x455800
        dbargb  0x5F6500
        dbargb  0x766F00
        dbargb  0x887700
        endbab


;------------------------------------------------------------------------------
; Ramp16 palette addresses, enumerated
;------------------------------------------------------------------------------


PM_Ramp16AddrTable:
  resetenum
  enumdat R16_FLAME,      dw, BAPM_Pal_Ramp16_Flame
  enumdat R16_BLAZE,      dw, BAPM_Pal_Ramp16_Blaze
  enumdat R16_RED_FIRE,   dw, BAPM_Pal_Ramp16_RedFire
  enumdat R16_FLURPLE,    dw, BAPM_Pal_Ramp16_Flurple
  enumdat R16_BUNSEN,     dw, BAPM_Pal_Ramp16_Bunsen
  enumdat R16_STRAWBERRY, dw, BAPM_Pal_Ramp16_Strawberry
  enumdat R16_ACRID,      dw, BAPM_Pal_Ramp16_Acrid
NUM_RAMP16_PALETTES equ ENUMIX


;------------------------------------------------------------------------------
; Patterns
;------------------------------------------------------------------------------


BAPM_Default_Ramps:
        bablock
        dba1b   BLK
        dba3b   ORA, BLZ1, RED2
        dba3b   CYN, SKY1, BLU2
        dba3b   WHT, WHT1, WHT2
        dba1b   WHT
DEF_RL equ BAPM_BYTE_OFFSET
        endbab

BAPM_Map_Medley6_UXXW:
        bablock
        dba1b   BLK
        dba3b   GRN, (GRN1), (GRN2)
        dba3b   CER, (CER1), (CER2)
        dba3b   WHT, (WHT1), (WHT2)
        dba4b   YEL, SKY, RED, SGR
        endbab

BAPM_Map_Medley7_UXXX:
        bablock
        dba1b   BLK
        dba3b   GRN, (GRN1), (GRN2)
        dba3b   CER, (CER1), (CER2)
        dba3b   YEL, (YEL1), (YEL2)
        dba4b   SKY, RED, SGR, BLU
        endbab

BAPM_Map_Medley5X_UXXX:
        bablock
        dba1b   BLK
        dba3b   GRN, (GRN1), (GRN2)
        dba3b   RED, (RED1), (RED2)
        dba3b   SKY, (SKY1), (SKY2)
        dba4b   YEL, CER, (SGR), XEN
        endbab

; Triangular rainbow fade pattern (balanced)
;
; Each LED is lerped from primary colour to primary colour so that the
; secondary colours have half the intensity they would have if the
; usual hexagonal rainbow fade animation were used.
;
;       R     G     B     R     G     B     R
; Max    \    /\    /\    /\    /\    /\    /\
;         \  /  \  /  \  /  \  /  \  /  \  /  \  /
;           /     /     /     /     /     /     /   . . .
;          /     /     /     /     /     /     /
;         /  \  /  \  /  \  /  \  /  \  /  \  /  \
; Off    ____ \____ \____ \____ \____ \____ \____
;
;       |     |     |     |     |     |     |     |
;        Trine Trine Trine Trine Trine Trine Trine  . . .
;          0     1     2     0     1     2     0

PM_Pat_BalRainbow:
        pattern PATDF_24B, 0, 1, PATSF_BAPM_F, 4
        dw  0, 0, 0
        bablock
        pf24b     1, 0, 0x0000FF, 0x00FF00, 0xFF0000
        pf24b   255, 1, 0x0100FF, 0x00FF01, 0xFF0100
        pf24b   255, 1, 0xFF0100, 0x0100FF, 0x00FF01
        pf24b   254, 1, 0x00FF01, 0xFF0100, 0x0100FF
        endbab

PM_Pat_FastBalRainbow:
        pattern PATDF_24B, 0, 1, PATSF_BAPM_F, 4
        dw  0, 0, 0
        bablock
        pf24b     1, 0, 0x0000FF, 0x00FF00, 0xFF0000
        pf24b     5, 1, 0x3300CD, 0x00CD33, 0xCD3300
        pf24b     5, 1, 0xCD3300, 0x3300CD, 0x00CD33
        pf24b     4, 1, 0x00CD33, 0xCD3300, 0x3300CD
        endbab

;PM_Pat_CMYRainbow:
;        pattern PATDF_24B, 0, 2, PATSF_BAPM_F, 4
;        dw  0, 0, 0
;        bablock
;        pf24b     1, 0, 0x00AAAA, 0xAA00AA, 0xAAAA00
;        pf24b   160, 1, 0x01FF00, 0x0001FF, 0xFF0001
;        pf24b   160, 1, 0x0001FF, 0xFF0001, 0x01FF00
;        pf24b   159, 1, 0xFF0001, 0x01FF00, 0x0001FF
;        endbab

PM_Pat_LagRainbow:
        pattern PATDF_24B, 0, 4, PATSF_BAPM_F, 10
        dw  0, 0, 0
        bablock
        pf24b     1, 0, 0x55AA00, 0xAA5500, 0xFF0000
        pf24b    85, 1, 0xFF0100, 0xFF0100, 0xFF0100
        pf24b    85, 1, 0x00FF01, 0xFF0100, 0xFF0100
        pf24b    85, 1, 0x00FF01, 0x00FF01, 0xFF0100
        pf24b    85, 1, 0x00FF01, 0x00FF01, 0x00FF01
        pf24b    85, 1, 0x0100FF, 0x00FF01, 0x00FF01
        pf24b    85, 1, 0x0100FF, 0x0100FF, 0x00FF01
        pf24b    85, 1, 0x0100FF, 0x0100FF, 0x0100FF
        pf24b    85, 1, 0xFF0100, 0x0100FF, 0x0100FF
        pf24b    84, 1, 0xFF0100, 0xFF0100, 0x0100FF
        endbab

PM_Pat_OutRainbow:
        pattern PATDF_24B, 0, 4, PATSF_BAPM_F, 7
        dw  0, 0, 0
        bablock
        pf24b     1, 0, 0xFF0000, 0xAA5500, 0xFF0000
        pf24b   170, 1, 0xFF0100, 0xFF0100, 0xFF0100
        pf24b    85, 1, 0xFF0100, 0x00FF01, 0xFF0100
        pf24b   170, 1, 0x00FF01, 0x00FF01, 0x00FF01
        pf24b    85, 1, 0x00FF01, 0x0100FF, 0x00FF01
        pf24b   170, 1, 0x0100FF, 0x0100FF, 0x0100FF
        pf24b    84, 1, 0x0100FF, 0xFF0100, 0x0100FF
        endbab

PM_Pat_UniRainbow:
        pattern PATDF_24B, 0, 16, PATSF_BAPM_F, 5
        dw  0, 0, 0
        bablock
        pf24b     1, 0, 0xCC0033, 0xCC0033, 0xCC0033
        pf24b    51, 1, 0x0100FF, 0x0100FF, 0x0100FF
        pf24b   255, 1, 0xFF0100, 0xFF0100, 0xFF0100
        pf24b   255, 1, 0x00FF01, 0x00FF01, 0x00FF01
        pf24b   203, 1, 0x0100FF, 0x0100FF, 0x0100FF
        endbab

PM_Pat_FastUniRainbow:
        pattern PATDF_24B, 0, 4, PATSF_BAPM_F, 4
        dw  0, 0, 0
        bablock
        pf24b     1, 0, 0xFF0000, 0xFF0000, 0xFF0000
        pf24b     5, 1, 0xCD3300, 0xCD3300, 0xCD3300
        pf24b     5, 1, 0x00CD33, 0x00CD33, 0x00CD33
        pf24b     4, 1, 0x3300CD, 0x3300CD, 0x3300CD
        endbab

;PM_Pat_HexRainbow:
;        pattern PATDF_24B, 0, 3, PATSF_BAPM_F, 7
;        dw  0, 0, 0
;        bablock
;        pf24b     1, 0, 0xCC0000, 0x00CC00, 0x0000CC
;        pf24b   204, 1, 0x000100, 0x000001, 0x010000
;        pf24b   204, 1, 0xFF0000, 0x00FF00, 0x0000FF
;        pf24b   204, 1, 0x000001, 0x010000, 0x000100
;        pf24b   204, 1, 0x00FF00, 0x0000FF, 0xFF0000
;        pf24b   204, 1, 0x010000, 0x000100, 0x000001
;        pf24b   203, 1, 0x0000FF, 0xFF0000, 0x00FF00
;        endbab

PM_Pat_Firefly:
        pattern PATDF_24B, 0, 1, PATSF_BAPM_F, 7
        dw  0, 0, 0
        bablock
        pf24b     1, 0, 0x10DD00, 0x40FF20, 0x10DD00
        pf24b     8, 1, 0xFEFC00, 0xF8FCFC, 0xFEFC00
        ;               0x00BD00, 0x00DF00, 0x00BD00
        pf24b    24, 1, 0x00FB00, 0x00FC00, 0x00FB00
        ;               0x004500, 0x007F00, 0x004500
        pf24b    24, 1, 0x00FE00, 0x00FD00, 0x00FE00
        ;               0x001500, 0x003700, 0x001500
        pf24b    21, 1, 0x00FF00, 0x00FE00, 0x00FF00
        ;               0x000000, 0x000D00, 0x000000
        pf24b    13, 1, 0x000000, 0x00FF00, 0x000000
        ;               0x000000, 0x000100, 0x000000
        pf24b     2, 1, 0x000000, 0x000000, 0x000000
        endbab

PM_Pat_IceStorm:
        pattern PATDF_24B, 0, 1, PATSF_BAPM_F, 9
        dw  0, 0, 0
        bablock
        pf24b     1, 0, 0x00FAFA, 0x000000, 0x006F6F
        pf24b    27, 1, 0x00FDFD, 0x000000, 0x00FEFE
        ;               0x00A9A9, 0x000000, 0x003939
        pf24b    28, 1, 0x00FEFE, 0x000000, 0x00FEFE
        ;               0x007171, 0x000000, 0x000101
        pf24b     1, 0, 0x006F6F, 0x00FAFA, 0x000000
        pf24b    27, 1, 0x00FEFE, 0x00FDFD, 0x000000
        ;               0x003939, 0x00A9A9, 0x000000
        pf24b    28, 1, 0x00FEFE, 0x00FEFE, 0x000000
        ;               0x000101, 0x007171, 0x000000
        pf24b     1, 0, 0x000000, 0x006F6F, 0x00FAFA
        pf24b    27, 1, 0x000000, 0x00FEFE, 0x00FDFD
        ;               0x000000, 0x003939, 0x00A9A9
        pf24b    28, 1, 0x000000, 0x00FEFE, 0x00FEFE
        ;               0x000000, 0x000101, 0x007171

        endbab


PM_Pat_Solid:
        pattern PATDF_4C, 4, 1, PATSF_BAPM_PMF, 1
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf4c      1,   1,  1,  1
        endbab

PM_Pat_SolidSteps:
        pattern PATDF_16C, 16, 224, PATSF_BAPM_PMF, 16
        dw BAPM_Pal_Basic, BAPM_Map_SolidSteps, 0
        bablock
        pf16c    10,   0,  0,  0
        pf16c    10,   1,  1,  1
        pf16c    10,   2,  2,  2
        pf16c    10,   3,  3,  3
        pf16c    10,   4,  4,  4
        pf16c    10,   5,  5,  5
        pf16c    10,   6,  6,  6
        pf16c    10,   7,  7,  7
        pf16c    10,   8,  8,  8
        pf16c    10,   9,  9,  9
        pf16c    10,  10, 10, 10
        pf16c    10,  11, 11, 11
        pf16c    10,  12, 12, 12
        pf16c    10,  13, 13, 13
        pf16c    10,  14, 14, 14
        pf16c    10,  15, 15, 15
        endbab
BAPM_Map_SolidSteps:
        bablock
        dba6b   RED, SGR, PUR, BLZ, CYN, VIO
        dba6b   GRN, ORA, BLU, MAG, SNT, SKY
        dba4b   CER, YEL, PNK, WHT
        endbab

PM_Pat_HaveYouSeenMyPoi:
        pattern PATDF_4C, 4, 1, PATSF_BAPM_PMF, 2
        dw BAPM_Pal_Basic, PM_Map_HaveYouSeenMyPoi, 0
        bablock
        pf4c      1,   0,  2,  0
        pf4c      1,   0,  0,  0
        endbab
PM_Map_HaveYouSeenMyPoi:
        ; In the darker ramps, the darkest colour may appear black when the
        ; global intensity is set low. It is important to never allow a fully
        ; black display. This pattern is used by the system to indicate that
        ; the colour ramp selected would made the poi appear to be off. It is
        ; also available to the user to make the poi easy to find in the dark.
        bablock
        dba4b   BLK, PNK1, PNK2, (RED2)
        endbab

PM_Pat_Flash1:
        pattern PATDF_16C, DEF_RL, 4, PATSF_BAPM_PMF, 2
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_Flashy
PM_Pat_Flash2:
        pattern PATDF_16C, DEF_RL, 4, PATSF_BAPM_PMF, 4
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_Flashy
PM_Pat_Flash3:
        pattern PATDF_16C, DEF_RL, 4, PATSF_BAPM_PMF, 6
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_Flashy
BAPM_Frs_Flashy:
        bablock
        pf16c     1,   1,  1,  1
        pf16c     1,   0,  0,  0
        pf16c     1,   4,  4,  4
        pf16c     1,   0,  0,  0
        pf16c     1,   7,  7,  7
        pf16c     1,   0,  0,  0
        endbab

PM_Pat_Alternate:
        pattern PATDF_16C, DEF_RL, 3, PATSF_BAPM_PMF, 2
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   1,  1,  1
        pf16c     1,   4,  4,  4
        endbab

PM_Pat_AltSpaced2:
        pattern PATDF_16C, DEF_RL, 3, PATSF_BAPM_PMF, 4
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, PM_Frs_AltSpaced
PM_Pat_AltSpaced3:
        pattern PATDF_16C, DEF_RL, 3, PATSF_BAPM_PMF, 6
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, PM_Frs_AltSpaced
PM_Frs_AltSpaced:
        bablock
        pf16c     3,   1,  1,  1
        pf16c     1,   0,  0,  0
        pf16c     3,   4,  4,  4
        pf16c     1,   0,  0,  0
        pf16c     3,   7,  7,  7
        pf16c     1,   0,  0,  0
        endbab

PM_Pat_JammyBattenberg:
        pattern PATDF_16C, DEF_RL, 12, PATSF_BAPM_PMF, 2
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   1,  7,  4
        pf16c     1,   4,  7,  1
        endbab

PM_Pat_RimWtdPacked1:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 1
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_RimWtdPacked
PM_Pat_RimWtdPacked2:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 2
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_RimWtdPacked
PM_Pat_RimWtdPacked3:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 3
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_RimWtdPacked
PM_Pat_RimWtdSpaced3:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 4
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_RimWtdPacked
BAPM_Frs_RimWtdPacked:
        bablock
        pf16c     1,   3,  2,  1
        pf16c     1,   6,  5,  4
        pf16c     1,   9,  8,  7
        pf16c     1,   0,  0,  0
        endbab

PM_Pat_HubWtdPacked1:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 1
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_HubWtdPacked
PM_Pat_HubWtdPacked2:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 2
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_HubWtdPacked
PM_Pat_HubWtdPacked3:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 3
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_HubWtdPacked
PM_Pat_HubWtdSpaced3:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 4
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_HubWtdPacked
BAPM_Frs_HubWtdPacked:
        bablock
        pf16c     1,   1,  2,  3
        pf16c     1,   4,  5,  6
        pf16c     1,   7,  8,  9
        pf16c     1,   0,  0,  0
        endbab

PM_Pat_RimWtdFlash1:
        pattern PATDF_16C, DEF_RL, 4, PATSF_BAPM_PMF, 2
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_RimWtdFlash
PM_Pat_RimWtdFlash2:
        pattern PATDF_16C, DEF_RL, 4, PATSF_BAPM_PMF, 4
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_RimWtdFlash
PM_Pat_RimWtdFlash3:
        pattern PATDF_16C, DEF_RL, 4, PATSF_BAPM_PMF, 6
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_RimWtdFlash
BAPM_Frs_RimWtdFlash:
        bablock
        pf16c     1,   3,  2,  1
        pf16c     1,   0,  0,  0
        pf16c     1,   6,  5,  4
        pf16c     1,   0,  0,  0
        pf16c     1,   9,  8,  7
        pf16c     1,   0,  0,  0
        endbab

PM_Pat_HubWtdFlash1:
        pattern PATDF_16C, DEF_RL, 4, PATSF_BAPM_PMF, 2
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_HubWtdFlash
PM_Pat_HubWtdFlash2:
        pattern PATDF_16C, DEF_RL, 4, PATSF_BAPM_PMF, 4
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_HubWtdFlash
PM_Pat_HubWtdFlash3:
        pattern PATDF_16C, DEF_RL, 4, PATSF_BAPM_PMF, 6
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_HubWtdFlash
BAPM_Frs_HubWtdFlash:
        bablock
        pf16c     1,   1,  2,  3
        pf16c     1,   0,  0,  0
        pf16c     1,   4,  5,  6
        pf16c     1,   0,  0,  0
        pf16c     1,   7,  8,  9
        pf16c     1,   0,  0,  0
        endbab

PM_Pat_SoftFlash2:
        pattern PATDF_16C, DEF_RL, 2, PATSF_BAPM_PMF, 12
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_SoftFlash
PM_Pat_SoftFlash3:
        pattern PATDF_16C, DEF_RL, 2, PATSF_BAPM_PMF, 18
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_SoftFlash
BAPM_Frs_SoftFlash:
        bablock
        pf16c     1,   3,  3,  3
        pf16c     1,   2,  2,  2
        pf16c     1,   1,  1,  1
        pf16c     1,   2,  2,  2
        pf16c     1,   3,  3,  3
        pf16c     1,   0,  0,  0
        pf16c     1,   6,  6,  6
        pf16c     1,   5,  5,  5
        pf16c     1,   4,  4,  4
        pf16c     1,   5,  5,  5
        pf16c     1,   6,  6,  6
        pf16c     1,   0,  0,  0
        pf16c     1,   9,  9,  9
        pf16c     1,   8,  8,  8
        pf16c     1,   7,  7,  7
        pf16c     1,   8,  8,  8
        pf16c     1,   9,  9,  9
        pf16c     1,   0,  0,  0
        endbab

PM_Pat_SoftFlash2X:
        pattern PATDF_16C, 14, 2, PATSF_BAPM_PMF, 12
        dw BAPM_Pal_Basic, BAPM_Map_Medley5X_UXXX, BAPM_Frs_SoftFlashX
PM_Pat_SoftFlash3X:
        pattern PATDF_16C, 14, 2, PATSF_BAPM_PMF, 18
        dw BAPM_Pal_Basic, BAPM_Map_Medley5X_UXXX, BAPM_Frs_SoftFlashX
BAPM_Frs_SoftFlashX:
        bablock
        pf16c     1,   3,  3,  3
        pf16c     1,   2,  2,  2
        pf16c     1,  13, 13, 13
        pf16c     1,   2,  2,  2
        pf16c     1,   3,  3,  3
        pf16c     1,   0,  0,  0
        pf16c     1,   6,  6,  6
        pf16c     1,   5,  5,  5
        pf16c     1,  13, 13, 13
        pf16c     1,   5,  5,  5
        pf16c     1,   6,  6,  6
        pf16c     1,   0,  0,  0
        pf16c     1,   9,  9,  9
        pf16c     1,   8,  8,  8
        pf16c     1,  13, 13, 13
        pf16c     1,   8,  8,  8
        pf16c     1,   9,  9,  9
        pf16c     1,   0,  0,  0
        endbab

PM_Pat_RampUp2:
        pattern PATDF_16C, DEF_RL, 2, PATSF_BAPM_PMF, 8
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_RampUp
PM_Pat_RampUp3:
        pattern PATDF_16C, DEF_RL, 2, PATSF_BAPM_PMF, 12
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_RampUp
BAPM_Frs_RampUp:
        bablock
        pf16c     1,   3,  3,  3
        pf16c     1,   2,  2,  2
        pf16c     1,   1,  1,  1
        pf16c     1,   0,  0,  0
        pf16c     1,   6,  6,  6
        pf16c     1,   5,  5,  5
        pf16c     1,   4,  4,  4
        pf16c     1,   0,  0,  0
        pf16c     1,   9,  9,  9
        pf16c     1,   8,  8,  8
        pf16c     1,   7,  7,  7
        pf16c     1,   0,  0,  0
        endbab

PM_Pat_RampDown2:
        pattern PATDF_16C, DEF_RL, 2, PATSF_BAPM_PMF, 8
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_RampDown
PM_Pat_RampDown3:
        pattern PATDF_16C, DEF_RL, 2, PATSF_BAPM_PMF, 12
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_RampDown
BAPM_Frs_RampDown:
        bablock
        pf16c     1,   1,  1,  1
        pf16c     1,   2,  2,  2
        pf16c     1,   3,  3,  3
        pf16c     1,   0,  0,  0
        pf16c     1,   4,  4,  4
        pf16c     1,   5,  5,  5
        pf16c     1,   6,  6,  6
        pf16c     1,   0,  0,  0
        pf16c     1,   7,  7,  7
        pf16c     1,   8,  8,  8
        pf16c     1,   9,  9,  9
        pf16c     1,   0,  0,  0
        endbab

PM_Pat_Dogs:
        pattern PATDF_16C, 14, 4, PATSF_BAPM_PMF, 5 * 2
        dw BAPM_Pal_Basic, BAPM_Map_Medley7_UXXX, BAPM_Frs_Dogs
PM_Pat_DogMedley:
        pattern PATDF_16C, 14, 4, PATSF_BAPM_PMF, 5 * 6
        dw BAPM_Pal_Basic, BAPM_Map_Medley7_UXXX, BAPM_Frs_Dogs
BAPM_Frs_Dogs:
        bablock
        pf16c     1,   1,  0,  0
        pf16c     1,   1,  1,  0
        pf16c     1,   1,  1,  1
        pf16c     1,   1,  1,  0
        pf16c     1,   1,  0,  0
        pf16c     1,   0,  0,  4
        pf16c     1,   0,  4,  4
        pf16c     1,   4,  4,  4
        pf16c     1,   0,  4,  4
        pf16c     1,   0,  0,  4
        pf16c     1,   7,  0,  0
        pf16c     1,   7,  7,  0
        pf16c     1,   7,  7,  7
        pf16c     1,   7,  7,  0
        pf16c     1,   7,  0,  0
        pf16c     1,   0,  0, 10
        pf16c     1,   0, 10, 10
        pf16c     1,  10, 10, 10
        pf16c     1,   0, 10, 10
        pf16c     1,   0,  0, 10
        pf16c     1,  11,  0,  0
        pf16c     1,  11, 11,  0
        pf16c     1,  11, 11, 11
        pf16c     1,  11, 11,  0
        pf16c     1,  11,  0,  0
        pf16c     1,   0,  0, 12
        pf16c     1,   0, 12, 12
        pf16c     1,  12, 12, 12
        pf16c     1,   0, 12, 12
        pf16c     1,   0,  0, 12
        endbab

PM_Pat_SoftDogs:
        pattern PATDF_16C, DEF_RL, 4, PATSF_BAPM_PMF, 10
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   3,  6,  5
        pf16c     1,   2,  3,  6
        pf16c     1,   1,  2,  3
        pf16c     1,   1,  1,  2
        pf16c     1,   1,  2,  3
        pf16c     1,   2,  3,  6
        pf16c     1,   3,  6,  5
        pf16c     1,   6,  5,  4
        pf16c     1,   5,  4,  4
        pf16c     1,   6,  5,  4
        endbab

PM_Pat_SoftDogMedley:
        pattern PATDF_256C, 19, 4, PATSF_BAPM_PMF, 30
        dw BAPM_Pal_Basic, BAPM_Map_SoftDogMedley, 0
        bablock
        pf256c    1,   3, 18, 17
        pf256c    1,   2,  3, 18
        pf256c    1,   1,  2,  3
        pf256c    1,   1,  1,  2
        pf256c    1,   1,  2,  3
        pf256c    1,   2,  3,  6
        pf256c    1,   3,  6,  5
        pf256c    1,   6,  5,  4
        pf256c    1,   5,  4,  4
        pf256c    1,   6,  5,  4
        pf256c    1,   9,  6,  5
        pf256c    1,   8,  9,  6
        pf256c    1,   7,  8,  9
        pf256c    1,   7,  7,  8
        pf256c    1,   7,  8,  9
        pf256c    1,   8,  9, 12
        pf256c    1,   9, 12, 11
        pf256c    1,  12, 11, 10
        pf256c    1,  11, 10, 10
        pf256c    1,  12, 11, 10
        pf256c    1,  15, 12, 11
        pf256c    1,  14, 15, 12
        pf256c    1,  13, 14, 15
        pf256c    1,  13, 13, 14
        pf256c    1,  13, 14, 15
        pf256c    1,  14, 15, 18
        pf256c    1,  15, 18, 17
        pf256c    1,  18, 17, 16
        pf256c    1,  17, 16, 16
        pf256c    1,  18, 17, 16
        endbab
BAPM_Map_SoftDogMedley:
        bablock
        dba1b   BLK
        dba3b   GRN,  GRN1, GRN2
        dba3b   CER,  CER1, CER2
        dba3b   YEL,  YEL1, YEL2
        dba3b   SKY,  SKY1, SKY2
        dba3b   RED,  RED1, RED2
        dba3b   SGR,  SGR1, SGR2
        endbab

PM_Pat_Squares_Fixed5:
        pattern PATDF_16C, 14, 4, PATSF_BAPM_PMF, 4 * 5
        dw BAPM_Pal_Basic, BAPM_Map_Medley5X_UXXX, BAPM_Frs_Squares
PM_Pat_Squares_User2:
        pattern PATDF_16C, DEF_RL, 4, PATSF_BAPM_PMF, 4 * 2
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_Squares
PM_Pat_Squares_User3:
        pattern PATDF_16C, DEF_RL, 4, PATSF_BAPM_PMF, 4 * 3
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_Squares
BAPM_Frs_Squares:
        bablock
        pf16c     1,   1,  1,  1
        pf16c     2,   1,  0,  1
        pf16c     1,   1,  1,  1
        pf16c     2,   0,  0,  0
        pf16c     1,   4,  4,  4
        pf16c     2,   4,  0,  4
        pf16c     1,   4,  4,  4
        pf16c     2,   0,  0,  0
        pf16c     1,   7,  7,  7
        pf16c     2,   7,  0,  7
        pf16c     1,   7,  7,  7
        pf16c     2,   0,  0,  0
        pf16c     1,  10, 10, 10
        pf16c     2,  10,  0, 10
        pf16c     1,  10, 10, 10
        pf16c     2,   0,  0,  0
        pf16c     1,  11, 11, 11
        pf16c     2,  11,  0, 11
        pf16c     1,  11, 11, 11
        pf16c     2,   0,  0,  0
        ;pf16c     1,  12, 12, 12
        ;pf16c     2,  12,  0, 12
        ;pf16c     1,  12, 12, 12
        ;pf16c     2,   0,  0,  0
        endbab

PM_Pat_QuantumSlalom_Fixed6:
        pattern PATDF_16C, 14, 2, PATSF_BAPM_PMF, 4 * 6
        dw BAPM_Pal_Basic, BAPM_Map_Medley6_UXXW, BAPM_Frs_QuantumSlalom
PM_Pat_QuantumSlalom_User2:
        pattern PATDF_16C, 14, 2, PATSF_BAPM_PMF, 4 * 2
        dw BAPM_Pal_Basic, BAPM_Map_Medley6_UXXW, BAPM_Frs_QuantumSlalom
BAPM_Frs_QuantumSlalom:
        bablock
        pf16c     1,   7,  7,  7
        pf16c     2,   7,  0,  0
        pf16c     2,   7,  0,  1
        pf16c     2,   7,  0,  0
        pf16c     1,   7,  7,  7
        pf16c     2,   0,  0,  7
        pf16c     2,   4,  0,  7
        pf16c     2,   0,  0,  7
        pf16c     1,   7,  7,  7
        pf16c     2,   7,  0,  0
        pf16c     2,   7,  0, 10
        pf16c     2,   7,  0,  0
        pf16c     1,   7,  7,  7
        pf16c     2,   0,  0,  7
        pf16c     2,  11,  0,  7
        pf16c     2,   0,  0,  7
        pf16c     1,   7,  7,  7
        pf16c     2,   7,  0,  0
        pf16c     2,   7,  0, 12
        pf16c     2,   7,  0,  0
        pf16c     1,   7,  7,  7
        pf16c     2,   0,  0,  7
        pf16c     2,  13,  0,  7
        pf16c     2,   0,  0,  7
        endbab

PM_Pat_Terminals_U2F4:
        pattern PATDF_16C, 14, 2, PATSF_BAPM_PMF, 24
        dw BAPM_Pal_Basic, BAPM_Map_Medley6_UXXW, BAPM_Frs_Terminals
PM_Pat_Terminals_User2:
        pattern PATDF_16C, 14, 2, PATSF_BAPM_PMF, 8
        dw BAPM_Pal_Basic, BAPM_Map_Medley6_UXXW, BAPM_Frs_Terminals
BAPM_Frs_Terminals:
        bablock
        pf16c     2,   0,  9,  1
        pf16c     1,   0,  9,  0
        pf16c     1,   0,  7,  0
        pf16c     1,   0,  9,  0
        pf16c     2,   4,  9,  0
        pf16c     1,   0,  9,  0
        pf16c     1,   0,  7,  0
        pf16c     1,   0,  9,  0
        pf16c     2,   0,  9, 10
        pf16c     1,   0,  9,  0
        pf16c     1,   0,  7,  0
        pf16c     1,   0,  9,  0
        pf16c     2,  11,  9,  0
        pf16c     1,   0,  9,  0
        pf16c     1,   0,  7,  0
        pf16c     1,   0,  9,  0
        pf16c     2,   0,  9, 12
        pf16c     1,   0,  9,  0
        pf16c     1,   0,  7,  0
        pf16c     1,   0,  9,  0
        pf16c     2,  13,  9,  0
        pf16c     1,   0,  9,  0
        pf16c     1,   0,  7,  0
        pf16c     1,   0,  9,  0
        endbab

PM_Pat_TriBounce:
        pattern PATDF_16C, DEF_RL, 2, PATSF_BAPM_PMF, 4
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   1,  0,  0
        pf16c     1,   0,  4,  0
        pf16c     1,   0,  0,  7
        pf16c     1,   0,  4,  0
        endbab

PM_Pat_TriBounceSpaced:
        pattern PATDF_16C, DEF_RL, 2, PATSF_BAPM_PMF, 8
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   1,  0,  0
        pf16c     1,   0,  0,  0
        pf16c     1,   0,  4,  0
        pf16c     1,   0,  0,  0
        pf16c     1,   0,  0,  7
        pf16c     1,   0,  0,  0
        pf16c     1,   0,  4,  0
        pf16c     1,   0,  0,  0
        endbab

PM_Pat_Crown:
        pattern PATDF_16C, DEF_RL, 2, PATSF_BAPM_PMF, 4
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   1,  0,  0
        pf16c     1,   4,  4,  0
        pf16c     1,   7,  7,  7
        pf16c     1,   4,  4,  0
        endbab

PM_Pat_Sunburst:
        pattern PATDF_16C, DEF_RL, 2, PATSF_BAPM_PMF, 8
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   1,  0,  0
        pf16c     1,   0,  0,  0
        pf16c     1,   4,  4,  0
        pf16c     1,   0,  0,  0
        pf16c     1,   7,  7,  7
        pf16c     1,   0,  0,  0
        pf16c     1,   4,  4,  0
        pf16c     1,   0,  0,  0
        endbab

PM_Pat_InterdigitationP:
        pattern PATDF_16C, DEF_RL, 2, PATSF_BAPM_PMF, 2
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   1,  2,  3
        pf16c     1,   6,  5,  4
        endbab

PM_Pat_InterdigitationS:
        pattern PATDF_16C, DEF_RL, 2, PATSF_BAPM_PMF, 4
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   1,  2,  3
        pf16c     1,   0,  0,  0
        pf16c     1,   6,  5,  4
        pf16c     1,   0,  0,  0
        endbab

PM_Pat_TrianglesNearMe_Fixed5:
  pattern PATDF_16C, 14, 4, PATSF_BAPM_PMF, 6 * 5
  dw BAPM_Pal_Basic, BAPM_Map_Medley5X_UXXX, BAPM_Frs_TrianglesNearMe
PM_Pat_TrianglesNearMe_User2:
  pattern PATDF_16C, 14, 4, PATSF_BAPM_PMF, 6 * 2
  dw BAPM_Pal_Basic, BAPM_Map_Medley5X_UXXX, BAPM_Frs_TrianglesNearMe
PM_Pat_TrianglesNearMe_User3:
  pattern PATDF_16C, 14, 4, PATSF_BAPM_PMF, 6 * 3
  dw BAPM_Pal_Basic, BAPM_Map_Medley5X_UXXX, BAPM_Frs_TrianglesNearMe
BAPM_Frs_TrianglesNearMe:
        bablock
        pf16c     1,   1,  0,  0
        pf16c     1,   1,  1,  0
        pf16c     1,   1,  1,  1
        pf16c     1,   1,  1,  0
        pf16c     1,   1,  0,  0
        pf16c     1,   0,  0,  0
        pf16c     1,   4,  0,  0
        pf16c     1,   4,  4,  0
        pf16c     1,   4,  4,  4
        pf16c     1,   4,  4,  0
        pf16c     1,   4,  0,  0
        pf16c     1,   0,  0,  0
        pf16c     1,   7,  0,  0
        pf16c     1,   7,  7,  0
        pf16c     1,   7,  7,  7
        pf16c     1,   7,  7,  0
        pf16c     1,   7,  0,  0
        pf16c     1,   0,  0,  0
        pf16c     1,  10,  0,  0
        pf16c     1,  10, 10,  0
        pf16c     1,  10, 10, 10
        pf16c     1,  10, 10,  0
        pf16c     1,  10,  0,  0
        pf16c     1,   0,  0,  0
        pf16c     1,  11,  0,  0
        pf16c     1,  11, 11,  0
        pf16c     1,  11, 11, 11
        pf16c     1,  11, 11,  0
        pf16c     1,  11,  0,  0
        pf16c     1,   0,  0,  0
        ;pf16c     1,  12,  0,  0
        ;pf16c     1,  12, 12,  0
        ;pf16c     1,  12, 12, 12
        ;pf16c     1,  12, 12,  0
        ;pf16c     1,  12,  0,  0
        ;pf16c     1,   0,  0,  0
        endbab

PM_Pat_ZigZag:
        pattern PATDF_16C, DEF_RL, 4, PATSF_BAPM_PMF, 4
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_ZigZagVs
PM_Pat_ConsummateVs:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 10
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_ZigZagVs
BAPM_Frs_ZigZagVs:
        bablock
        pf16c     1,   1,  0,  0
        pf16c     1,   0,  1,  0
        pf16c     1,   0,  0,  1
        pf16c     1,   0,  1,  0
        pf16c     1,   1,  0,  0
        pf16c     1,   0,  0,  4
        pf16c     1,   0,  4,  0
        pf16c     1,   4,  0,  0
        pf16c     1,   0,  4,  0
        pf16c     1,   0,  0,  4
        endbab

PM_Pat_Step:
        pattern PATDF_4C, 4, 3, PATSF_BAPM_PMF, 4
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf4c      3,   1,  0,  0
        pf4c      1,   1,  1,  1
        pf4c      3,   0,  0,  1
        pf4c      1,   1,  1,  1
        endbab

PM_Pat_Spokes:
        pattern PATDF_4C, 4, 4, PATSF_BAPM_PMF, 2
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf4c      1,   1,  1,  1
        pf4c      1,   1,  0,  0
        endbab

PM_Pat_Spokes2:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 6
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_Spokes2or3
PM_Pat_Spokes3:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 9
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_Spokes2or3
BAPM_Frs_Spokes2or3:
        bablock
        pf16c     1,   2,  0,  0
        pf16c     2,   1,  1,  1
        pf16c     1,   2,  0,  0
        pf16c     1,   5,  0,  0
        pf16c     2,   4,  4,  4
        pf16c     1,   5,  0,  0
        pf16c     1,   8,  0,  0
        pf16c     2,   7,  7,  7
        pf16c     1,   8,  0,  0
        endbab

PM_Pat_SpotsBars:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 8
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   1,  1,  1
        pf16c     1,   0,  0,  0
        pf16c     1,   0,  7,  0
        pf16c     1,   0,  0,  0
        pf16c     1,   4,  4,  4
        pf16c     1,   0,  0,  0
        pf16c     1,   0,  7,  0
        pf16c     1,   0,  0,  0
        endbab

PM_Pat_RadCon:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 8
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   1,  1,  1
        pf16c     2,   0,  0,  0
        pf16c     1,   1,  1,  1
        pf16c     2,   0,  0,  0
        pf16c     1,   1,  1,  1
        pf16c     2,   0,  0,  0
        pf16c     5,   4,  0,  4
        pf16c     2,   0,  0,  0
        endbab

PM_Pat_Twist:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 12
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     3,   1,  0,  4
        pf16c     1,   1,  0,  5
        pf16c     1,   2,  3,  6
        pf16c     1,   0,  1,  0
        pf16c     1,   6,  3,  2
        pf16c     1,   5,  0,  1
        pf16c     3,   4,  0,  1
        pf16c     1,   4,  0,  2
        pf16c     1,   5,  6,  3
        pf16c     1,   0,  4,  0
        pf16c     1,   3,  6,  5
        pf16c     1,   2,  0,  4
        endbab

PM_Pat_PiedCatNoses:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 16
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   1,  1,  1
        pf16c     2,   1,  0,  0
        pf16c     1,   1,  0,  6
        pf16c     2,   2,  0,  5
        pf16c     1,   3,  0,  4
        pf16c     1,   0,  0,  4
        pf16c     1,   4,  4,  4
        pf16c     2,   0,  0,  0
        pf16c     1,   1,  1,  1
        pf16c     1,   0,  0,  1
        pf16c     1,   6,  0,  1
        pf16c     2,   5,  0,  2
        pf16c     1,   4,  0,  3
        pf16c     1,   4,  0,  0
        pf16c     1,   4,  4,  4
        pf16c     2,   0,  0,  0
        endbab

PM_Pat_PolarCatNoses:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 18
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   1,  1,  1
        pf16c     2,   1,  0,  0
        pf16c     1,   1,  0,  6
        pf16c     1,   2,  0,  5
        pf16c     1,   2,  0,  5
        pf16c     1,   3,  0,  4
        pf16c     1,   0,  0,  4
        pf16c     1,   4,  4,  4
        pf16c     2,   0,  0,  0
        pf16c     1,   4,  4,  4
        pf16c     1,   0,  0,  4
        pf16c     1,   3,  0,  4
        pf16c     1,   2,  0,  5
        pf16c     1,   2,  0,  5
        pf16c     1,   1,  0,  6
        pf16c     1,   1,  0,  0
        pf16c     1,   1,  1,  1
        pf16c     2,   0,  0,  0
        endbab

PM_Pat_Chain:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 12
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     2,   1,  1,  1
        pf16c     1,   1,  0,  1
        pf16c     1,   1,  6,  1
        pf16c     1,   2,  5,  2
        pf16c     1,   3,  5,  3
        pf16c     1,   0,  4,  0
        pf16c     1,   0,  4,  0
        pf16c     1,   0,  4,  0
        pf16c     1,   3,  5,  3
        pf16c     1,   2,  5,  2
        pf16c     1,   1,  6,  1
        pf16c     1,   1,  0,  1
        endbab

PM_Pat_Barberpole2:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 4
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   1,  1,  4
        pf16c     1,   4,  1,  1
        pf16c     1,   4,  4,  1
        pf16c     1,   1,  4,  4
        endbab

PM_Pat_Barberpole3:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 6
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   1,  1,  7
        pf16c     1,   4,  1,  1
        pf16c     1,   4,  4,  1
        pf16c     1,   7,  4,  4
        pf16c     1,   7,  7,  4
        pf16c     1,   1,  7,  7
        endbab

PM_Pat_Barberpole3S:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 8
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   1,  1,  0
        pf16c     1,   4,  1,  1
        pf16c     1,   4,  4,  1
        pf16c     1,   7,  4,  4
        pf16c     1,   7,  7,  4
        pf16c     1,   0,  7,  7
        pf16c     1,   0,  0,  7
        pf16c     1,   1,  0,  0
        endbab

PM_Pat_FatBarberpole2:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 6
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   1,  1,  1
        pf16c     1,   4,  1,  1
        pf16c     1,   4,  4,  1
        pf16c     1,   4,  4,  4
        pf16c     1,   1,  4,  4
        pf16c     1,   1,  1,  4
        endbab

PM_Pat_FatBarberpole3:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 9
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   1,  1,  1
        pf16c     1,   4,  1,  1
        pf16c     1,   4,  4,  1
        pf16c     1,   4,  4,  4
        pf16c     1,   7,  4,  4
        pf16c     1,   7,  7,  4
        pf16c     1,   7,  7,  7
        pf16c     1,   1,  7,  7
        pf16c     1,   1,  1,  7
        endbab

PM_Pat_FatBarberpole3S:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 12
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   1,  1,  1
        pf16c     1,   4,  1,  1
        pf16c     1,   4,  4,  1
        pf16c     1,   4,  4,  4
        pf16c     1,   7,  4,  4
        pf16c     1,   7,  7,  4
        pf16c     1,   7,  7,  7
        pf16c     1,   0,  7,  7
        pf16c     1,   0,  0,  7
        pf16c     1,   0,  0,  0
        pf16c     1,   1,  0,  0
        pf16c     1,   1,  1,  0
        endbab

PM_Pat_SoftBarberpole2:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 18
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   1,  1,  0
        pf16c     1,   2,  1,  3
        pf16c     1,   3,  1,  2
        pf16c     1,   0,  1,  1
        pf16c     1,   6,  2,  1
        pf16c     1,   5,  3,  1
        pf16c     1,   4,  0,  1
        pf16c     1,   4,  6,  2
        pf16c     1,   4,  5,  3
        pf16c     1,   4,  4,  0
        pf16c     1,   5,  4,  6
        pf16c     1,   6,  4,  5
        pf16c     1,   0,  4,  4
        pf16c     1,   3,  5,  4
        pf16c     1,   2,  6,  4
        pf16c     1,   1,  0,  4
        pf16c     1,   1,  3,  5
        pf16c     1,   1,  2,  6
        endbab

PM_Pat_SoftBarberpole3:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 27
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   1,  1,  0
        pf16c     1,   2,  1,  3
        pf16c     1,   3,  1,  2
        pf16c     1,   0,  1,  1
        pf16c     1,   6,  2,  1
        pf16c     1,   5,  3,  1
        pf16c     1,   4,  0,  1
        pf16c     1,   4,  6,  2
        pf16c     1,   4,  5,  3
        pf16c     1,   4,  4,  0
        pf16c     1,   5,  4,  6
        pf16c     1,   6,  4,  5
        pf16c     1,   0,  4,  4
        pf16c     1,   9,  5,  4
        pf16c     1,   8,  6,  4
        pf16c     1,   7,  0,  4
        pf16c     1,   7,  9,  5
        pf16c     1,   7,  8,  6
        pf16c     1,   7,  7,  0
        pf16c     1,   8,  7,  9
        pf16c     1,   9,  7,  8
        pf16c     1,   0,  7,  7
        pf16c     1,   3,  8,  7
        pf16c     1,   2,  9,  7
        pf16c     1,   1,  0,  7
        pf16c     1,   1,  3,  8
        pf16c     1,   1,  2,  9
        endbab

PM_Pat_SoftBarberpole3S:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 36
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   1,  1,  0
        pf16c     1,   2,  1,  3
        pf16c     1,   3,  1,  2
        pf16c     1,   0,  1,  1
        pf16c     1,   6,  2,  1
        pf16c     1,   5,  3,  1
        pf16c     1,   4,  0,  1
        pf16c     1,   4,  6,  2
        pf16c     1,   4,  5,  3
        pf16c     1,   4,  4,  0
        pf16c     1,   5,  4,  6
        pf16c     1,   6,  4,  5
        pf16c     1,   0,  4,  4
        pf16c     1,   9,  5,  4
        pf16c     1,   8,  6,  4
        pf16c     1,   7,  0,  4
        pf16c     1,   7,  9,  5
        pf16c     1,   7,  8,  6
        pf16c     1,   7,  7,  0
        pf16c     1,   8,  7,  9
        pf16c     1,   9,  7,  8
        pf16c     1,   0,  7,  7
        pf16c     1,   0,  8,  7
        pf16c     1,   0,  9,  7
        pf16c     1,   0,  0,  7
        pf16c     1,   0,  0,  8
        pf16c     1,   0,  0,  9
        pf16c     1,   0,  0,  0
        pf16c     1,   0,  0,  0
        pf16c     1,   0,  0,  0
        pf16c     1,   0,  0,  0
        pf16c     1,   3,  0,  0
        pf16c     1,   2,  0,  0
        pf16c     1,   1,  0,  0
        pf16c     1,   1,  3,  0
        pf16c     1,   1,  2,  0
        endbab

PM_Pat_WavySpokes2:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 16
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_WavySpokes
PM_Pat_WavySpokes3:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 24
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_WavySpokes
BAPM_Frs_WavySpokes:
        bablock
        pf16c     1,   0,  3,  0
        pf16c     1,   0,  1,  0
        pf16c     1,   3,  1,  3
        pf16c     1,   1,  1,  1
        pf16c     1,   1,  3,  1
        pf16c     1,   1,  0,  1
        pf16c     1,   3,  0,  3
        pf16c     3,   0,  0,  0
        pf16c     1,   0,  6,  0
        pf16c     1,   0,  4,  0
        pf16c     1,   6,  4,  6
        pf16c     1,   4,  4,  4
        pf16c     1,   4,  6,  4
        pf16c     1,   4,  0,  4
        pf16c     1,   6,  0,  6
        pf16c     3,   0,  0,  0
        pf16c     1,   0,  9,  0
        pf16c     1,   0,  7,  0
        pf16c     1,   9,  7,  9
        pf16c     1,   7,  7,  7
        pf16c     1,   7,  9,  7
        pf16c     1,   7,  0,  7
        pf16c     1,   9,  0,  9
        pf16c     3,   0,  0,  0
        endbab

PM_Pat_Chevrons2:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 24
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_Chevrons
PM_Pat_Chevrons3:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 36
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_Chevrons
BAPM_Frs_Chevrons:
        bablock
        pf16c     1,   3,  0,  3
        pf16c     1,   2,  0,  2
        pf16c     1,   1,  0,  1
        pf16c     1,   1,  3,  1
        pf16c     1,   1,  2,  1
        pf16c     1,   1,  1,  1
        pf16c     1,   2,  1,  2
        pf16c     1,   3,  1,  3
        pf16c     1,   0,  1,  0
        pf16c     1,   0,  2,  0
        pf16c     1,   0,  3,  0
        pf16c     6,   0,  0,  0
        pf16c     1,   6,  0,  6
        pf16c     1,   5,  0,  5
        pf16c     1,   4,  0,  4
        pf16c     1,   4,  6,  4
        pf16c     1,   4,  5,  4
        pf16c     1,   4,  4,  4
        pf16c     1,   5,  4,  5
        pf16c     1,   6,  4,  6
        pf16c     1,   0,  4,  0
        pf16c     1,   0,  5,  0
        pf16c     1,   0,  6,  0
        pf16c     6,   0,  0,  0
        pf16c     1,   9,  0,  9
        pf16c     1,   8,  0,  8
        pf16c     1,   7,  0,  7
        pf16c     1,   7,  9,  7
        pf16c     1,   7,  8,  7
        pf16c     1,   7,  7,  7
        pf16c     1,   8,  7,  8
        pf16c     1,   9,  7,  9
        pf16c     1,   0,  7,  0
        pf16c     1,   0,  8,  0
        pf16c     1,   0,  9,  0
        pf16c     6,   0,  0,  0
        endbab

PM_Pat_Weave:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 6
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   3,  5,  9
        pf16c     1,   2,  6,  8
        pf16c     1,   1,  0,  7
        pf16c     1,   2,  6,  8
        pf16c     1,   3,  5,  9
        pf16c     1,   0,  4,  0
        endbab

PM_Pat_LumCrawl:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 32
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   1,  1,  1
        pf16c     1,   0,  0,  0
        pf16c     1,   4,  4,  4
        pf16c     1,   0,  0,  0
        pf16c     1,   2,  1,  1
        pf16c     1,   0,  0,  0
        pf16c     1,   5,  4,  4
        pf16c     1,   0,  0,  0
        pf16c     1,   3,  2,  1
        pf16c     1,   0,  0,  0
        pf16c     1,   6,  5,  4
        pf16c     1,   0,  0,  0
        pf16c     1,   3,  3,  2
        pf16c     1,   0,  0,  0
        pf16c     1,   6,  6,  5
        pf16c     1,   0,  0,  0
        pf16c     1,   3,  3,  3
        pf16c     1,   0,  0,  0
        pf16c     1,   6,  6,  6
        pf16c     1,   0,  0,  0
        pf16c     1,   2,  3,  3
        pf16c     1,   0,  0,  0
        pf16c     1,   5,  6,  6
        pf16c     1,   0,  0,  0
        pf16c     1,   1,  2,  3
        pf16c     1,   0,  0,  0
        pf16c     1,   4,  5,  6
        pf16c     1,   0,  0,  0
        pf16c     1,   1,  1,  2
        pf16c     1,   0,  0,  0
        pf16c     1,   4,  4,  5
        pf16c     1,   0,  0,  0
        endbab

PM_Pat_Chequer:
        pattern PATDF_16C, DEF_RL, 3, PATSF_BAPM_PMF, 2
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   1,  4,  1
        pf16c     1,   4,  1,  4
        endbab

PM_Pat_ByYourCommand:
        pattern PATDF_24B, 0, 1, PATSF_BAPM_PMF, 6
        dw 0, 0, 0
        bablock
        pf24b    40, 0, 0xE00000, 0x000000, 0x000000
        pf24b   112, 1, 0xFE0000, 0x020000, 0x000000
        pf24b   111, 1, 0x000000, 0xFE0000, 0x020000
        pf24b    40, 0, 0x000000, 0x000000, 0xE00000
        pf24b   112, 1, 0x000000, 0x020000, 0xFE0000
        pf24b   111, 1, 0x020000, 0xFE0000, 0x000000
        endbab

;PM_Pat_AWL:  ; Aircraft Warning Light
;        pattern PATDF_4C, 4, 1, PATSF_BAPM_PMF, 1
;        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
;        bablock
;        pf4c      1,   0,  0,  1
;        endbab

PM_Pat_TriC_Solid:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 1
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_TriConc
PM_Pat_TriC_Flash:
        pattern PATDF_16C, DEF_RL, 4, PATSF_BAPM_PMF, 2
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_TriConc
PM_Pat_TriC_Dashes:
        pattern PATDF_16C, DEF_RL, 6, PATSF_BAPM_PMF, 4
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_TriConc
BAPM_Frs_TriConc:
        bablock
        pf16c     1,   1,  4,  7
        pf16c     1,   0,  0,  0
        pf16c     1,   1,  4,  7
        pf16c     1,   1,  4,  7
        endbab

PM_Pat_TriR_Solid:
        pattern PATDF_16C, DEF_RL, 8, PATSF_BAPM_PMF, 3
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_TriRad
PM_Pat_TriR_Spaced:
        pattern PATDF_16C, DEF_RL, 4, PATSF_BAPM_PMF, 4
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, BAPM_Frs_TriRad
BAPM_Frs_TriRad:
        bablock
        pf16c     1,   1,  1,  1
        pf16c     1,   4,  4,  4
        pf16c     1,   7,  7,  7
        pf16c     1,   0,  0,  0
        endbab

PM_Pat_TriChaser:
        pattern PATDF_16C, DEF_RL, 160, PATSF_BAPM_PMF, 3
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   1,  4,  7
        pf16c     1,   7,  1,  4
        pf16c     1,   4,  7,  1
        endbab

PM_Pat_BlurStep:
        pattern PATDF_16C, DEF_RL, 2, PATSF_BAPM_PMF, 12
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   1,  1,  1
        pf16c     1,   2,  2,  1
        pf16c     1,   3,  3,  1
        pf16c     1,   0,  0,  1
        pf16c     1,   0,  0,  2
        pf16c     1,   0,  0,  2
        pf16c     1,   4,  4,  4
        pf16c     1,   4,  5,  5
        pf16c     1,   4,  6,  6
        pf16c     1,   4,  0,  0
        pf16c     1,   5,  0,  0
        pf16c     1,   5,  0,  0
        endbab

PM_Pat_Decay2:
        pattern PATDF_16C, DEF_RL, 2, PATSF_BAPM_PMF, 8
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, PM_Frs_Decay
PM_Pat_Decay3:
        pattern PATDF_16C, DEF_RL, 2, PATSF_BAPM_PMF, 12
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, PM_Frs_Decay
PM_Frs_Decay:
        bablock
        pf16c     1,   1,  1,  1
        pf16c     1,   2,  2,  2
        pf16c     1,   3,  3,  3
        pf16c     2,   0,  0,  0
        pf16c     1,   4,  4,  4
        pf16c     1,   5,  5,  5
        pf16c     1,   6,  6,  6
        pf16c     2,   0,  0,  0
        pf16c     1,   7,  7,  7
        pf16c     1,   8,  8,  8
        pf16c     1,   9,  9,  9
        pf16c     2,   0,  0,  0
        endbab

PM_Pat_Decay2X:
        pattern PATDF_16C, 14, 2, PATSF_BAPM_PMF, 10
        dw BAPM_Pal_Basic, BAPM_Map_Medley5X_UXXX, PM_Frs_DecayX
PM_Pat_Decay3X:
        pattern PATDF_16C, 14, 2, PATSF_BAPM_PMF, 15
        dw BAPM_Pal_Basic, BAPM_Map_Medley5X_UXXX, PM_Frs_DecayX
PM_Frs_DecayX:
        bablock
        pf16c     1,  13, 13, 13
        pf16c     1,   1,  1,  1
        pf16c     1,   2,  2,  2
        pf16c     1,   3,  3,  3
        pf16c     2,   0,  0,  0
        pf16c     1,  13, 13, 13
        pf16c     1,   4,  4,  4
        pf16c     1,   5,  5,  5
        pf16c     1,   6,  6,  6
        pf16c     2,   0,  0,  0
        pf16c     1,  13, 13, 13
        pf16c     1,   7,  7,  7
        pf16c     1,   8,  8,  8
        pf16c     1,   9,  9,  9
        pf16c     2,   0,  0,  0
        endbab

PM_Pat_Bookends:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 8
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   3,  3,  3
        pf16c     1,   2,  2,  2
        pf16c     1,   1,  1,  1
        pf16c     1,   7,  7,  7
        pf16c     1,   4,  4,  4
        pf16c     1,   5,  5,  5
        pf16c     1,   6,  6,  6
        pf16c     2,   0,  0,  0
        endbab

PM_Pat_BookendsX:
        pattern PATDF_16C, 14, 1, PATSF_BAPM_PMF, 8
        dw BAPM_Pal_Basic, BAPM_Map_Medley5X_UXXX, 0
        bablock
        pf16c     1,   3,  3,  3
        pf16c     1,   2,  2,  2
        pf16c     1,   1,  1,  1
        pf16c     1,  13, 13, 13
        pf16c     1,   4,  4,  4
        pf16c     1,   5,  5,  5
        pf16c     1,   6,  6,  6
        pf16c     2,   0,  0,  0
        endbab

PM_Pat_Slides:
        pattern PATDF_16C, DEF_RL, 16, PATSF_BAPM_PMF, 26
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c    16,   1,  1,  1
        pf16c     1,   2,  1,  1
        pf16c     1,   3,  1,  1
        pf16c     1,   0,  1,  1
        pf16c     1,   0,  2,  1
        pf16c     1,   6,  3,  1
        pf16c     1,   5,  0,  1
        pf16c     1,   4,  0,  2
        pf16c     1,   4,  6,  3
        pf16c     1,   4,  5,  0
        pf16c     1,   4,  4,  0
        pf16c     1,   4,  4,  6
        pf16c     1,   4,  4,  5
        pf16c    16,   4,  4,  4
        pf16c     1,   4,  4,  5
        pf16c     1,   4,  4,  6
        pf16c     1,   4,  4,  0
        pf16c     1,   4,  5,  0
        pf16c     1,   4,  6,  3
        pf16c     1,   4,  0,  2
        pf16c     1,   5,  0,  1
        pf16c     1,   6,  3,  1
        pf16c     1,   0,  2,  1
        pf16c     1,   0,  1,  1
        pf16c     1,   3,  1,  1
        pf16c     1,   2,  1,  1
        endbab

PM_Pat_RampUpOut:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 3
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   1,  2,  3
        pf16c     1,   3,  1,  2
        pf16c     1,   2,  3,  1
        endbab

PM_Pat_RampUpIn:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 3
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   3,  2,  1
        pf16c     1,   2,  1,  3
        pf16c     1,   1,  3,  2
        endbab

PM_Pat_FatRampUpOut:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 6
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   2,  3,  3
        pf16c     1,   2,  2,  3
        pf16c     1,   1,  2,  2
        pf16c     1,   1,  1,  2
        pf16c     1,   3,  1,  1
        pf16c     1,   3,  3,  1
        endbab

PM_Pat_FatRampUpIn:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 6
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   3,  3,  2
        pf16c     1,   3,  2,  2
        pf16c     1,   2,  2,  1
        pf16c     1,   2,  1,  1
        pf16c     1,   1,  1,  3
        pf16c     1,   1,  3,  3
        endbab

PM_Pat_RampDownOut:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 3
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   3,  2,  1
        pf16c     1,   1,  3,  2
        pf16c     1,   2,  1,  3
        endbab

PM_Pat_RampDownIn:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 3
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   1,  2,  3
        pf16c     1,   2,  3,  1
        pf16c     1,   3,  1,  2
        endbab

PM_Pat_FatRampDownOut:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 6
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   2,  1,  1
        pf16c     1,   2,  2,  1
        pf16c     1,   3,  2,  2
        pf16c     1,   3,  3,  2
        pf16c     1,   1,  3,  3
        pf16c     1,   1,  1,  3
        endbab

PM_Pat_FatRampDownIn:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 6
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   1,  1,  2
        pf16c     1,   1,  2,  2
        pf16c     1,   2,  2,  3
        pf16c     1,   2,  3,  3
        pf16c     1,   3,  3,  1
        pf16c     1,   3,  1,  1
        endbab

PM_Pat_AlienCircuitry:
        pattern PATDF_16C, 16, 1, PATSF_BAPM_PMF, 122
        dw BAPM_Pal_Basic, 0, BAPM_Frs_AlienCircuitry
BAPM_Frs_AlienCircuitry:
        bablock
        pf16c     1,   7,  7,  7
        pf16c     2,   0,  0,  0
        pf16c     1,   7,  7,  7
        pf16c     2,   0,  0,  7
        pf16c     1,   1,  0,  7
        pf16c     2,   0,  0,  7
        pf16c     1,   1,  0,  7
        pf16c     2,   0,  0,  7
        pf16c     1,   7,  7,  7
        pf16c     2,   7,  0,  0
        pf16c     1,   7,  0,  4
        pf16c     2,   7,  0,  0
        pf16c     4,   7,  0,  7
        pf16c     2,   0,  0,  7
        pf16c     1,   7,  7,  7
        pf16c     2,   0,  0,  0
        pf16c     1,   0,  1,  0
        pf16c     2,   0,  0,  0
        pf16c     1,   7,  7,  7
        pf16c     2,   0,  7,  0
        pf16c     1,   0,  7,  7
        pf16c     2,   0,  7,  0
        pf16c     1,   0,  7,  7
        pf16c     2,   0,  7,  0
        pf16c     1,   7,  7,  7
        pf16c     2,   7,  0,  0
        pf16c     1,   7,  7,  7
        pf16c     2,   0,  0,  7
        pf16c     1,   4,  0,  7
        pf16c     2,   0,  0,  7
        pf16c     1,   4,  0,  7
        pf16c     2,   0,  0,  7
        pf16c     1,   1,  0,  7
        pf16c     2,   0,  0,  7
        pf16c     1,   7,  7,  7
        pf16c     2,   7,  0,  0
        pf16c     1,   7,  0,  4
        pf16c     2,   7,  0,  0
        pf16c     1,   7,  7,  7
        pf16c     2,   0,  0,  0
        pf16c     1,   2,  1,  2
        pf16c     2,   0,  0,  0
        pf16c     1,   7,  7,  7
        pf16c     2,   0,  0,  7
        pf16c     1,   4,  0,  7
        pf16c     5,   0,  0,  7
        pf16c     1,   4,  0,  7
        pf16c     2,   0,  0,  7
        pf16c     1,   4,  0,  7
        pf16c     2,   0,  0,  7
        pf16c     4,   7,  0,  7
        pf16c     5,   7,  0,  0
        pf16c     1,   7,  7,  7
        pf16c     2,   0,  0,  0
        pf16c     1,   7,  7,  7
        pf16c     2,   0,  7,  0
        pf16c     1,   7,  7,  0
        pf16c     2,   0,  7,  0
        pf16c     1,   7,  7,  0
        pf16c     2,   0,  0,  0
        pf16c     1,   0,  7,  7
        pf16c     2,   0,  7,  0
        pf16c     1,   7,  7,  7
        pf16c     2,   0,  0,  0
        pf16c     1,   2,  1,  2
        pf16c     2,   0,  0,  0
        pf16c     1,   7,  7,  0
        pf16c     5,   0,  7,  0
        pf16c     1,   0,  7,  7
        pf16c     2,   0,  0,  0
        pf16c     1,   5,  4,  5
        pf16c     2,   0,  0,  0
        pf16c     1,   5,  4,  5
        pf16c     2,   0,  0,  0
        pf16c     1,   7,  7,  7
        pf16c     5,   7,  0,  0
        pf16c     1,   7,  0,  1
        pf16c     2,   7,  0,  0
        pf16c     1,   7,  0,  1
        pf16c     2,   7,  0,  0
        pf16c     1,   7,  0,  4
        pf16c     2,   7,  0,  0
        pf16c     1,   7,  7,  7
        pf16c     5,   0,  7,  0
        pf16c     1,   7,  7,  7
        pf16c     1,   7,  0,  7
        pf16c     1,   0,  0,  0
        pf16c     1,   0,  1,  0
        pf16c     1,   0,  0,  0
        pf16c     1,   7,  0,  7
        pf16c     1,   7,  7,  7
        pf16c     5,   0,  7,  0
        pf16c     1,   0,  7,  7
        pf16c     2,   0,  0,  0
        pf16c     1,   0,  7,  7
        pf16c     2,   0,  7,  0
        pf16c     1,   7,  7,  7
        pf16c     2,   0,  0,  7
        pf16c     1,   4,  0,  7
        pf16c     2,   0,  0,  7
        pf16c     1,   4,  0,  7
        pf16c     2,   0,  0,  7
        pf16c     1,   7,  7,  7
        pf16c     2,   0,  0,  7
        pf16c     1,   7,  7,  7
        pf16c     2,   0,  0,  0
        pf16c     1,   1,  0,  4
        pf16c     2,   0,  0,  0
        pf16c     1,   1,  0,  4
        pf16c     2,   0,  0,  0
        pf16c     1,   7,  7,  7
        pf16c     5,   7,  0,  0
        pf16c     1,   7,  0,  1
        pf16c     2,   7,  0,  0
        pf16c     1,   7,  7,  7
        pf16c     2,   0,  0,  7
        pf16c     1,   7,  7,  7
        pf16c     2,   7,  0,  0
        pf16c     1,   7,  7,  7
        pf16c     2,   0,  0,  7
        pf16c     1,   7,  7,  7
        pf16c     2,   7,  0,  0
        endbab

PM_Pat_PulseJet:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 12
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   2,  3,  3
        pf16c     1,   1,  3,  3
        pf16c     1,   1,  3,  3
        pf16c     1,   1,  2,  3
        pf16c     1,   1,  2,  3
        pf16c     1,   1,  1,  3
        pf16c     1,   1,  1,  3
        pf16c     1,   1,  1,  2
        pf16c     1,   1,  2,  2
        pf16c     1,   1,  2,  2
        pf16c     1,   2,  3,  2
        pf16c     1,   2,  3,  3
        endbab

PM_Pat_HubBrazier:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 37
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   1,  2,  3
        pf16c     1,   1,  1,  3
        pf16c     1,   1,  2,  2
        pf16c     1,   2,  3,  0
        pf16c     2,   1,  2,  3
        pf16c     1,   2,  2,  3
        pf16c     1,   2,  3,  3
        pf16c     1,   1,  3,  3
        pf16c     1,   1,  3,  0
        pf16c     2,   1,  2,  0
        pf16c     1,   2,  2,  3
        pf16c     1,   3,  2,  3
        pf16c     1,   2,  3,  0
        pf16c     1,   1,  3,  0
        pf16c     1,   2,  2,  3
        pf16c     1,   3,  3,  0
        pf16c     1,   2,  3,  0
        pf16c     1,   1,  3,  3
        pf16c     1,   1,  2,  3
        pf16c     1,   1,  1,  3
        pf16c     1,   1,  1,  2
        pf16c     1,   2,  2,  1
        pf16c     1,   1,  3,  2
        pf16c     1,   1,  2,  2
        pf16c     1,   1,  1,  2
        pf16c     1,   1,  2,  2
        pf16c     1,   1,  2,  3
        pf16c     1,   2,  2,  3
        pf16c     1,   2,  3,  3
        pf16c     1,   3,  3,  0
        pf16c     1,   1,  3,  0
        pf16c     1,   1,  2,  3
        pf16c     1,   1,  1,  3
        pf16c     1,   2,  3,  2
        pf16c     1,   3,  0,  2
        pf16c     1,   2,  0,  0
        pf16c     1,   1,  3,  0
        endbab

PM_Pat_RimBrazier:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 37
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   3,  2,  1
        pf16c     1,   3,  1,  1
        pf16c     1,   2,  2,  1
        pf16c     1,   0,  3,  2
        pf16c     2,   3,  2,  1
        pf16c     1,   3,  2,  2
        pf16c     1,   3,  3,  2
        pf16c     1,   3,  3,  1
        pf16c     1,   0,  3,  1
        pf16c     2,   0,  2,  1
        pf16c     1,   3,  2,  2
        pf16c     1,   3,  2,  3
        pf16c     1,   0,  3,  2
        pf16c     1,   0,  3,  1
        pf16c     1,   3,  2,  2
        pf16c     1,   0,  3,  3
        pf16c     1,   0,  3,  2
        pf16c     1,   3,  3,  1
        pf16c     1,   3,  2,  1
        pf16c     1,   3,  1,  1
        pf16c     1,   2,  1,  1
        pf16c     1,   1,  2,  2
        pf16c     1,   2,  3,  1
        pf16c     1,   2,  2,  1
        pf16c     1,   2,  1,  1
        pf16c     1,   2,  2,  1
        pf16c     1,   3,  2,  1
        pf16c     1,   3,  2,  2
        pf16c     1,   3,  3,  2
        pf16c     1,   0,  3,  3
        pf16c     1,   0,  3,  1
        pf16c     1,   3,  2,  1
        pf16c     1,   3,  1,  1
        pf16c     1,   2,  3,  2
        pf16c     1,   2,  0,  3
        pf16c     1,   0,  0,  2
        pf16c     1,   0,  3,  1
        endbab

PM_Pat_MidBrazier:
        pattern PATDF_16C, DEF_RL, 1, PATSF_BAPM_PMF, 40
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
        pf16c     1,   2,  1,  2
        pf16c     1,   2,  1,  3
        pf16c     1,   1,  1,  0
        pf16c     1,   1,  2,  3
        pf16c     1,   2,  1,  3
        pf16c     1,   2,  1,  2
        pf16c     1,   3,  2,  1
        pf16c     1,   2,  1,  2
        pf16c     1,   3,  1,  2
        pf16c     1,   0,  2,  1
        pf16c     1,   2,  1,  0
        pf16c     1,   1,  1,  3
        pf16c     1,   1,  2,  1
        pf16c     1,   3,  1,  2
        pf16c     1,   2,  2,  1
        pf16c     1,   1,  3,  2
        pf16c     1,   3,  1,  0
        pf16c     1,   2,  1,  3
        pf16c     1,   0,  2,  2
        pf16c     1,   3,  2,  0
        pf16c     1,   2,  1,  3
        pf16c     1,   1,  1,  1
        pf16c     1,   3,  1,  2
        pf16c     1,   2,  1,  1
        pf16c     1,   1,  2,  3
        pf16c     1,   2,  3,  0
        pf16c     1,   0,  1,  3
        pf16c     1,   3,  2,  3
        pf16c     1,   2,  1,  2
        pf16c     1,   3,  1,  1
        pf16c     1,   3,  2,  3
        pf16c     1,   2,  1,  0
        pf16c     1,   1,  1,  2
        pf16c     1,   2,  1,  3
        pf16c     1,   3,  2,  3
        pf16c     1,   1,  1,  2
        pf16c     1,   2,  2,  1
        pf16c     1,   0,  3,  2
        pf16c     1,   3,  2,  0
        pf16c     1,   2,  2,  3
        endbab

PM_Pat_HQHubBrazier:
        pattern PATDF_16C, 16, 1, PATSF_BAPM_PMF, 137
        dw BAPM_Pal_Ramp16_Flame, 0, 0
        bablock
        pf16c     1,  11,  7,  6
        pf16c     1,  11,  8,  7
        pf16c     1,  12, 10,  5
        pf16c     1,  13, 11,  4
        pf16c     1,  13, 12,  4
        pf16c     1,  14, 12,  5
        pf16c     1,  14, 11,  7
        pf16c     1,  14, 10,  9
        pf16c     1,  13,  8,  9
        pf16c     1,  12,  7,  8
        pf16c     1,  10,  6,  5
        pf16c     1,   7,  5,  3
        pf16c     1,   7,  5,  1
        pf16c     1,   8,  6,  1
        pf16c     1,  12,  9,  2
        pf16c     1,  15, 11,  4
        pf16c     2,  15, 12,  5
        pf16c     1,  14, 11,  6
        pf16c     1,  11,  9,  6
        pf16c     1,   9,  8,  5
        pf16c     1,   8,  7,  5
        pf16c     1,   8,  6,  4
        pf16c     1,   7,  4,  3
        pf16c     1,   7,  3,  2
        pf16c     1,   8,  3,  3
        pf16c     1,   9,  3,  4
        pf16c     1,  10,  3,  6
        pf16c     1,  11,  3,  6
        pf16c     1,  11,  3,  5
        pf16c     1,  12,  3,  3
        pf16c     2,  12,  4,  0
        pf16c     1,  12,  5,  0
        pf16c     1,  13,  6,  0
        pf16c     1,  13,  7,  1
        pf16c     1,  13,  8,  3
        pf16c     1,  13,  8,  4
        pf16c     1,  13,  9,  5
        pf16c     1,  13,  8,  6
        pf16c     1,  12,  8,  7
        pf16c     1,  10,  8,  7
        pf16c     1,   7,  7,  8
        pf16c     1,   4,  7,  8
        pf16c     1,   3,  6,  7
        pf16c     1,   4,  6,  6
        pf16c     1,   6,  6,  4
        pf16c     1,   8,  5,  2
        pf16c     1,  10,  5,  0
        pf16c     1,  11,  5,  0
        pf16c     2,  12,  6,  0
        pf16c     1,  11,  6,  0
        pf16c     1,  11,  7,  2
        pf16c     1,  11,  8,  4
        pf16c     1,  10,  8,  6
        pf16c     1,  10,  9,  7
        pf16c     1,   9,  9,  6
        pf16c     1,   7,  8,  3
        pf16c     1,   6,  8,  0
        pf16c     1,   6,  7,  0
        pf16c     1,   7,  7,  0
        pf16c     1,   9,  7,  0
        pf16c     1,  11,  6,  2
        pf16c     1,  12,  6,  3
        pf16c     1,  12,  5,  4
        pf16c     1,  12,  4,  4
        pf16c     2,  12,  3,  5
        pf16c     1,  13,  4,  6
        pf16c     1,  14,  7,  7
        pf16c     1,  15,  9,  7
        pf16c     1,  15, 12,  7
        pf16c     1,  15, 13,  6
        pf16c     1,  14, 14,  5
        pf16c     1,  12, 15,  3
        pf16c     1,  11, 15,  2
        pf16c     1,  11, 15,  3
        pf16c     1,  12, 15,  5
        pf16c     1,  12, 15,  7
        pf16c     1,  12, 14,  9
        pf16c     1,  11, 14, 11
        pf16c     1,   9, 13, 12
        pf16c     1,   8, 12, 12
        pf16c     1,   7, 11, 12
        pf16c     1,   9, 10, 11
        pf16c     1,  12,  8, 10
        pf16c     1,  14,  7,  8
        pf16c     2,  15,  6,  6
        pf16c     1,  15,  7,  6
        pf16c     1,  13,  8,  6
        pf16c     1,  12,  9,  6
        pf16c     1,  12, 10,  7
        pf16c     1,  12, 12,  9
        pf16c     4,  12, 13, 10
        pf16c     1,  13, 12,  9
        pf16c     1,  13, 11,  8
        pf16c     1,  12, 11,  6
        pf16c     1,  12, 11,  5
        pf16c     1,  11, 12,  4
        pf16c     1,  11, 12,  3
        pf16c     1,  11, 12,  4
        pf16c     1,  11, 12,  5
        pf16c     1,  11, 12,  7
        pf16c     1,  11, 11,  8
        pf16c     1,  10,  9,  7
        pf16c     1,   9,  6,  6
        pf16c     1,   7,  4,  4
        pf16c     1,   6,  2,  3
        pf16c     1,   6,  2,  2
        pf16c     1,   6,  4,  2
        pf16c     1,   7,  5,  2
        pf16c     1,   7,  6,  2
        pf16c     1,   9,  6,  2
        pf16c     1,  11,  6,  2
        pf16c     1,  13,  6,  2
        pf16c     2,  14,  6,  3
        pf16c     1,  14,  7,  4
        pf16c     1,  13,  7,  5
        pf16c     1,  12,  8,  6
        pf16c     1,  12, 11,  6
        pf16c     1,  11, 13,  6
        pf16c     2,  11, 15,  5
        pf16c     1,  10, 14,  6
        pf16c     1,   9, 11,  7
        pf16c     1,   9,  7,  7
        pf16c     1,   8,  4,  8
        pf16c     1,   7,  3,  8
        pf16c     1,   6,  3,  8
        pf16c     2,   5,  3,  7
        pf16c     1,   5,  3,  5
        pf16c     1,   6,  3,  4
        pf16c     1,   7,  2,  2
        pf16c     1,   8,  3,  0
        pf16c     1,   9,  3,  0
        pf16c     1,  11,  4,  0
        pf16c     1,  12,  4,  0
        pf16c     1,  13,  5,  0
        pf16c     1,  13,  6,  0
        pf16c     1,  12,  6,  2
        pf16c     1,  11,  6,  5
        endbab

PM_Pat_HQRimBrazier:
        pattern PATDF_16C, 16, 1, PATSF_BAPM_PMF, 140
        dw BAPM_Pal_Ramp16_Flame, 0, 0
        bablock
        pf16c     1,   2,  7, 15
        pf16c     1,   3,  8, 15
        pf16c     1,   3, 10, 14
        pf16c     1,   4, 11, 13
        pf16c     1,   5, 12, 12
        pf16c     1,   6, 12, 12
        pf16c     1,   8, 11, 13
        pf16c     1,   9, 10, 14
        pf16c     1,   9,  8, 13
        pf16c     1,   7,  7, 13
        pf16c     1,   3,  6, 12
        pf16c     2,   0,  5, 10
        pf16c     1,   0,  6, 10
        pf16c     1,   2,  9, 12
        pf16c     1,   5, 11, 13
        pf16c     1,   7, 12, 13
        pf16c     1,   8, 12, 13
        pf16c     1,   7, 11, 12
        pf16c     1,   6,  9, 11
        pf16c     1,   5,  8, 10
        pf16c     1,   4,  7,  9
        pf16c     1,   4,  6,  8
        pf16c     1,   3,  4,  7
        pf16c     1,   3,  3,  6
        pf16c     1,   3,  3,  8
        pf16c     1,   2,  3, 11
        pf16c     1,   2,  3, 13
        pf16c     1,   2,  3, 15
        pf16c     1,   1,  3, 15
        pf16c     1,   0,  3, 14
        pf16c     1,   0,  4, 13
        pf16c     1,   0,  4, 12
        pf16c     1,   0,  5, 13
        pf16c     1,   0,  6, 14
        pf16c     1,   0,  7, 15
        pf16c     1,   0,  8, 15
        pf16c     1,   2,  8, 15
        pf16c     1,   4,  9, 14
        pf16c     1,   6,  8, 13
        pf16c     1,   7,  8, 11
        pf16c     1,   7,  8, 10
        pf16c     1,   6,  7,  9
        pf16c     1,   4,  7,  8
        pf16c     1,   3,  6,  7
        pf16c     1,   2,  6,  7
        pf16c     1,   2,  6,  8
        pf16c     1,   2,  5,  8
        pf16c     1,   2,  5,  9
        pf16c     1,   1,  5, 10
        pf16c     1,   0,  6, 11
        pf16c     1,   0,  6, 12
        pf16c     1,   0,  6, 13
        pf16c     1,   0,  7, 13
        pf16c     1,   2,  8, 13
        pf16c     1,   4,  8, 12
        pf16c     1,   5,  9, 11
        pf16c     1,   5,  9,  9
        pf16c     1,   4,  8,  6
        pf16c     1,   2,  8,  4
        pf16c     1,   1,  7,  3
        pf16c     1,   1,  7,  4
        pf16c     1,   2,  7,  7
        pf16c     1,   3,  6, 10
        pf16c     1,   3,  6, 12
        pf16c     1,   3,  5, 13
        pf16c     1,   3,  4, 13
        pf16c     1,   3,  3, 13
        pf16c     1,   3,  3, 14
        pf16c     1,   4,  4, 15
        pf16c     1,   5,  7, 15
        pf16c     1,   7,  9, 15
        pf16c     1,   7, 12, 15
        pf16c     1,   6, 13, 15
        pf16c     1,   5, 14, 14
        pf16c     1,   3, 15, 12
        pf16c     1,   2, 15, 11
        pf16c     1,   3, 15, 11
        pf16c     1,   4, 15, 12
        pf16c     1,   6, 15, 13
        pf16c     1,   7, 14, 13
        pf16c     1,   9, 14, 13
        pf16c     1,  10, 13, 11
        pf16c     1,  11, 12,  9
        pf16c     1,  11, 11,  8
        pf16c     1,  12, 10,  8
        pf16c     1,  12,  8,  9
        pf16c     1,  12,  7, 10
        pf16c     2,  11,  6, 11
        pf16c     1,  10,  7, 11
        pf16c     1,   8,  8, 11
        pf16c     1,   7,  9, 11
        pf16c     1,   7, 10, 12
        pf16c     1,   7, 12, 13
        pf16c     2,   7, 13, 14
        pf16c     2,   8, 13, 14
        pf16c     1,   9, 12, 13
        pf16c     1,   8, 11, 12
        pf16c     1,   7, 11, 12
        pf16c     1,   5, 11, 12
        pf16c     1,   3, 12, 12
        pf16c     2,   2, 12, 12
        pf16c     1,   4, 12, 12
        pf16c     1,   6, 12, 12
        pf16c     1,   6, 11, 12
        pf16c     1,   6,  9, 11
        pf16c     1,   4,  6, 10
        pf16c     1,   3,  4,  9
        pf16c     2,   2,  2,  7
        pf16c     1,   2,  4,  6
        pf16c     1,   3,  5,  6
        pf16c     1,   3,  6,  6
        pf16c     1,   3,  6,  8
        pf16c     1,   2,  6, 11
        pf16c     1,   1,  6, 14
        pf16c     2,   1,  6, 15
        pf16c     1,   2,  7, 15
        pf16c     1,   3,  7, 15
        pf16c     1,   3,  8, 14
        pf16c     1,   3, 11, 14
        pf16c     1,   2, 13, 15
        pf16c     1,   2, 15, 15
        pf16c     1,   2, 15, 14
        pf16c     1,   3, 14, 13
        pf16c     1,   5, 11, 11
        pf16c     1,   6,  7, 10
        pf16c     1,   8,  4,  8
        pf16c     1,   9,  3,  6
        pf16c     1,  10,  3,  4
        pf16c     1,  10,  3,  3
        pf16c     1,   9,  3,  2
        pf16c     1,   7,  3,  3
        pf16c     1,   4,  3,  5
        pf16c     1,   1,  2,  7
        pf16c     1,   0,  3,  9
        pf16c     1,   0,  3, 10
        pf16c     2,   0,  4, 10
        pf16c     1,   0,  5, 11
        pf16c     1,   1,  6, 12
        pf16c     1,   1,  6, 13
        pf16c     1,   2,  6, 14
        endbab

PM_Pat_HQMidBrazier:
        pattern PATDF_16C, 16, 1, PATSF_BAPM_PMF, 154
        dw BAPM_Pal_Ramp16_Flame, 0, 0
        bablock
        pf16c     1,   7, 11, 11
        pf16c     1,   7, 11, 10
        pf16c     1,   7, 12,  8
        pf16c     1,   8, 12,  6
        pf16c     1,   9, 12,  4
        pf16c     1,  10, 12,  2
        pf16c     1,  12, 13,  1
        pf16c     1,  13, 13,  1
        pf16c     1,  13, 13,  0
        pf16c     1,  13, 12,  1
        pf16c     1,  13, 11,  3
        pf16c     1,  12, 10,  4
        pf16c     1,  11,  9,  5
        pf16c     1,  11, 11,  6
        pf16c     1,  12, 13,  5
        pf16c     2,  12, 15,  5
        pf16c     1,  12, 15,  6
        pf16c     1,  11, 15,  7
        pf16c     1,  10, 14,  9
        pf16c     1,   9, 12, 10
        pf16c     1,   8, 11, 11
        pf16c     1,   6, 10, 11
        pf16c     1,   4,  8, 11
        pf16c     2,   3,  8, 11
        pf16c     1,   4, 10, 11
        pf16c     1,   6, 11, 11
        pf16c     1,   6, 12, 10
        pf16c     1,   6, 13, 10
        pf16c     1,   5, 13,  9
        pf16c     1,   4, 13,  8
        pf16c     1,   3, 13,  8
        pf16c     1,   2, 12, 10
        pf16c     1,   1, 10, 13
        pf16c     1,   0,  9, 15
        pf16c     1,   0,  8, 15
        pf16c     1,   2,  8, 14
        pf16c     1,   6, 10,  9
        pf16c     1,   9, 12,  5
        pf16c     1,  12, 13,  2
        pf16c     1,  13, 13,  2
        pf16c     1,  12, 12,  4
        pf16c     1,  12, 11,  6
        pf16c     1,  11, 11,  7
        pf16c     1,  12, 10,  9
        pf16c     1,  14, 10, 11
        pf16c     1,  15,  9, 12
        pf16c     1,  15,  9, 13
        pf16c     1,  12, 11, 13
        pf16c     1,   9, 13, 11
        pf16c     1,   5, 15,  9
        pf16c     1,   3, 15,  9
        pf16c     1,   3, 14, 10
        pf16c     1,   5, 13, 13
        pf16c     1,   8, 10, 15
        pf16c     1,  10,  9, 15
        pf16c     1,  11,  8, 14
        pf16c     1,  13,  7, 12
        pf16c     1,  14,  7,  9
        pf16c     1,  14,  7,  7
        pf16c     1,  13,  9,  6
        pf16c     1,  11, 11,  4
        pf16c     1,   9, 13,  4
        pf16c     2,   7, 14,  3
        pf16c     1,   7, 14,  4
        pf16c     1,   8, 12,  4
        pf16c     1,   7, 11,  5
        pf16c     1,   6, 11,  7
        pf16c     1,   5, 12, 10
        pf16c     1,   4, 12, 12
        pf16c     1,   3, 12, 12
        pf16c     1,   2, 12,  9
        pf16c     1,   2, 11,  5
        pf16c     2,   2, 11,  0
        pf16c     1,   3, 12,  0
        pf16c     1,   5, 13,  0
        pf16c     1,   6, 14,  2
        pf16c     1,   7, 14,  5
        pf16c     1,   9, 15,  7
        pf16c     1,  10, 15, 10
        pf16c     1,  11, 15, 11
        pf16c     2,  11, 15, 12
        pf16c     1,  10, 15, 10
        pf16c     1,   8, 15,  7
        pf16c     1,   7, 15,  6
        pf16c     1,   7, 15,  7
        pf16c     1,   7, 15,  9
        pf16c     1,   7, 14, 10
        pf16c     1,   7, 13, 11
        pf16c     1,   9, 13, 10
        pf16c     1,  10, 12,  9
        pf16c     1,  11, 10,  8
        pf16c     1,  12,  9,  6
        pf16c     1,  12,  8,  4
        pf16c     1,  11,  7,  2
        pf16c     1,  10,  6,  0
        pf16c     1,   8,  7,  0
        pf16c     1,   6,  9,  0
        pf16c     1,   2, 12,  0
        pf16c     1,   0, 14,  2
        pf16c     1,   0, 15,  3
        pf16c     1,   0, 15,  5
        pf16c     1,   1, 14,  6
        pf16c     1,   4, 12,  7
        pf16c     1,   6, 11,  8
        pf16c     1,   7, 10,  8
        pf16c     1,   7, 10,  7
        pf16c     1,   6, 10,  7
        pf16c     1,   6, 11,  7
        pf16c     1,   7, 12,  9
        pf16c     1,   7, 13, 12
        pf16c     1,   7, 14, 14
        pf16c     1,   7, 15, 15
        pf16c     1,   7, 14, 14
        pf16c     1,   6, 13, 12
        pf16c     1,   6, 11,  9
        pf16c     1,   5, 10,  7
        pf16c     1,   6, 11,  5
        pf16c     1,   6, 11,  3
        pf16c     1,   7, 12,  2
        pf16c     1,   8, 13,  1
        pf16c     1,   9, 14,  3
        pf16c     1,  10, 15,  6
        pf16c     1,  10, 15,  8
        pf16c     1,  11, 15, 10
        pf16c     1,  10, 15,  9
        pf16c     1,  10, 15,  7
        pf16c     1,   9, 14,  5
        pf16c     1,   8, 13,  3
        pf16c     1,   7, 13,  2
        pf16c     1,   6, 12,  2
        pf16c     2,   5, 12,  2
        pf16c     1,   6, 13,  4
        pf16c     1,   9, 14,  5
        pf16c     1,  11, 15,  7
        pf16c     1,  12, 15,  9
        pf16c     1,  12, 15, 10
        pf16c     1,  12, 13, 10
        pf16c     1,  11, 11, 11
        pf16c     1,   9,  9, 11
        pf16c     1,   6,  7, 10
        pf16c     1,   3,  6, 10
        pf16c     1,   0,  4,  9
        pf16c     1,   0,  3,  8
        pf16c     1,   0,  5,  6
        pf16c     1,   2,  7,  4
        pf16c     1,   4,  9,  1
        pf16c     1,   7, 11,  0
        pf16c     1,   8, 12,  1
        pf16c     1,   9, 12,  3
        pf16c     1,   9, 12,  5
        pf16c     1,   9, 11,  7
        pf16c     1,   9, 12,  9
        pf16c     1,   8, 11, 10
        pf16c     1,   7, 11, 11
        endbab

PM_Pat_HQPulseJet:
        pattern PATDF_16C, 16, 1, PATSF_BAPM_PMF, 21
        dw BAPM_Pal_Ramp16_Flame, 0, 0
        bablock
        pf16c     4,  12,  8,  5
        pf16c     2,  12,  8,  6
        pf16c     2,  13,  8,  6
        pf16c     2,  14,  8,  6
        pf16c     3,  15,  8,  6
        pf16c     3,  15,  9,  6
        pf16c     2,  15, 10,  6
        pf16c     3,  14, 11,  6
        pf16c     2,  13, 11,  6
        pf16c     2,  12, 11,  7
        pf16c     2,  11, 11,  7
        pf16c     2,  11, 10,  8
        pf16c     1,  10, 10,  8
        pf16c     2,  10,  9,  8
        pf16c     2,  10,  8,  8
        pf16c     1,  11,  8,  7
        pf16c     4,  11,  7,  7
        pf16c     1,  11,  7,  6
        pf16c     3,  12,  7,  6
        pf16c     4,  12,  7,  5
        pf16c     1,  12,  8,  5
        endbab


INCLUDE_MORSE equ 1

  if INCLUDE_MORSE

; Temporal Morse (the usual kind)

morse_dash macro
        pf4c      3,   1,  1,  1
        pf4c      1,   0,  0,  0
num_frames += 2
  endm

morse_dot macro
        pf4c      1,   2,  2,  2
        pf4c      1,   0,  0,  0
num_frames += 2
  endm

morse_letter_space macro
        pf4c      2,   0,  0,  0
num_frames += 1
  endm

morse_word_space macro
        pf4c      3,   0,  0,  0
        pf4c      3,   0,  0,  0
num_frames += 2
  endm

morse_A macro
        morse_dot
        morse_dash
        morse_letter_space
  endm
morse_B macro
        morse_dash
        morse_dot
        morse_dot
        morse_dot
        morse_letter_space
  endm
morse_C macro
        morse_dash
        morse_dot
        morse_dash
        morse_dot
        morse_letter_space
  endm
morse_D macro
        morse_dash
        morse_dot
        morse_dot
        morse_letter_space
  endm
morse_E macro
        morse_dot
        morse_letter_space
  endm
morse_F macro
        morse_dot
        morse_dot
        morse_dash
        morse_dot
        morse_letter_space
  endm
morse_G macro
        morse_dash
        morse_dash
        morse_dot
        morse_letter_space
  endm
morse_H macro
        morse_dot
        morse_dot
        morse_dot
        morse_dot
        morse_letter_space
  endm
morse_I macro
        morse_dot
        morse_dot
        morse_letter_space
  endm
morse_J macro
        morse_dot
        morse_dash
        morse_dash
        morse_dash
        morse_letter_space
  endm
morse_K macro
        morse_dash
        morse_dot
        morse_dash
        morse_letter_space
  endm
morse_L macro
        morse_dot
        morse_dash
        morse_dot
        morse_dot
        morse_letter_space
  endm
morse_M macro
        morse_dash
        morse_dash
        morse_letter_space
  endm
morse_N macro
        morse_dash
        morse_dot
        morse_letter_space
  endm
morse_O macro
        morse_dash
        morse_dash
        morse_dash
        morse_letter_space
  endm
morse_P macro
        morse_dot
        morse_dash
        morse_dash
        morse_dot
        morse_letter_space
  endm
morse_Q macro
        morse_dash
        morse_dash
        morse_dot
        morse_dash
        morse_letter_space
  endm
morse_R macro
        morse_dot
        morse_dash
        morse_dot
        morse_letter_space
  endm
morse_S macro
        morse_dot
        morse_dot
        morse_dot
        morse_letter_space
  endm
morse_T macro
        morse_dash
        morse_letter_space
  endm
morse_U macro
        morse_dot
        morse_dot
        morse_dash
        morse_letter_space
  endm
morse_V macro
        morse_dot
        morse_dot
        morse_dot
        morse_dash
        morse_letter_space
  endm
morse_W macro
        morse_dot
        morse_dash
        morse_dash
        morse_letter_space
  endm
morse_X macro
        morse_dash
        morse_dot
        morse_dot
        morse_dash
        morse_letter_space
  endm
morse_Y macro
        morse_dash
        morse_dot
        morse_dash
        morse_dash
        morse_letter_space
  endm
morse_Z macro
        morse_dash
        morse_dash
        morse_dot
        morse_dot
        morse_letter_space
  endm
morse_apos macro
        morse_dot
        morse_dash
        morse_dash
        morse_dash
        morse_dash
        morse_dot
        morse_letter_space
  endm
morse_period macro
        morse_dot
        morse_dash
        morse_dot
        morse_dash
        morse_dot
        morse_dash
        morse_letter_space
  endm
morse_comma macro
        morse_dash
        morse_dash
        morse_dot
        morse_dot
        morse_dash
        morse_dash
        morse_letter_space
  endm
morse_exclaim macro
        morse_dash
        morse_dot
        morse_dash
        morse_dot
        morse_dash
        morse_dash
        morse_letter_space
  endm
morse_question macro
        morse_dot
        morse_dot
        morse_dash
        morse_dash
        morse_dot
        morse_dot
        morse_letter_space
  endm

; Spatial Morse (Persistence of Vision)

smorse_dash macro
        pf4c      1,   1,  1,  1
        pf4c      3,   1,  1,  1
        pf4c      2,   0,  0,  0
num_frames += 3
  endm

smorse_dot macro
        pf4c      1,   2,  2,  2
        pf4c      2,   0,  0,  0
num_frames += 2
  endm

smorse_letter_space macro
        pf4c      3,   0,  0,  0
num_frames += 1
  endm

smorse_word_space macro
        pf4c      3,   0,  0,  0
        pf4c      3,   0,  0,  0
        pf4c      3,   0,  0,  0
num_frames += 3
  endm

smorse_A macro
        smorse_dot
        smorse_dash
        smorse_letter_space
  endm
smorse_B macro
        smorse_dash
        smorse_dot
        smorse_dot
        smorse_dot
        smorse_letter_space
  endm
smorse_C macro
        smorse_dash
        smorse_dot
        smorse_dash
        smorse_dot
        smorse_letter_space
  endm
smorse_D macro
        smorse_dash
        smorse_dot
        smorse_dot
        smorse_letter_space
  endm
smorse_E macro
        smorse_dot
        smorse_letter_space
  endm
smorse_F macro
        smorse_dot
        smorse_dot
        smorse_dash
        smorse_dot
        smorse_letter_space
  endm
smorse_G macro
        smorse_dash
        smorse_dash
        smorse_dot
        smorse_letter_space
  endm
smorse_H macro
        smorse_dot
        smorse_dot
        smorse_dot
        smorse_dot
        smorse_letter_space
  endm
smorse_I macro
        smorse_dot
        smorse_dot
        smorse_letter_space
  endm
smorse_J macro
        smorse_dot
        smorse_dash
        smorse_dash
        smorse_dash
        smorse_letter_space
  endm
smorse_K macro
        smorse_dash
        smorse_dot
        smorse_dash
        smorse_letter_space
  endm
smorse_L macro
        smorse_dot
        smorse_dash
        smorse_dot
        smorse_dot
        smorse_letter_space
  endm
smorse_M macro
        smorse_dash
        smorse_dash
        smorse_letter_space
  endm
smorse_N macro
        smorse_dash
        smorse_dot
        smorse_letter_space
  endm
smorse_O macro
        smorse_dash
        smorse_dash
        smorse_dash
        smorse_letter_space
  endm
smorse_P macro
        smorse_dot
        smorse_dash
        smorse_dash
        smorse_dot
        smorse_letter_space
  endm
smorse_Q macro
        smorse_dash
        smorse_dash
        smorse_dot
        smorse_dash
        smorse_letter_space
  endm
smorse_R macro
        smorse_dot
        smorse_dash
        smorse_dot
        smorse_letter_space
  endm
smorse_S macro
        smorse_dot
        smorse_dot
        smorse_dot
        smorse_letter_space
  endm
smorse_T macro
        smorse_dash
        smorse_letter_space
  endm
smorse_U macro
        smorse_dot
        smorse_dot
        smorse_dash
        smorse_letter_space
  endm
smorse_V macro
        smorse_dot
        smorse_dot
        smorse_dot
        smorse_dash
        smorse_letter_space
  endm
smorse_W macro
        smorse_dot
        smorse_dash
        smorse_dash
        smorse_letter_space
  endm
smorse_X macro
        smorse_dash
        smorse_dot
        smorse_dot
        smorse_dash
        smorse_letter_space
  endm
smorse_Y macro
        smorse_dash
        smorse_dot
        smorse_dash
        smorse_dash
        smorse_letter_space
  endm
smorse_Z macro
        smorse_dash
        smorse_dash
        smorse_dot
        smorse_dot
        smorse_letter_space
  endm
smorse_apos macro
        smorse_dot
        smorse_dash
        smorse_dash
        smorse_dash
        smorse_dash
        smorse_dot
        smorse_letter_space
  endm
smorse_period macro
        smorse_dot
        smorse_dash
        smorse_dot
        smorse_dash
        smorse_dot
        smorse_dash
        smorse_letter_space
  endm
smorse_comma macro
        smorse_dash
        smorse_dash
        smorse_dot
        smorse_dot
        smorse_dash
        smorse_dash
        smorse_letter_space
  endm
smorse_exclaim macro
        smorse_dash
        smorse_dot
        smorse_dash
        smorse_dot
        smorse_dash
        smorse_dash
        smorse_letter_space
  endm
smorse_question macro
        smorse_dot
        smorse_dot
        smorse_dash
        smorse_dash
        smorse_dot
        smorse_dot
        smorse_letter_space
  endm

; This message is n = 378 dit units long, including the terminal spacing.
; The PWM is good for f = 443Hz. The message takes T_m = n*(1/f) = 0.853273s
; to transmit. The poi stick is w = 0.028m wide. To avoid smudging, the
; message requires a linear distance of d = n*w = 10.584m, which is the
; circumference of a circle of r = d/(2*pi) = 1.6845m. The linear speed of
; the poi will be v = d/T_m = 12.4m/s (just over 44.6km/h).
;
; Changing the pattern unit frame period does not change the radius required
; but the increasing the frame period allows a lower linear speed.
;
;       Message
;  UFP  Period    Linear Speed
;   1    0.85s  12.4m/s, 44.6km/h
;   2    1.71s   6.2m/s, 22.3km/h
;   3    2.56s   4.1m/s, 14.9km/h

PM_Pat_Morse:
        pattern PATDF_4C, 4, 96, PATSF_BAPM_PMF, NUM_MORSE_FRAMES
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
num_frames = 0
        morse_H
        morse_E
        morse_L
        morse_P
        morse_exclaim
        morse_word_space
        morse_I
        morse_apos
        morse_M
        morse_word_space
        morse_T
        morse_R
        morse_A
        morse_P
        morse_P
        morse_E
        morse_D
        morse_word_space
        morse_I
        morse_N
        morse_word_space
        morse_A
        morse_word_space
        morse_P
        morse_O
        morse_I
        morse_word_space
        morse_F
        morse_A
        morse_C
        morse_T
        morse_O
        morse_R
        morse_Y
        morse_exclaim
        morse_word_space
        endbab

NUM_MORSE_FRAMES equ num_frames
  if NUM_MORSE_FRAMES > 256
    messg "Morse code message takes up more than 256 animation frames."
  endif

PM_Pat_SMorse:
        pattern PATDF_4C, 4, 1, PATSF_BAPM_PMF, NUM_SMORSE_FRAMES
        dw BAPM_Pal_Basic, BAPM_Default_Ramps, 0
        bablock
num_frames = 0
        smorse_T
        smorse_I
        smorse_M
        smorse_E
        smorse_word_space
        smorse_F
        smorse_O
        smorse_R
        smorse_word_space
        smorse_T
        smorse_E
        smorse_A
        smorse_word_space
        smorse_A
        smorse_N
        smorse_D
        smorse_word_space
        smorse_B
        smorse_I
        smorse_S
        smorse_C
        smorse_U
        smorse_I
        smorse_T
        smorse_S
        smorse_exclaim
        smorse_word_space
        endbab

NUM_SMORSE_FRAMES equ num_frames
  if NUM_SMORSE_FRAMES > 256
    messg "S-Morse code message takes up more than 256 animation frames."
  endif

  endif


;------------------------------------------------------------------------------
; Pattern address table, enumerated
;------------------------------------------------------------------------------


PM_PatAddrTable:
  resetenum
  enumdat PAT_UNI_RAINBOW,        dw, PM_Pat_UniRainbow
  enumdat PAT_BAL_RAINBOW,        dw, PM_Pat_BalRainbow
  enumdat PAT_LAG_RAINBOW,        dw, PM_Pat_LagRainbow
  enumdat PAT_OUT_RAINBOW,        dw, PM_Pat_OutRainbow
  enumdat PAT_FAST_UNI_RAINBOW,   dw, PM_Pat_FastUniRainbow
  enumdat PAT_FAST_BAL_RAINBOW,   dw, PM_Pat_FastBalRainbow
  enumdat PAT_SOLID_STEPS,        dw, PM_Pat_SolidSteps
  enumdat PAT_SOLID,              dw, PM_Pat_Solid
  enumdat PAT_HAVE_YOU_SEEN_MY_POI, dw, PM_Pat_HaveYouSeenMyPoi
  enumdat PAT_FLASH1,             dw, PM_Pat_Flash1
  enumdat PAT_FLASH2,             dw, PM_Pat_Flash2
  enumdat PAT_FLASH3,             dw, PM_Pat_Flash3
  enumdat PAT_RIM_WTD_PACKED1,    dw, PM_Pat_RimWtdPacked1
  enumdat PAT_RIM_WTD_PACKED2,    dw, PM_Pat_RimWtdPacked2
  enumdat PAT_RIM_WTD_PACKED3,    dw, PM_Pat_RimWtdPacked3
  enumdat PAT_RIM_WTD_SPACED3,    dw, PM_Pat_RimWtdSpaced3
  enumdat PAT_RIM_WTD_FLASH1,     dw, PM_Pat_RimWtdFlash1
  enumdat PAT_RIM_WTD_FLASH2,     dw, PM_Pat_RimWtdFlash2
  enumdat PAT_RIM_WTD_FLASH3,     dw, PM_Pat_RimWtdFlash3
  enumdat PAT_HUB_WTD_PACKED1,    dw, PM_Pat_HubWtdPacked1
  enumdat PAT_HUB_WTD_PACKED2,    dw, PM_Pat_HubWtdPacked2
  enumdat PAT_HUB_WTD_PACKED3,    dw, PM_Pat_HubWtdPacked3
  enumdat PAT_HUB_WTD_SPACED3,    dw, PM_Pat_HubWtdSpaced3
  enumdat PAT_HUB_WTD_FLASH1,     dw, PM_Pat_HubWtdFlash1
  enumdat PAT_HUB_WTD_FLASH2,     dw, PM_Pat_HubWtdFlash2
  enumdat PAT_HUB_WTD_FLASH3,     dw, PM_Pat_HubWtdFlash3
  enumdat PAT_RAMP_UP2,           dw, PM_Pat_RampUp2
  enumdat PAT_RAMP_UP3,           dw, PM_Pat_RampUp3
  enumdat PAT_RAMP_DOWN2,         dw, PM_Pat_RampDown2
  enumdat PAT_RAMP_DOWN3,         dw, PM_Pat_RampDown3
  enumdat PAT_RAMP_UP_OUT,        dw, PM_Pat_RampUpOut
  enumdat PAT_RAMP_UP_IN,         dw, PM_Pat_RampUpIn
  enumdat PAT_FAT_RAMP_UP_OUT,    dw, PM_Pat_FatRampUpOut
  enumdat PAT_FAT_RAMP_UP_IN,     dw, PM_Pat_FatRampUpIn
  enumdat PAT_RAMP_DOWN_OUT,      dw, PM_Pat_RampDownOut
  enumdat PAT_RAMP_DOWN_IN,       dw, PM_Pat_RampDownIn
  enumdat PAT_FAT_RAMP_DOWN_OUT,  dw, PM_Pat_FatRampDownOut
  enumdat PAT_FAT_RAMP_DOWN_IN,   dw, PM_Pat_FatRampDownIn
  enumdat PAT_INTERDIGITATION_P,  dw, PM_Pat_InterdigitationP
  enumdat PAT_INTERDIGITATION_S,  dw, PM_Pat_InterdigitationS
  enumdat PAT_ALTERNATE,          dw, PM_Pat_Alternate
  enumdat PAT_ALT_SPACED2,        dw, PM_Pat_AltSpaced2
  enumdat PAT_ALT_SPACED3,        dw, PM_Pat_AltSpaced3
  enumdat PAT_JAMMY_BATTENBERG,   dw, PM_Pat_JammyBattenberg
  enumdat PAT_SOFT_FLASH2,        dw, PM_Pat_SoftFlash2
  enumdat PAT_SOFT_FLASH3,        dw, PM_Pat_SoftFlash3
  enumdat PAT_SOFT_FLASH2_X,      dw, PM_Pat_SoftFlash2X
  enumdat PAT_SOFT_FLASH3_X,      dw, PM_Pat_SoftFlash3X
  enumdat PAT_DOGS,               dw, PM_Pat_Dogs
  enumdat PAT_DOG_MEDLEY,         dw, PM_Pat_DogMedley
  enumdat PAT_SOFT_DOGS,          dw, PM_Pat_SoftDogs
  enumdat PAT_SOFT_DOG_MEDLEY,    dw, PM_Pat_SoftDogMedley
  enumdat PAT_SQUARES_FIXED5,     dw, PM_Pat_Squares_Fixed5
  enumdat PAT_SQUARES_USER2,      dw, PM_Pat_Squares_User2
  enumdat PAT_SQUARES_USER3,      dw, PM_Pat_Squares_User3
  enumdat PAT_QUANTUM_SLALOM_F6,  dw, PM_Pat_QuantumSlalom_Fixed6
  enumdat PAT_QUANTUM_SLALOM_U2,  dw, PM_Pat_QuantumSlalom_User2
  enumdat PAT_TERMINALS_U2F4,     dw, PM_Pat_Terminals_U2F4
  enumdat PAT_TERMINALS_USER2,    dw, PM_Pat_Terminals_User2
  enumdat PAT_CROWN,              dw, PM_Pat_Crown
  enumdat PAT_SUNBURST,           dw, PM_Pat_Sunburst
  enumdat PAT_TRI_BOUNCE,         dw, PM_Pat_TriBounce
  enumdat PAT_TRI_BOUNCE_SPACED,  dw, PM_Pat_TriBounceSpaced
  enumdat PAT_TRIANGLES_NEAR_ME,  dw, PM_Pat_TrianglesNearMe_Fixed5
  enumdat PAT_TNM_USER2,          dw, PM_Pat_TrianglesNearMe_User2
  enumdat PAT_TNM_USER3,          dw, PM_Pat_TrianglesNearMe_User3
  enumdat PAT_ZIGZAG,             dw, PM_Pat_ZigZag
  enumdat PAT_CONSUMMATE_VS,      dw, PM_Pat_ConsummateVs
  enumdat PAT_STEP,               dw, PM_Pat_Step
  enumdat PAT_SPOKES,             dw, PM_Pat_Spokes
  enumdat PAT_SPOKES2,            dw, PM_Pat_Spokes2
  enumdat PAT_SPOKES3,            dw, PM_Pat_Spokes3
  enumdat PAT_SPOTS_BARS,         dw, PM_Pat_SpotsBars
  enumdat PAT_RAD_CON,            dw, PM_Pat_RadCon
  enumdat PAT_TWIST,              dw, PM_Pat_Twist
  enumdat PAT_PIED_CAT_NOSES,     dw, PM_Pat_PiedCatNoses
  enumdat PAT_POLAR_CAT_NOSES,    dw, PM_Pat_PolarCatNoses
  enumdat PAT_CHAIN,              dw, PM_Pat_Chain
  enumdat PAT_BARBERPOLE2,        dw, PM_Pat_Barberpole2
  enumdat PAT_BARBERPOLE3,        dw, PM_Pat_Barberpole3
  enumdat PAT_BARBERPOLE3S,       dw, PM_Pat_Barberpole3S
  enumdat PAT_FAT_BARBERPOLE2,    dw, PM_Pat_FatBarberpole2
  enumdat PAT_FAT_BARBERPOLE3,    dw, PM_Pat_FatBarberpole3
  enumdat PAT_FAT_BARBERPOLE3S,   dw, PM_Pat_FatBarberpole3S
  enumdat PAT_SOFT_BARBERPOLE2,   dw, PM_Pat_SoftBarberpole2
  enumdat PAT_SOFT_BARBERPOLE3,   dw, PM_Pat_SoftBarberpole3
  enumdat PAT_SOFT_BARBERPOLE3S,  dw, PM_Pat_SoftBarberpole3S
  enumdat PAT_WAVY_SPOKES2,       dw, PM_Pat_WavySpokes2
  enumdat PAT_WAVY_SPOKES3,       dw, PM_Pat_WavySpokes3
  enumdat PAT_CHEVRONS2,          dw, PM_Pat_Chevrons2
  enumdat PAT_CHEVRONS3,          dw, PM_Pat_Chevrons3
  enumdat PAT_WEAVE,              dw, PM_Pat_Weave
  enumdat PAT_LUM_CRAWL,          dw, PM_Pat_LumCrawl
  enumdat PAT_CHEQUER,            dw, PM_Pat_Chequer
  enumdat PAT_BY_YOUR_COMMAND,    dw, PM_Pat_ByYourCommand
  enumdat PAT_BLUR_STEP,          dw, PM_Pat_BlurStep
  enumdat PAT_DECAY2,             dw, PM_Pat_Decay2
  enumdat PAT_DECAY3,             dw, PM_Pat_Decay3
  enumdat PAT_DECAY2_X,           dw, PM_Pat_Decay2X
  enumdat PAT_DECAY3_X,           dw, PM_Pat_Decay3X
  enumdat PAT_BOOKENDS,           dw, PM_Pat_Bookends
  enumdat PAT_BOOKENDS_X,         dw, PM_Pat_BookendsX
  enumdat PAT_SLIDES,             dw, PM_Pat_Slides
  enumdat PAT_ALIEN_CIRCUITRY,    dw, PM_Pat_AlienCircuitry
  enumdat PAT_TRIC_SOLID,         dw, PM_Pat_TriC_Solid
  enumdat PAT_TRIC_FLASH,         dw, PM_Pat_TriC_Flash
  enumdat PAT_TRIC_DASHES,        dw, PM_Pat_TriC_Dashes
  enumdat PAT_TRIR_SOLID,         dw, PM_Pat_TriR_Solid
  enumdat PAT_TRIR_SPACED,        dw, PM_Pat_TriR_Spaced
  enumdat PAT_TRI_CHASER,         dw, PM_Pat_TriChaser
  enumdat PAT_FIREFLY,            dw, PM_Pat_Firefly
  enumdat PAT_ICE_STORM,          dw, PM_Pat_IceStorm
  enumdat PAT_PULSE_JET,          dw, PM_Pat_PulseJet
  enumdat PAT_HUB_BRAZIER,        dw, PM_Pat_HubBrazier
  enumdat PAT_RIM_BRAZIER,        dw, PM_Pat_RimBrazier
  enumdat PAT_MID_BRAZIER,        dw, PM_Pat_MidBrazier
  enumdat PAT_HQ_HUB_BRAZIER,     dw, PM_Pat_HQHubBrazier
  enumdat PAT_HQ_RIM_BRAZIER,     dw, PM_Pat_HQRimBrazier
  enumdat PAT_HQ_MID_BRAZIER,     dw, PM_Pat_HQMidBrazier
  enumdat PAT_HQ_PULSEJET,        dw, PM_Pat_HQPulseJet
  if INCLUDE_MORSE
    enumdat PAT_MORSE,            dw, PM_Pat_Morse
    enumdat PAT_SMORSE,           dw, PM_Pat_SMorse
  endif
NUM_PATTERNS equ ENUMIX


;------------------------------------------------------------------------------
; Pattern bank storage macros
;------------------------------------------------------------------------------


; Whether a pattern's colours are fixed or modifiable with External Ramps
; is indicated by the External ramps mode bit in the bank pattern record.
; (The old system of "ER" display formats is no longer used). The pattern's
; Frame Unit Period is loaded from the bank pattern record so that it is
; no longer necessary to have many pattern headers differing only by their
; frame unit periods.
;
; Bank pattern entry format:
;
;                  bit      p = Enumerated pattern index (9 bits)
;               76543210    e = External ramps mode
;               --------    s = FUP scaler: 0..3 => (1, 2, 8, 32)
; First Byte:   pppppppp    T = Frame Unit Period - 1
; Second Byte:  eTAAAAAA    A = Colour ramp index A
; Third Byte:   TTBBBBBB    B = Colour ramp index B
; Fourth Byte:  ssCCCCCC    C = Colour ramp index C

pattable macro banknum
_ptbn = banknum
PM_PatTable_Bank#v(_ptbn):
  dw  NUM_PATS_IN_BANK#v(_ptbn)
  bablock
  endm

endpattable macro
NUM_PATS_IN_BANK#v(_ptbn) equ BAPM_BYTE_OFFSET / 4
  if NUM_PATS_IN_BANK#v(_ptbn) > 64
    error "Too many patterns in bank. (64 is the maximum.)"
  endif
  endbab
  endm

pat_fixed macro pat_id, fupscaler, fup
    dbab (pat_id)
    dbab (0 << 7) | (((((fup) - 1) >> 2) & 1) << 6)
    dbab ((((fup) - 1) & 3) << 6)
    dbab (((fupscaler) & 3) << 6)
  endm

pat_er macro pat_id, rampa, rampb, rampc, fupscaler, fup
    dbab (pat_id)
    dbab (1 << 7) | (((((fup - 1)) >> 2) & 1) << 6) | ((rampa) & 63)
    dbab ((((fup) - 1) & 3) << 6) | ((rampb) & 63)
    dbab (((fupscaler) & 3) << 6) | ((rampc) & 63)
  endm

banktable macro
    dw  NUM_BANKS
  endm

endbanktable macro
NUM_BANKS equ ($ - PM_BankTable - 1)
  if NUM_BANKS > 16
    error "Too many pattern banks! (16 is the maximum.)"
  endif
  if NUM_BANKS > 15
    if NUM_RAMPS > 63
      error "NUM_BANKS and NUM_BANKS must not both be at their maximums!"
      ; When NUM_BANKS = 16 and NUM_BANKS = 64, it is possible for the user
      ; to create a Favourite Pattern record that is all bit ones, which is
      ; the list terminator for the Favourites Patterns list.
    endif
  endif
  endm

patbank macro bankaddr
    dw  bankaddr
  endm


;------------------------------------------------------------------------------
; Pattern banks
;------------------------------------------------------------------------------


  pattable 0
    ; Rainbows and solid colours
    pat_fixed PAT_UNI_RAINBOW,           1, 8
    pat_fixed PAT_BAL_RAINBOW,           0, 1
    pat_fixed PAT_LAG_RAINBOW,           0, 3
    pat_fixed PAT_OUT_RAINBOW,           0, 4
    pat_fixed PAT_FAST_UNI_RAINBOW,      0, 1
    pat_fixed PAT_FAST_BAL_RAINBOW,      0, 3
    pat_fixed PAT_SOLID_STEPS,           3, 7
    pat_fixed PAT_SOLID_STEPS,           2, 4
    pat_fixed PAT_SOLID_STEPS,           1, 3
    pat_fixed PAT_SOLID_STEPS,           0, 2
    pat_er    PAT_SOLID,  R_RED,  0, 0,  0, 1
    pat_er    PAT_SOLID,  R_BLZ,  0, 0,  0, 1
    pat_er    PAT_SOLID,  R_ORA,  0, 0,  0, 1
    pat_er    PAT_SOLID,  R_YEL,  0, 0,  0, 1
    pat_er    PAT_SOLID,  R_SNT,  0, 0,  0, 1
    pat_er    PAT_SOLID,  R_GRN,  0, 0,  0, 1
    pat_er    PAT_SOLID,  R_SGR,  0, 0,  0, 1
    pat_er    PAT_SOLID,  R_CYN,  0, 0,  0, 1
    pat_er    PAT_SOLID,  R_SKY,  0, 0,  0, 1
    pat_er    PAT_SOLID,  R_BLU,  0, 0,  0, 1
    pat_er    PAT_SOLID,  R_PUR,  0, 0,  0, 1
    pat_er    PAT_SOLID,  R_VIO,  0, 0,  0, 1
    pat_er    PAT_SOLID,  R_MAG,  0, 0,  0, 1
    pat_er    PAT_SOLID,  R_CER,  0, 0,  0, 1
    pat_er    PAT_SOLID,  R_PNK,  0, 0,  0, 1
    pat_er    PAT_SOLID,  R_WHT,  0, 0,  0, 1
  endpattable

  pattable 1
    ; Pyromania
    pat_er    PAT_HQ_HUB_BRAZIER,   R16_BLAZE,   0, 0,  1, 2
    pat_er    PAT_HQ_MID_BRAZIER,   R16_BLAZE,   0, 0,  1, 2
    pat_er    PAT_HQ_RIM_BRAZIER,   R16_BLAZE,   0, 0,  1, 2
    pat_er    PAT_HQ_HUB_BRAZIER,   R16_FLAME,   0, 0,  0, 3
    pat_er    PAT_HQ_MID_BRAZIER,   R16_FLAME,   0, 0,  0, 3
    pat_er    PAT_HQ_RIM_BRAZIER,   R16_FLAME,   0, 0,  0, 3
    pat_er    PAT_HQ_HUB_BRAZIER,   R16_FLURPLE, 0, 0,  0, 3
    pat_er    PAT_HQ_MID_BRAZIER,   R16_FLURPLE, 0, 0,  0, 3
    pat_er    PAT_HQ_RIM_BRAZIER,   R16_FLURPLE, 0, 0,  0, 3
    pat_er    PAT_HUB_BRAZIER,      R_MINT,      0, 0,  1, 6
    pat_er    PAT_MID_BRAZIER,      R_MINT,      0, 0,  1, 6
    pat_er    PAT_RIM_BRAZIER,      R_MINT,      0, 0,  1, 6
    pat_er    PAT_HQ_PULSEJET,      R16_BUNSEN,  0, 0,  0, 4
    pat_er    PAT_PULSE_JET,        R_BUNSEN_E,  0, 0,  1, 4
    pat_er    PAT_PULSE_JET,        R_STRAWBRY,  0, 0,  0, 4
  endpattable

  pattable 2
    ; One flat colour
    pat_er    PAT_ZIGZAG,           R_VIO,  0,      0,      2, 1
    pat_er    PAT_ZIGZAG,           R_ORA,  0,      0,      1, 2
    pat_er    PAT_ZIGZAG,           R_CYN,  0,      0,      0, 1
    pat_er    PAT_FLASH1,           R_VIO,  0,      0,      1, 3
    pat_er    PAT_FLASH1,           R_SKY,  0,      0,      0, 2
    pat_er    PAT_STEP,             R_GRN,  0,      0,      1, 3
    pat_er    PAT_STEP,             R_BLZ,  0,      0,      1, 2
    pat_er    PAT_STEP,             R_BLU,  0,      0,      0, 3
    pat_er    PAT_STEP,             R_YEL,  0,      0,      0, 2
    pat_er    PAT_STEP,             R_SKY,  0,      0,      0, 1
    pat_er    PAT_SPOKES,           R_VIO,  0,      0,      1, 2
    pat_er    PAT_SPOKES,           R_RED,  0,      0,      0, 3
    pat_er    PAT_SPOKES,           R_YEL,  0,      0,      0, 2
    pat_er    PAT_SPOKES,           R_SKY,  0,      0,      0, 1
  endpattable

  pattable 3
    ; Two or three flat colours, Set A
    pat_er    PAT_FLASH2,           R_BLU1, R_CYN,  0,      1, 2
    pat_er    PAT_FLASH2,           R_RED,  R_SKY,  0,      1, 3
    pat_er    PAT_FLASH2,           R_RED,  R_CER,  0,      0, 3
    pat_er    PAT_FLASH3,           R_GRN,  R_BLU1, R_ORA,  1, 2
    pat_er    PAT_FLASH3,           R_YEL,  R_SKY,  R_RED,  0, 2
    pat_er    PAT_ALTERNATE,        R_YEL,  R_RED1, 0,      1, 5
    pat_er    PAT_ALTERNATE,        R_BLU1, R_SNT,  0,      0, 5
    pat_er    PAT_ALT_SPACED2,      R_YEL,  R_WHT,  0,      1, 3
    pat_er    PAT_ALT_SPACED2,      R_RED,  R_GRN1, 0,      0, 3
    pat_er    PAT_ALT_SPACED3,      R_RED,  R_GRN,  R_BLU,  1, 2
    pat_er    PAT_ALT_SPACED3,      R_RED,  R_WHT,  R_GRN1, 0, 3
    pat_er    PAT_SQUARES_USER2,    R_RED,  R_WHT,  0,      0, 4
    pat_er    PAT_SQUARES_USER3,    R_GRN,  R_ORA,  R_SKY,  0, 4
    pat_er    PAT_QUANTUM_SLALOM_U2,R_CER,  R_GRN,  R_WHT,  0, 2
    pat_er    PAT_TERMINALS_USER2,  R_BLZ,  R_BLZ,  R_BLU1, 1, 2
    pat_er    PAT_TERMINALS_USER2,  R_RED,  R_SKY,  R_BLK,  0, 2
    pat_er    PAT_TRI_BOUNCE,       R_GRN,  R_ORA,  R_RED,  1, 3
    pat_er    PAT_TRI_BOUNCE,       R_YEL,  R_CYN,  R_YEL,  0, 3
    pat_er    PAT_TRI_BOUNCE_SPACED,R_RED,  R_GRN1, R_BLU,  1, 3
    pat_er    PAT_TRI_BOUNCE_SPACED,R_PUR,  R_WHT,  R_PUR,  0, 3
    pat_er    PAT_CROWN,            R_RED,  R_BLZ,  R_YEL,  1, 3
    pat_er    PAT_CROWN,            R_BLU1, R_CER,  R_SGR,  0, 3
    pat_er    PAT_SUNBURST,         R_YEL,  R_YEL,  R_YEL,  1, 3
    pat_er    PAT_SUNBURST,         R_RED1, R_CER,  R_WHT,  0, 3
    pat_er    PAT_JAMMY_BATTENBERG, R_BLU1, R_ORA1, R_YEL,  1, 3
  endpattable

  pattable 4
    ; Two or three flat colours, Set B
    pat_er    PAT_DOGS,             R_ORA,  R_VIO,  0,      1, 2
    pat_er    PAT_DOGS,             R_YEL,  R_SKY,  0,      0, 3
    pat_er    PAT_DOGS,             R_CYN,  R_CER,  0,      0, 2
    pat_er    PAT_DOGS,             R_SNT,  R_RED1, 0,      0, 1
    pat_er    PAT_RAD_CON,          R_SKY,  R_BLZ,  0,      0, 2
    pat_er    PAT_RAD_CON,          R_WHT,  R_GRN1, 0,      1, 1
    pat_er    PAT_TNM_USER2,        R_BLZ,  R_SKY,  0,      0, 4
    pat_er    PAT_TNM_USER3,        R_BLU1, R_YEL,  R_CER,  0, 3
    pat_er    PAT_CONSUMMATE_VS,    R_YEL,  R_SKY,  0,      1, 2
    pat_er    PAT_CONSUMMATE_VS,    R_SKY,  R_WHT,  0,      0, 2
    pat_er    PAT_CONSUMMATE_VS,    R_GRN1, R_RED,  0,      0, 1
    pat_er    PAT_CHEQUER,          R_VIO,  R_BLK,  0,      1, 4
    pat_er    PAT_CHEQUER,          R_RED1, R_YEL,  0,      0, 5
    pat_er    PAT_BARBERPOLE2,      R_RED,  R_GRN1, 0,      0, 3
    pat_er    PAT_BARBERPOLE2,      R_BLZ,  R_CYN,  0,      1, 3
    pat_er    PAT_BARBERPOLE3,      R_CER,  R_YEL,  R_BLU1, 0, 3
    pat_er    PAT_BARBERPOLE3,      R_RED1, R_WHT,  R_BLU1, 1, 3
    pat_er    PAT_BARBERPOLE3S,     R_BLU1, R_WHT,  R_BLU1, 0, 3
    pat_er    PAT_BARBERPOLE3S,     R_RED1, R_YEL,  R_GRN1, 1, 3
    pat_er    PAT_FAT_BARBERPOLE2,  R_BLZ,  R_SKY,  0,      0, 2
    pat_er    PAT_FAT_BARBERPOLE2,  R_ORA,  R_VIO1, 0,      1, 2
    pat_er    PAT_FAT_BARBERPOLE3,  R_YEL,  R_MAG,  R_SKY,  0, 2
    pat_er    PAT_FAT_BARBERPOLE3,  R_RED1, R_WHT,  R_BLU1, 1, 2
    pat_er    PAT_FAT_BARBERPOLE3S, R_CER,  R_WHT,  R_GRN1, 0, 2
    pat_er    PAT_FAT_BARBERPOLE3S, R_RED1, R_YEL,  R_BLU1, 1, 2
  endpattable

  pattable 5
    ; Two or three colour ramps, Set A
    pat_er    PAT_WEAVE,            R_FIRE, R_ICE,  R_FIRE, 0, 2
    pat_er    PAT_WEAVE,            R_ICE,  R_STRAWBRY, R_ICE, 0, 1
    pat_er    PAT_WEAVE,            R_TIEDYED_A, R_TIEDYED_B, R_TIEDYED_A, 0, 2
    pat_er    PAT_WEAVE,            R_SKY,  R_SKY,  R_SKY,  0, 2
    pat_er    PAT_SPOTS_BARS,       R_SKY,  R_RED,  R_WHT,  0, 2
    pat_er    PAT_SOFT_FLASH2,      R_YEL,  R_BLZ,  0,      0, 1
    pat_er    PAT_SOFT_FLASH2,      R_GOLD, R_WHT,  0,      0, 2
    pat_er    PAT_SOFT_FLASH2,      R_FIRE, R_ICE,  0,      0, 1
    pat_er    PAT_SOFT_FLASH3,      R_RED,  R_YEL,  R_BLU,  0, 2
    pat_er    PAT_SOFT_FLASH2_X,    R_STRAWBRY, R_GOLD, 0,  0, 1
    pat_er    PAT_SOFT_FLASH3_X,    R_GRN,  R_BLU1, R_ORA,  0, 2
    pat_er    PAT_SOFT_DOGS,        R_TIEDYED_A, R_TIEDYED_B, 0, 0, 4
    pat_er    PAT_SOFT_DOGS,        R_BLURPLE, R_BLURPLE, 0, 0, 3
    pat_er    PAT_SOFT_DOGS,        R_ICE,  R_STRAWBRY, 0,  0, 3
    pat_er    PAT_SOFT_DOGS,        R_SKY,  R_WHT,  0,      0, 1
    pat_er    PAT_SOFT_DOGS,        R_FIRE, R_ICE,  0,      0, 1
    pat_er    PAT_WAVY_SPOKES2,     R_RED,  R_CER,  0,      0, 1
    pat_er    PAT_WAVY_SPOKES2,     R_FIRE, R_ICE,  0,      0, 1
    pat_er    PAT_WAVY_SPOKES2,     R_SKY,  R_WHT,  0,      0, 1
    pat_er    PAT_WAVY_SPOKES3,     R_SNT,  R_VIO1, R_ORA,  0, 1
    pat_er    PAT_SOFT_BARBERPOLE2, R_RED,  R_WHT,  0,      0, 2
    pat_er    PAT_SOFT_BARBERPOLE3, R_FIRE, R_WHT,  R_ICE,  0, 2
    pat_er    PAT_SOFT_BARBERPOLE3S,R_STRAWBRY, R_WHT, R_STRAWBRY, 0, 1
  endpattable

  pattable 6
    ; Two or three colour ramps, Set B
    pat_er    PAT_LUM_CRAWL,        R_FIRE, R_ICE,  0,      0, 5
    pat_er    PAT_LUM_CRAWL,        R_BLURPLE, R_BLURPLE, 0, 0, 2
    pat_er    PAT_LUM_CRAWL,        R_ORA,  R_RED,  0,      0, 3
    pat_er    PAT_CHAIN,            R_FIRE, R_ICE,  0,      0, 2
    pat_er    PAT_TWIST,            R_CYN,  R_ORA,  0,      0, 2
    pat_er    PAT_PIED_CAT_NOSES,   R_GOLD, R_BLU1, 0,      0, 2
    pat_er    PAT_POLAR_CAT_NOSES,  R_FIRE, R_ICE,  0,      0, 2
    pat_er    PAT_BOOKENDS,         R_FIRE, R_ICE,  0,      0, 1
    pat_er    PAT_BOOKENDS_X,       R_BLU,  R_RED,  0,      0, 1
    pat_er    PAT_CHEVRONS2,        R_SKY,  R_WHT,  0,      0, 1
    pat_er    PAT_CHEVRONS2,        R_GOLD, R_WHT,  0,      0, 1
    pat_er    PAT_CHEVRONS2,        R_RED_MIST, R_RED_MIST, 0, 0, 1
    pat_er    PAT_CHEVRONS2,        R_BUNSEN, R_BUNSEN, 0,  0, 1
    pat_er    PAT_CHEVRONS2,        R_FLAME, R_VIO1, 0,     0, 1
    pat_er    PAT_CHEVRONS3,        R_RED,  R_WHT,  R_GRN1, 0, 1
    pat_er    PAT_BLUR_STEP,        R_FLAME, R_FLAME, 0,    0, 3
    pat_er    PAT_BLUR_STEP,        R_SKY,  R_WHT,  0,      0, 2
    pat_er    PAT_BLUR_STEP,        R_GOLD, R_WHT,  0,      0, 1
    pat_er    PAT_DECAY2,           R_FIRE, R_ICE,  0,      0, 2
    pat_er    PAT_DECAY3,           R_RED,  R_GRN,  BLU,    0, 2
    pat_er    PAT_DECAY2_X,         R_BUNSEN, R_BUNSEN, 0,  0, 1
    pat_er    PAT_DECAY3_X,         R_BLU,  R_MAG,  R_RED,  0, 1
    pat_er    PAT_SPOKES2,          R_BLZ,  R_SKY,  0,      1, 1
    pat_er    PAT_SPOKES2,          R_YEL,  R_SKY,  0,      0, 1
    pat_er    PAT_SPOKES3,          R_YEL,  R_RED1, R_SKY,  0, 1
  endpattable

  pattable 7
    ; Miscellaneous
    pat_fixed PAT_BY_YOUR_COMMAND,    0, 1
    pat_er    PAT_TRIC_SOLID,         R_BLK,  R_BLK,  R_RED,  0, 1
    pat_er    PAT_ALIEN_CIRCUITRY,    R_RED,  R_BLU,  R_WHT,  0, 2
    pat_fixed PAT_FIREFLY,            0, 1
    pat_fixed PAT_ICE_STORM,          0, 2
    pat_er    PAT_DOG_MEDLEY,         R_GRN,  R_CER,  R_YEL,  1, 2
    pat_er    PAT_DOG_MEDLEY,         R_VIO,  R_YEL,  R_ORA,  0, 1
    pat_er    PAT_SOFT_DOG_MEDLEY,    R_GRN,  R_CER,  R_YEL,  1, 2
    pat_er    PAT_SOFT_DOG_MEDLEY,    R_VIO,  R_YEL,  R_ORA,  0, 1
    pat_fixed PAT_SQUARES_FIXED5,     0, 4
    pat_fixed PAT_QUANTUM_SLALOM_F6,  0, 2
    pat_er    PAT_TERMINALS_U2F4,     R_GRN,  R_CER,  R_BLK,  0, 2
    pat_fixed PAT_TRIANGLES_NEAR_ME,  0, 4
    pat_er    PAT_SLIDES,             R_BLZ,  R_SKY,  0,      2, 1
    pat_er    PAT_SLIDES,             R_YEL,  R_RED1, 0,      1, 3
    if INCLUDE_MORSE
      pat_er    PAT_SMORSE,           R_GOLD, 0,      0,      0, 1
      pat_er    PAT_SMORSE,           R_ICE,  0,      0,      3, 2
      pat_er    PAT_MORSE,            R_GRN,  0,      0,      0, 2
      pat_er    PAT_MORSE,            R_STRAWBRY, 0,  0,      2, 4
      pat_er    PAT_MORSE,            R_FIRE, 0,      0,      3, 2
    endif
    pat_er  PAT_HAVE_YOU_SEEN_MY_POI, R_RED,  0,      0,      0, 1
  endpattable

  pattable 8
    ; Tricolours - Concentric
    pat_er    PAT_TRIC_SOLID,   R_BLU,  R_YEL,  R_CER,  0, 1
    pat_er    PAT_TRIC_SOLID,   R_SKY,  R_WHT,  R_CER,  0, 1
    pat_er    PAT_TRIC_SOLID,   R_BLU,  R_PNK,  R_RED,  0, 1
    pat_er    PAT_TRIC_SOLID,   R_RED,  R_GRN,  R_BLU,  0, 1
    pat_er    PAT_TRIC_SOLID,   R_RED,  R_WHT,  R_BLU,  0, 1
    pat_er    PAT_TRIC_SOLID,   R_CYN,  R_MAG,  R_YEL,  0, 1
    pat_er    PAT_TRIC_SOLID,   R_BLU,  R_WHT,  R_RED,  0, 1
    pat_er    PAT_TRIC_SOLID,   R_RED,  R_YEL,  R_GRN1, 0, 1
    pat_er    PAT_TRIC_SOLID,   R_RED,  R_WHT,  R_GRN1, 0, 1
    pat_er    PAT_TRIC_SOLID,   R_ORA1, R_WHT,  R_GRN1, 0, 1
    pat_er    PAT_TRIC_SOLID,   R_RED,  R_WHT,  R_RED,  0, 1
    pat_er    PAT_TRIC_SOLID,   R_GRN1, R_WHT,  R_GRN1, 0, 1
    pat_er    PAT_TRIC_SOLID,   R_CYN,  R_WHT,  R_CYN,  0, 1
    pat_er    PAT_TRIC_SOLID,   R_BLU1, R_WHT,  R_BLU1, 0, 1
    pat_er    PAT_TRIC_SOLID,   R_CYN,  R_SKY,  R_BLU,  0, 1
    pat_er    PAT_TRIC_FLASH,   R_BLU,  R_YEL,  R_CER,  2, 1
    pat_er    PAT_TRIC_FLASH,   R_SKY,  R_WHT,  R_CER,  2, 1
    pat_er    PAT_TRIC_FLASH,   R_BLU,  R_PNK,  R_RED,  2, 1
    pat_er    PAT_TRIC_FLASH,   R_RED,  R_GRN,  R_BLU,  2, 1
    pat_er    PAT_TRIC_FLASH,   R_RED,  R_WHT,  R_BLU,  2, 1
    pat_er    PAT_TRIC_FLASH,   R_CYN,  R_MAG,  R_YEL,  2, 1
    pat_er    PAT_TRIC_FLASH,   R_BLU,  R_WHT,  R_RED,  2, 1
    pat_er    PAT_TRIC_FLASH,   R_RED,  R_YEL,  R_GRN1, 2, 1
    pat_er    PAT_TRIC_FLASH,   R_RED,  R_WHT,  R_GRN1, 2, 1
    pat_er    PAT_TRIC_FLASH,   R_ORA1, R_WHT,  R_GRN1, 2, 1
    pat_er    PAT_TRIC_FLASH,   R_RED,  R_WHT,  R_RED,  2, 1
    pat_er    PAT_TRIC_FLASH,   R_GRN1, R_WHT,  R_GRN1, 2, 1
    pat_er    PAT_TRIC_FLASH,   R_CYN,  R_WHT,  R_CYN,  2, 1
    pat_er    PAT_TRIC_FLASH,   R_BLU1, R_WHT,  R_BLU1, 2, 1
    pat_er    PAT_TRIC_FLASH,   R_CYN,  R_SKY,  R_BLU,  2, 1
    pat_er    PAT_TRIC_FLASH,   R_BLU,  R_YEL,  R_CER,  1, 2
    pat_er    PAT_TRIC_FLASH,   R_SKY,  R_WHT,  R_CER,  1, 2
    pat_er    PAT_TRIC_FLASH,   R_BLU,  R_PNK,  R_RED,  1, 2
    pat_er    PAT_TRIC_FLASH,   R_RED,  R_GRN,  R_BLU,  1, 2
    pat_er    PAT_TRIC_FLASH,   R_RED,  R_WHT,  R_BLU,  1, 2
    pat_er    PAT_TRIC_FLASH,   R_CYN,  R_MAG,  R_YEL,  1, 2
    pat_er    PAT_TRIC_FLASH,   R_BLU,  R_WHT,  R_RED,  1, 2
    pat_er    PAT_TRIC_FLASH,   R_RED,  R_YEL,  R_GRN1, 1, 2
    pat_er    PAT_TRIC_FLASH,   R_RED,  R_WHT,  R_GRN1, 1, 2
    pat_er    PAT_TRIC_FLASH,   R_ORA1, R_WHT,  R_GRN1, 1, 2
    pat_er    PAT_TRIC_FLASH,   R_RED,  R_WHT,  R_RED,  1, 2
    pat_er    PAT_TRIC_FLASH,   R_GRN1, R_WHT,  R_GRN1, 1, 2
    pat_er    PAT_TRIC_FLASH,   R_CYN,  R_WHT,  R_CYN,  1, 2
    pat_er    PAT_TRIC_FLASH,   R_BLU1, R_WHT,  R_BLU1, 1, 2
    pat_er    PAT_TRIC_FLASH,   R_CYN,  R_SKY,  R_BLU,  1, 2
    pat_er    PAT_TRIC_DASHES,  R_BLU,  R_YEL,  R_CER,  1, 3
    pat_er    PAT_TRIC_DASHES,  R_SKY,  R_WHT,  R_CER,  1, 3
    pat_er    PAT_TRIC_DASHES,  R_BLU,  R_PNK,  R_RED,  1, 3
    pat_er    PAT_TRIC_DASHES,  R_RED,  R_GRN,  R_BLU,  1, 3
    pat_er    PAT_TRIC_DASHES,  R_RED,  R_WHT,  R_BLU,  1, 3
    pat_er    PAT_TRIC_DASHES,  R_CYN,  R_MAG,  R_YEL,  1, 3
    pat_er    PAT_TRIC_DASHES,  R_BLU,  R_WHT,  R_RED,  1, 3
    pat_er    PAT_TRIC_DASHES,  R_RED,  R_YEL,  R_GRN1, 1, 3
    pat_er    PAT_TRIC_DASHES,  R_RED,  R_WHT,  R_GRN1, 1, 3
    pat_er    PAT_TRIC_DASHES,  R_ORA1, R_WHT,  R_GRN1, 1, 3
    pat_er    PAT_TRIC_DASHES,  R_RED,  R_WHT,  R_RED,  1, 3
    pat_er    PAT_TRIC_DASHES,  R_GRN1, R_WHT,  R_GRN1, 1, 3
    pat_er    PAT_TRIC_DASHES,  R_CYN,  R_WHT,  R_CYN,  1, 3
    pat_er    PAT_TRIC_DASHES,  R_BLU1, R_WHT,  R_BLU1, 1, 3
    pat_er    PAT_TRIC_DASHES,  R_CYN,  R_SKY,  R_BLU,  1, 3
  endpattable

  pattable 9
    ; Tricolours - Chasers and radial bars
    pat_er    PAT_TRI_CHASER,   R_BLU,  R_YEL,  R_CER,  3, 5
    pat_er    PAT_TRI_CHASER,   R_SKY,  R_WHT,  R_CER,  3, 5
    pat_er    PAT_TRI_CHASER,   R_BLU,  R_PNK,  R_RED,  3, 5
    pat_er    PAT_TRI_CHASER,   R_RED,  R_GRN,  R_BLU,  3, 5
    pat_er    PAT_TRI_CHASER,   R_RED,  R_WHT,  R_BLU,  3, 5
    pat_er    PAT_TRI_CHASER,   R_CYN,  R_MAG,  R_YEL,  3, 5
    pat_er    PAT_TRI_CHASER,   R_BLU,  R_WHT,  R_RED,  3, 5
    pat_er    PAT_TRI_CHASER,   R_RED,  R_YEL,  R_GRN1, 3, 5
    pat_er    PAT_TRI_CHASER,   R_RED,  R_WHT,  R_GRN1, 3, 5
    pat_er    PAT_TRI_CHASER,   R_ORA1, R_WHT,  R_GRN1, 3, 5
    pat_er    PAT_TRI_CHASER,   R_RED,  R_WHT,  R_RED,  3, 5
    pat_er    PAT_TRI_CHASER,   R_GRN1, R_WHT,  R_GRN1, 3, 5
    pat_er    PAT_TRI_CHASER,   R_CYN,  R_WHT,  R_CYN,  3, 5
    pat_er    PAT_TRI_CHASER,   R_BLU1, R_WHT,  R_BLU1, 3, 5
    pat_er    PAT_TRI_CHASER,   R_CYN,  R_SKY,  R_BLU,  3, 5
    pat_er    PAT_TRIR_SOLID,   R_BLU,  R_YEL,  R_CER,  1, 4
    pat_er    PAT_TRIR_SOLID,   R_SKY,  R_WHT,  R_CER,  1, 4
    pat_er    PAT_TRIR_SOLID,   R_BLU,  R_PNK,  R_RED,  1, 4
    pat_er    PAT_TRIR_SOLID,   R_RED,  R_GRN,  R_BLU,  1, 4
    pat_er    PAT_TRIR_SOLID,   R_RED,  R_WHT,  R_BLU,  1, 4
    pat_er    PAT_TRIR_SOLID,   R_CYN,  R_MAG,  R_YEL,  1, 4
    pat_er    PAT_TRIR_SOLID,   R_BLU,  R_WHT,  R_RED,  1, 4
    pat_er    PAT_TRIR_SOLID,   R_RED,  R_YEL,  R_GRN1, 1, 4
    pat_er    PAT_TRIR_SOLID,   R_RED,  R_WHT,  R_GRN1, 1, 4
    pat_er    PAT_TRIR_SOLID,   R_ORA1, R_WHT,  R_GRN1, 1, 4
    pat_er    PAT_TRIR_SOLID,   R_RED,  R_WHT,  R_RED,  1, 4
    pat_er    PAT_TRIR_SOLID,   R_GRN1, R_WHT,  R_GRN1, 1, 4
    pat_er    PAT_TRIR_SOLID,   R_CYN,  R_WHT,  R_CYN,  1, 4
    pat_er    PAT_TRIR_SOLID,   R_BLU1, R_WHT,  R_BLU1, 1, 4
    pat_er    PAT_TRIR_SOLID,   R_CYN,  R_SKY,  R_BLU,  1, 4
    pat_er    PAT_TRIR_SPACED,  R_BLU,  R_YEL,  R_CER,  1, 4
    pat_er    PAT_TRIR_SPACED,  R_SKY,  R_WHT,  R_CER,  1, 4
    pat_er    PAT_TRIR_SPACED,  R_BLU,  R_PNK,  R_RED,  1, 4
    pat_er    PAT_TRIR_SPACED,  R_RED,  R_GRN,  R_BLU,  1, 4
    pat_er    PAT_TRIR_SPACED,  R_RED,  R_WHT,  R_BLU,  1, 4
    pat_er    PAT_TRIR_SPACED,  R_CYN,  R_MAG,  R_YEL,  1, 4
    pat_er    PAT_TRIR_SPACED,  R_BLU,  R_WHT,  R_RED,  1, 4
    pat_er    PAT_TRIR_SPACED,  R_RED,  R_YEL,  R_GRN1, 1, 4
    pat_er    PAT_TRIR_SPACED,  R_RED,  R_WHT,  R_GRN1, 1, 4
    pat_er    PAT_TRIR_SPACED,  R_ORA1, R_WHT,  R_GRN1, 1, 4
    pat_er    PAT_TRIR_SPACED,  R_RED,  R_WHT,  R_RED,  1, 4
    pat_er    PAT_TRIR_SPACED,  R_GRN1, R_WHT,  R_GRN1, 1, 4
    pat_er    PAT_TRIR_SPACED,  R_CYN,  R_WHT,  R_CYN,  1, 4
    pat_er    PAT_TRIR_SPACED,  R_BLU1, R_WHT,  R_BLU1, 1, 4
    pat_er    PAT_TRIR_SPACED,  R_CYN,  R_SKY,  R_BLU,  1, 4
    pat_er    PAT_TRIR_SPACED,  R_BLU,  R_YEL,  R_CER,  0, 4
    pat_er    PAT_TRIR_SPACED,  R_SKY,  R_WHT,  R_CER,  0, 4
    pat_er    PAT_TRIR_SPACED,  R_BLU,  R_PNK,  R_RED,  0, 4
    pat_er    PAT_TRIR_SPACED,  R_RED,  R_GRN,  R_BLU,  0, 4
    pat_er    PAT_TRIR_SPACED,  R_RED,  R_WHT,  R_BLU,  0, 4
    pat_er    PAT_TRIR_SPACED,  R_CYN,  R_MAG,  R_YEL,  0, 4
    pat_er    PAT_TRIR_SPACED,  R_BLU,  R_WHT,  R_RED,  0, 4
    pat_er    PAT_TRIR_SPACED,  R_RED,  R_YEL,  R_GRN1, 0, 4
    pat_er    PAT_TRIR_SPACED,  R_RED,  R_WHT,  R_GRN1, 0, 4
    pat_er    PAT_TRIR_SPACED,  R_ORA1, R_WHT,  R_GRN1, 0, 4
    pat_er    PAT_TRIR_SPACED,  R_RED,  R_WHT,  R_RED,  0, 4
    pat_er    PAT_TRIR_SPACED,  R_GRN1, R_WHT,  R_GRN1, 0, 4
    pat_er    PAT_TRIR_SPACED,  R_CYN,  R_WHT,  R_CYN,  0, 4
    pat_er    PAT_TRIR_SPACED,  R_BLU1, R_WHT,  R_BLU1, 0, 4
    pat_er    PAT_TRIR_SPACED,  R_CYN,  R_SKY,  R_BLU,  0, 4
  endpattable

  pattable 10
    ; Colour ramps as tricolours, Set A
    pat_er    PAT_RIM_WTD_PACKED1,    R_STRAWBRY, 0,  0,      0, 1
    pat_er    PAT_RIM_WTD_PACKED2,    R_FIRE, R_ICE,  0,      1, 4
    pat_er    PAT_RIM_WTD_PACKED2,    R_BLU1, R_SKY,  0,      0, 3
    pat_er    PAT_RIM_WTD_PACKED3,    R_GRN,  R_BLU1, R_ORA,  1, 3
    pat_er    PAT_RIM_WTD_PACKED3,    R_RED,  R_WHT,  R_BLU1, 0, 2
    pat_er    PAT_RIM_WTD_SPACED3,    R_BLU,  R_YEL,  R_CER,  1, 3
    pat_er    PAT_RIM_WTD_SPACED3,    R_PUR,  R_WHT,  R_PUR,  0, 2
    pat_er    PAT_RIM_WTD_FLASH1,     R_BLURPLE, 0,   0,      1, 4
    pat_er    PAT_RIM_WTD_FLASH1,     R_CER,  0,      0,      0, 2
    pat_er    PAT_RIM_WTD_FLASH2,     R_GOLD, R_WHT,  0,      1, 3
    pat_er    PAT_RIM_WTD_FLASH2,     R_SKY,  R_CER,  0,      0, 2
    pat_er    PAT_RIM_WTD_FLASH3,     R_RED1, R_WHT,  R_GRN1, 1, 3
    pat_er    PAT_RIM_WTD_FLASH3,     R_BLZ,  R_WHT,  R_SKY,  0, 2
    pat_er    PAT_INTERDIGITATION_P,  R_VIO,  R_ORA,  0,      1, 3
    pat_er    PAT_INTERDIGITATION_P,  R_YEL,  R_CYN,  0,      0, 3
    pat_er    PAT_RAMP_DOWN2,         R_FIRE, R_ICE,  0,      0, 3
    pat_er    PAT_RAMP_DOWN3,         R_RED,  R_GRN,  R_BLU,  0, 2
    pat_er    PAT_FAT_RAMP_DOWN_OUT,  R_GOLD, 0,      0,      0, 2
    pat_er    PAT_FAT_RAMP_DOWN_IN,   R_MINT, 0,      0,      0, 2
    pat_er    PAT_RAMP_DOWN_OUT,      R_STRAWBRY, 0,  0,      0, 3
    pat_er    PAT_RAMP_DOWN_IN,       R_ICE,  0,      0,      0, 3
  endpattable

  pattable 11
    ; Colour ramps as tricolours, Set B
    pat_er    PAT_HUB_WTD_PACKED1,    R_STRAWBRY, 0,  0,      0, 1
    pat_er    PAT_HUB_WTD_PACKED2,    R_FIRE, R_ICE,  0,      1, 4
    pat_er    PAT_HUB_WTD_PACKED2,    R_BLU1, R_SKY,  0,      0, 3
    pat_er    PAT_HUB_WTD_PACKED3,    R_GRN,  R_BLU1, R_ORA,  1, 3
    pat_er    PAT_HUB_WTD_PACKED3,    R_RED,  R_WHT,  R_BLU1, 0, 2
    pat_er    PAT_HUB_WTD_SPACED3,    R_BLU,  R_YEL,  R_CER,  1, 3
    pat_er    PAT_HUB_WTD_SPACED3,    R_PUR,  R_WHT,  R_PUR,  0, 2
    pat_er    PAT_HUB_WTD_FLASH1,     R_BLURPLE, 0,   0,      1, 4
    pat_er    PAT_HUB_WTD_FLASH1,     R_CER,  0,      0,      0, 2
    pat_er    PAT_HUB_WTD_FLASH2,     R_GOLD, R_WHT,  0,      1, 3
    pat_er    PAT_HUB_WTD_FLASH2,     R_SKY,  R_CER,  0,      0, 2
    pat_er    PAT_HUB_WTD_FLASH3,     R_RED1, R_WHT,  R_GRN1, 1, 3
    pat_er    PAT_HUB_WTD_FLASH3,     R_BLZ,  R_WHT,  R_SKY,  0, 2
    pat_er    PAT_INTERDIGITATION_S,  R_VIO,  R_ORA,  0,      1, 3
    pat_er    PAT_INTERDIGITATION_S,  R_YEL,  R_CYN,  0,      0, 2
    pat_er    PAT_RAMP_UP2,           R_FIRE, R_ICE,  0,      0, 3
    pat_er    PAT_RAMP_UP3,           R_RED,  R_GRN,  R_BLU,  0, 2
    pat_er    PAT_FAT_RAMP_UP_OUT,    R_GOLD, 0,      0,      0, 2
    pat_er    PAT_FAT_RAMP_UP_IN,     R_MINT, 0,      0,      0, 2
    pat_er    PAT_RAMP_UP_OUT,        R_STRAWBRY, 0,  0,      0, 3
    pat_er    PAT_RAMP_UP_IN,         R_ICE,  0,      0,      0, 3
  endpattable


;------------------------------------------------------------------------------


PM_BankTable:
  banktable
    patbank PM_PatTable_Bank0  ; Rainbows and solid colours
    patbank PM_PatTable_Bank1  ; Pyromania
    patbank PM_PatTable_Bank2  ; One flat colour
    patbank PM_PatTable_Bank3  ; Two or three flat colours, Set A
    patbank PM_PatTable_Bank4  ; Two or three flat colours, Set B
    patbank PM_PatTable_Bank5  ; Two or three colour ramps, Set A
    patbank PM_PatTable_Bank6  ; Two or three colour ramps, Set B
    patbank PM_PatTable_Bank7  ; Miscellaneous
    patbank PM_PatTable_Bank8  ; Tricolours - Concentric
    patbank PM_PatTable_Bank9  ; Tricolours - Chasers and radial bars
    patbank PM_PatTable_Bank10 ; Colour ramps as tricolours, Set A
    patbank PM_PatTable_Bank11 ; Colour ramps as tricolours, Set B
  endbanktable


;------------------------------------------------------------------------------


PM_SlideshowIntvTable:
        dw  0
        dw  SSHOW_INTV_1_PWM_CYCLES_14
        dw  SSHOW_INTV_2_PWM_CYCLES_14
        dw  SSHOW_INTV_3_PWM_CYCLES_14


;------------------------------------------------------------------------------

Pages1to3CodeEnd:
Pages1to3WordsUsed = Pages1to3CodeEnd - 0x0800
Pages1to3WordsFree = 0x2000 - Pages1to3CodeEnd
 if Pages1to3WordsFree < 0x0100
   messg "Less than 256 words of Program Memory free!"
 endif

;------------------------------------------------------------------------------
; Configuration words
;------------------------------------------------------------------------------


; Configuration Word 1 (at 8007)
;   13:FCMEN, 12:IESO,
;   11:~CLKOUTEN, [10:9]:BOREN[1:0], 8:~CPD,
;   7:~CP, 6:MCLRE, 5:~PWRTE, [4:3]:WDTE[1:0], [2:0]:FOSC[2:0]

cw1 = 0x3FFF
cw1 = cw1 & _FCMEN_OFF
cw1 = cw1 & _IESO_ON
cw1 = cw1 & _CLKOUTEN_OFF
cw1 = cw1 & _BOREN_ON
cw1 = cw1 & _CPD_OFF
cw1 = cw1 & _CP_OFF
cw1 = cw1 & _MCLRE_ON
cw1 = cw1 & _PWRTE_ON
cw1 = cw1 & _WDTE_OFF
cw1 = cw1 & _FOSC_INTOSC

        __CONFIG _CONFIG1, cw1  ; 0x1FC4

; Configuration Word 2 (at 8008)
;   13:LVP, 12:~DEBUG,
;   11:Unused, 10:BORV, 9:STVREN, 8:PLLEN,
;   [7:5]:Unused, 4:Reserved, [3:2]:Unused, [1:0]:WRT[1:0]

cw2 = 0x3FFF
cw2 = cw2 & _LVP_OFF
cw2 = cw2 & _DEBUG_OFF
cw2 = cw2 & _BORV_LO
cw2 = cw2 & _STVREN_OFF
cw2 = cw2 & _PLLEN_ON
cw2 = cw2 & _WRT_OFF

        __CONFIG _CONFIG2, cw2  ; 0x1DFF


;------------------------------------------------------------------------------

        END
