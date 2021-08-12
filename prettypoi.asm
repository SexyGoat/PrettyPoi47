;------------------------------------------------------------------------------
; PrettyPoi47 code for the New Series "Ninja Poi" incorporating the PIC16F1847
; (For use with gpasm, an assembler with a C preprocessor)
;------------------------------------------------------------------------------


; PrettyPoi47 firmware for Ninja LED stick poi
; Version: 0.3.1.0
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
;   * The updating of the Last Used Pattern memory can be switched on or offf
;     via the boot menu.
;   * Via a boot menu option, the Favourites list can be locked and
;     the Mode Button may additionally be disabled.
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
;   * The last two banks of patterns are all uniformly coloured with whatever
;     pair of colour ramps have been selected by the user in the boot menu.
;     When one of those patterns is saved to the Favourites list, the index
;     of currently selected colour ramp pair is stored within the Favourite
;     recorded that is aved. Choosing a new colour ramp pair won't alter the
;     appearance of patterns already saved in Favourites.
;   * A short cue is displayed when a Pattern or Mode selection wraps around.
;     (but only as a result of a button press).
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
;     UpdateEffectiveRampsIx
;     ApplyExternalRamps
;     JumpToAnimFrame
;     FetchAnimFrame
;     AdvanceAnimCounters
;   Bank and Pattern-specific:
;     GetPatternAddress
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
;     InvalidateFavIx
;     PromoteOrDelFavourite_Common
;     PromoteFavourite
;     DeleteFavourite
;   Simple settings in EPROM:
;     LoadRestrictionsFromEEPROM
;     SaveRestrictionsToEEPROM
;     LoadIntensityIxFromEEPROM
;     SaveIntensityIxToEEPROM
;     LoadRampsIxFromEEPROM
;     SaveRampsIxToEEPROM
;   Input and UI functions:
;     WaitForReleaseOfAllButtons
;     RegisterButtonPress
;     AnimateCue
;     AnimateLongPress
;     AnimateBootMenuOption
;   Main functions:
;     Main
;
; Page 1 (0x0800..0x0FFF) and beyond
;
;   Memory clearing (moved to Page 0):
;     ClearLinearMemory
;     ClearCommonMemory
;     ConditionalyInitEEPROM
;   Additional UI stuff (moved to Page 0):
;     AnimateRampsIxMenu
;
;   System cues and menus (partially moved to Page 0)
;   Basic palette, enumerated
;   Ramp pairs (palette mappings to be applied to suitable patterns)
;   Ramp pairs address table, enumerated
;   Patterns
;   Pattern address table, enumerated
;   Pattern banks


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
MF_BIT_CUE                equ 7  ; A system feedback cue is playing.
MF_BIT_SLIDESHOW          equ 6  ; Slideshow mode is active.
MF_BIT_NOFAVSPILL         equ 3  ; Adding a new Favourite requires a free slot.
MF_BIT_LUPLOCKED          equ 2  ; Last Used Pattern record no longer updated.
MF_BIT_FAVPROTECT         equ 1  ; Favourites list cannot be altered.
MF_BIT_RESTRICTED         equ 0  ; Bank cannot be changed.

x = 0
x = x | (1 << MF_BIT_NOFAVSPILL)
x = x | (1 << MF_BIT_LUPLOCKED)
x = x | (1 << MF_BIT_FAVPROTECT)
x = x | (1 << MF_BIT_RESTRICTED)
MF_MASK_RESTRICTIONS    equ x

DEFAULT_LOCK_BITS equ (1 << MF_BIT_NOFAVSPILL)

; Byte-Addressed Program Memory pointer status byte
BAPM_BIT_CACHED         equ 7  ; Part of an odd byte was fetched earlier
BAPM_BIT_LOW_BYTES_ONLY equ 3  ; Signals plaing, non-BAPM mode
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

RAMPS_MASK  equ 0x3F  ; Mask for ramps index for palette modification
RAMPS_USER  equ 0x3F  ; Indicates pattern ramp index is user-settable

; Linear RAM metrics
LINEAR_RAM_SIZE     equ FULL_RAM_SIZE - 16
LINEAR_RAM_START    equ 0x2000
LINEAR_RAM_END      equ LINEAR_RAM_START + LINEAR_RAM_SIZE

; Where the expanded pattern goes, ready to be read into the pattern player
DEFAULT_PATTERN_ADDR  equ LINEAR_RAM_START + 64
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
SLIDESHOW_INTV_1_PWM_CYCLES equ SLIDESHOW_INTERVAL_1 * PWM_CYCLE_FREQUENCY/1000
SLIDESHOW_INTV_2_PWM_CYCLES equ SLIDESHOW_INTERVAL_2 * PWM_CYCLE_FREQUENCY/1000
SLIDESHOW_INTV_3_PWM_CYCLES equ SLIDESHOW_INTERVAL_3 * PWM_CYCLE_FREQUENCY/1000


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
; bit ones (0xFF, 0xFF) is used in a ring that is less than full to mark
; the end of the Favourites list.
        CBLOCK 0
FavRec_BankIx: 1
FavRec_PatternIx: 1
FavRecSize:       ; Program code currently expects the the size to be 2.
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


;------------------------------------------------------------------------------
; EEPROM addresses
;------------------------------------------------------------------------------


; The standard initial EEPROM state expected by the application is all
; bit ones (0xFF), though Intel hex files may show an unprogrammed byte
; as "FF00" to be consistent with the 14-bit program memory format.
; To present spurious advancement of the EEPROM Wear-Levelling rings,
; the EEPROM should be reset to the 0xFF in each byte,

FAVOURITES_NUM_SLOTS  equ 12
LUPEWL_NUM_SLOTS      equ 71  ; Last Used Pattern EEPROM wear levelling

        CBLOCK  0x00
EEPROM_PROBABLY_WORN_OUT_BY_NOW: 2  ; Probably worn out by the NS-2 V1.2
EEPROM_LOW_CONFIG_AREA: 0
EEPROM_LockBits: 1
EEPROM_Brightness: 1
EEPROM_RampsIndex: 1
EEPROM_UNUSED: 1
EEPROM_FavStatusRing: FAVOURITES_NUM_SLOTS
EEPROM_FavDataRing: 2 * FAVOURITES_NUM_SLOTS
EEPROM_LUPStatusRing: LUPEWL_NUM_SLOTS
EEPROM_LUPDataRing: 2 * LUPEWL_NUM_SLOTS
EEPROM_END:
        ENDC

  if EEPROM_END > 256
    error "EEPROM memory limit exceeded."
  endif

EEPROM_LOW_CONFIG_AREA_SIZE equ 3


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
SlideshowIntervalIx: 1  ; Index into table of slideshow intervals in PWM cycles

; The user's favourite colours
UserRampIxA: 1
; UserRampIxB: 1
; UserRampIxMidC: 1

; Fetched from a pattern in Favourites
FavPatRampIxA: 1
; FavPatRampIxB: 1
; FavPatRampIxMidC: 1

; Bank-sourced palette modification
PatternRampIxA: 1
; PatternRampIxB: 1
; PatternRampIxMidC: 1

; To be saved when a Favourite Pattern is saved
EffectiveRampIxA: 1
; EffectiveRampIxB: 1
; EffectiveRampIxMidC: 1
;
;
;
PatternRampsMode: 1  ; 0 = fixed, 1 = external ramps, 2 = user ramps
        ; 31
        ENDC

        CBLOCK  0x0040
; EPROM Wear-Levelling and general EEPROM access
EWLFind: EWLFindSize  ; 4
EWLAccess: EWLAccessSize  ; 5
EEPROMPtr: 1
        ; 10
        ENDC

        CBLOCK  0x0040
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

        CBLOCK  0x0040
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
; Don't trust even the stack pointer!
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
  if TURNON_HOLDTIME > 65535
    error "TURNON_HOLDTIME is too large."
  else
    if TURNON_HOLDTIME < 1
      error "TURNON_HOLDTIME is too small."
    endif
  endif
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
        ;

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
; Read the upper 6-bits of a 14-bit Program Memory word.
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
        movwf   BAPMRec + BAPM_Status
        ; Get the next palette colour index from the map.
        ; Clearing the Cached flag is important since the previous
        ; read from the palette would have dirtied the cache.
        bcf     BAPMRec + BAPM_Status, BAPM_BIT_CACHED
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
        retlw   0

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
;   pppp3333: Period 0..15 => 1..16 frame unit periods, LED3 colour index
;   22221111: LED2 colour index and LED1 colour index

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
        retlw   4
        retlw   2
        retlw   1
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


UpdateEffectiveRampsIx:
; The effective ramps index modifies the palette of selected patterns.
;
; In: FavouriteIx
;     PatternRampIxA
;     FavPatRampIxA
;     UserRampIxA
; Out: EffectiveRampIxA

        movf    PatternRampIxA, W
        andlw   RAMPS_MASK
        movwf   EffectiveRampIxA
        xorlw   RAMPS_USER
        btfss   STATUS, Z
        retlw   0
        movf    UserRampIxA, W
        movwf   EffectiveRampIxA
        incf    FavouriteIx, W
        btfsc   STATUS, Z
        retlw   0
        movf    FavPatRampIxA, W
        movwf   EffectiveRampIxA
        retlw   0


;------------------------------------------------------------------------------


ApplyExternalRamps:
; Overwrite a pattern's expanded palette with a 7-colour ramp set.
;
; The expanded pattern's display format must be PATDF_ER256C, PATDF_ER16C
; or PATDF_ER4C and the pattern must have a palette that is loaded to SRAM.
;
; In: GAState configured by LoadPattern, say
;     EffectiveRampIxA

        movf    PatternRampsMode, W
        btfsc   STATUS, Z
        retlw   0
        ;~ movf    GAState + GAS_AnimControlFlags, W <<<
        ;~ andlw   ACF_FORMAT_MASK
        ;~ sublw   PATDF_ER256C - 1
        ;~ btfsc   STATUS, C
        ;~ retlw   0
        ;~ ; The display format is one of the External Ramp type.
        movf    EffectiveRampIxA, W
        sublw   NUM_RAMP_PAIRS - 1
        btfss   STATUS, C
        retlw   0
        movlw   LOW(PM_RampAddrTable)
        movwf   FSR0L
        movlw   HIGH(PM_RampAddrTable)
        movwf   FSR0H
        movf    EffectiveRampIxA, W
        call    FetchPMWordFromTable
        bsf     FSR0H, 7
        clrf    MapWS + MapWS_MapBAPMStatus
        movf    FSR0L, W
        movwf   MapWS + MapWS_MapPtr + 0
        movf    FSR0H, W
        movwf   MapWS + MapWS_MapPtr + 1
        clrf    MapWS + MapWS_SourceBAPMStatus
        movlw   LOW(BAPM_Pal_Basic)
        movwf   MapWS + MapWS_SourcePtr + 0
        movlw   HIGH(BAPM_Pal_Basic)
        movwf   MapWS + MapWS_SourcePtr + 1
        movf    GAState + GAS_PalettePtr + 0, W
        movwf   MapWS + MapWS_DestPtr + 0
        movf    GAState + GAS_PalettePtr + 1, W
        movwf   MapWS + MapWS_DestPtr + 1
        movf    GAState + GAS_AnimControlFlags, W
        andlw   ACF_FORMAT_MASK
        xorlw   PATDF_4C
        movlw   7
        btfsc   STATUS, Z
        movlw   4
        movwf   MapWS + MapWS_NumEntries
        goto    LoadMappedPalette


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
        goto    FetchAnimFrame_Paletted_256c ; (External ramps)
        goto    FetchAnimFrame_Paletted_16c  ; (External ramps)
        goto    FetchAnimFrame_Paletted_4c   ; (External ramps)
        ;goto    FetchAnimFrame_Invalid

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


GetPatternAddress:
        pagesel GetPatternAddress_Page1
        call    GetPatternAddress_Page1
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
        incf    BankIx, W
        movwf   Scratch0
        incf    PatternIx, W
        iorwf   Scratch0, W
        btfss   STATUS, Z
        bra     _GetLUP_SkipUnpgmd
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
        goto    GetFavourite


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
        call    ReadEEPROMByte
        andwf   Scratch0, F
        comf    Scratch0, F
        btfsc   STATUS, Z
        bra     _GetFavMets_HaveNumFavs
        incf    NumFavs, F
        movlw   2 + FavRecSize
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
; First byte:  rrrrbbbb  ; Bank index and ramps index bits 3..0
; Second byte: rrpppppp  ; Pattern index and ramps index bits 5..4
;
; Out: FavouriteIx, BankIx, PatternIx, FavPatRampIxA

        call    SetEWLAccessForFav
        call    GetFavouriteDataSlotIx
        call    GetEWLDataAddress
        movf    EWLFind + EWLFind_DataSlotAddr, W
        movwf   EEPROMPtr
        call    ReadEEPROMByte
        movwf   BankIx
        call    ReadEEPROMByte
        movwf   PatternIx
        swapf   BankIx, W
        andlw   0x0F
        movwf   FavPatRampIxA
        btfsc   PatternIx, 6
        bsf     FavPatRampIxA, 4
        btfsc   PatternIx, 7
        bsf     FavPatRampIxA, 5
        movlw   0x0F
        andwf   BankIx, F
        movlw   0x3F
        andwf   PatternIx, F
        retlw   0


;------------------------------------------------------------------------------


SaveFavourite:
; Save the current Bank and Pattern to the Favourites list in EEPROM.
;
; The currently effective ramps index used to modify a pattern's palette
; is saved within the individual Favourite Pattern record. When a Favourite
; Pattern is recalled, its ramps index will be given priority over the
; general user ramps index (the one which amounts to the user's favourite
; colour).
;
; First byte:  rrrrbbbb  ; Bank index and ramps index bits 3..0
; Second byte: rrpppppp  ; Pattern index and ramps index bits 5..4
;
; It is possible to have one pattern appear multiple times in the list.

        call    SetEWLAccessForFav
SaveFavourite_HaveEWLAccess:
        movf    FavHeadEWLSlotIx, W
        incf    WREG, F
        call    GetEWLDataAddress
        movf    EWLFind + EWLFind_DataSlotAddr, W
        movwf   EEPROMPtr
        swapf   EffectiveRampIxA, W
        andlw   0xF0
        iorwf   BankIx, W
        call    WriteEEPROMByte_Smart
        movf    PatternIx, W
        btfsc   EffectiveRampIxA, 4
        bsf     WREG, 6
        btfsc   EffectiveRampIxA, 5
        bsf     WREG, 7
        call    WriteEEPROMByte_Smart
        call    AdvanceEWLStatusRing
        movwf   FavHeadEWLSlotIx
        incf    NumFavs, W
        sublw   FAVOURITES_NUM_SLOTS
        movlw   0
        addwfc  NumFavs, F
        retlw   0


;------------------------------------------------------------------------------


InvalidateFavIx:
; A Favourite Pattern index of 255 means that no Favourite Pattern is playing.

        movlw   255
        movwf   FavouriteIx
        retlw   0


;------------------------------------------------------------------------------


PromoteOrDelFavourite_Common:
; Delete a Favourite pattern entry by moving all older entries up one slot.
;
; This is rather complicated due to the wrap-around nature of the EWL rings.
; In any case, the slot corresponding to the oldest entry is filled with
; (0xFF, 0xFF).
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
        ; Copy Bank index byte
        movf    EWLFind + EWLFind_NextIx, W
        movwf   EEPROMPtr
        call    ReadEEPROMByte
        movwf   Scratch0
        movf    EWLFind + EWLFind_DataSlotAddr, W
        movwf   EEPROMPtr
        movf    Scratch0, W
        call    WriteEEPROMByte_Smart
        ; Copy Pattern index byte
        incf    EWLFind + EWLFind_NextIx, W
        movwf   EEPROMPtr
        call    ReadEEPROMByte
        movwf   Scratch0
        incf    EWLFind + EWLFind_DataSlotAddr, W
        movwf   EEPROMPtr
        movf    Scratch0, W
        call    WriteEEPROMByte_Smart
        ; Switch to the next target address
        movf    EWLFind + EWLFind_NextIx, W
        movwf   EWLFind + EWLFind_DataSlotAddr
        decfsz  LoopCtr0, F
        bra     _DelFav_Loop
_DelFav_TailShifted:
        movf    EWLFind + EWLFind_DataSlotAddr, W
        movwf   EEPROMPtr
        movlw   255
        call    WriteEEPROMByte
        movlw   255
        call    WriteEEPROMByte
        decf    NumFavs, F
        ; Do not validate FavouriteIx! This deletion may be a part of
        ; a procedure to promote the removed item to the head of the list.
        retlw   0


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

        movf    NumFavs, W
        btfsc   STATUS, Z
        goto    InvalidateFavIx
        incf    FavouriteIx, W
        lsrf    WREG, F
        btfsc   STATUS, Z
        retlw   0  ; FavouriteIx is either 255 or 0
        call    PromoteOrDelFavourite_Common
        goto    SaveFavourite_HaveEWLAccess
        retlw   0


;------------------------------------------------------------------------------


DeleteFavourite:
; Remove a pattern from the Favourite Pattern list.
;
; Older patterns will each be moved up to fill the hole. If the list becomes
; empty, the Favourite Index will be invalidated, forcing the first standard
; pattern to begin playing.

        movf    NumFavs, W
        btfsc   STATUS, Z
        goto    InvalidateFavIx
        incf    FavouriteIx, W
        btfsc   STATUS, Z
        retlw   0
        call    PromoteOrDelFavourite_Common
        movf    NumFavs, W
        btfsc   STATUS, Z
        goto    InvalidateFavIx
        subwf   FavouriteIx, W
        btfss   STATUS, Z
        bra     _DelFav_Done
        decf    NumFavs, W
        movwf   FavouriteIx
_DelFav_Done:
        retlw   0


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


LoadRampsIxFromEEPROM:
; Fetch the user ramps index (amounting to a favourite colour) from EEPROM.
;
; Out: UserRampIxA

        movlw   EEPROM_RampsIndex
        movwf   EEPROMPtr
        call    ReadEEPROMByte
        andlw   RAMPS_MASK
        movwf   UserRampIxA
        return


;------------------------------------------------------------------------------


SaveRampsIxToEEPROM:
; Save the user ramps index to EEPROM, if it has changed.
;
; In: UserRampIxA

        movlw   EEPROM_RampsIndex
        movwf   EEPROMPtr
        movf    UserRampIxA, W
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
ctrv = 8 * DEBOUNCE_RELEASE_TIME
  if ctrv > 65535
    error "DEBOUNCE_RELEASE_TIME is too large."
  else
    if ctrv < 1
      error "DEBOUNCE_RELEASE_TIME is too small."
    endif
  endif
        movlw   LOW(ctrv)
        movwf   LoopCtr0
        movlw   HIGH(ctrv)
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
ctrv = 8 * DEBOUNCE_PRESS_TIME
  if ctrv > 65535
    error "DEBOUNCE_PRESS_TIME is too large."
  else
    if ctrv < 1
      error "DEBOUNCE_PRESS_TIME is too small."
    endif
  endif
        movlw   LOW(ctrv)
        movwf   LoopCtr0
        movlw   HIGH(ctrv)
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
dbrt = DEBOUNCE_RELEASE_TIME * PWM_CYCLE_FREQUENCY / 1000
  if dbrt > 255
    error "DEBOUNCE_RELEASE_TIME is too large."
  else
    if dbrt < 1
      error "DEBOUNCE_RELEASE_TIME is too small."
    endif
  endif
        movlw   DEBOUNCE_RELEASE_TIME * PWM_CYCLE_FREQUENCY / 1000
        subwf   PWMCycleCounter, W
        btfss   STATUS, C
        bra     _AnimLP_Loop
        movf    GAState + GAS_FrameIx, W
        return


;------------------------------------------------------------------------------


AnimateBootMenuOption:
; Animate a simple tap-to-cycle menu until the Mode button is pressed.
;
; Each animation frame corresponds to one menu item. The unit frame
; period must be 1 and each frame must has a period of 1.
;
; In: FSR0 = Simple menu pattern header in Program Memory
;     W = Staring frame index
; Out: W = Frame index where button was released.

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
; Main functions
;------------------------------------------------------------------------------


Main:

        call    MashTheSnoozeButton
        pagesel ClearLinearMemory
        call    ClearLinearMemory
        pagesel ClearCommonMemory
        call    ClearCommonMemory
        pagesel ConditionalyInitEEPROM
        call    ConditionalyInitEEPROM
        movlp   0x00
        call    ConfigureHardware

        call    GetFavMetrics
        movlw   255
        movwf   FavouriteIx
        call    LoadIntensityIxFromEEPROM
        call    LoadRestrictionsFromEEPROM
        call    LoadRampsIxFromEEPROM

        movlw   SYSPAT_LPMENU_POWER_ON
        call    AnimateLongPress
        lsrf    WREG, W
        sublw   5 - 1  ; Warning: Performs W <- argument - W.
        btfss   STATUS, C
        bra     _Main_SkipBootMenu
        sublw   5 - 1 ; W' = x-(x-W) = W
        brw
        bra     _Main_SkipBootMenu
        bra     _Main_ChooseRampsSet
        bra     _Main_ChooseIntensity
        bra     _Main_ChooseLUPMode
        bra     _Main_ChooseRestrictions

_Main_ChooseRampsSet:
        movf    UserRampIxA, W
        pagesel AnimateRampsIxMenu
        call    AnimateRampsIxMenu
        movlp   0x00
        movwf   UserRampIxA
        call    SaveRampsIxToEEPROM
        bra     _Main_SkipBootMenu

_Main_ChooseIntensity:
        movlw   SYSPAT_MENU_INTENSITY
        call    GetEnumeratedSysPatAddr
        clrw
        movwf   IntensityIx
        call    AnimateBootMenuOption
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
        call    AnimateBootMenuOption
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
        call    AnimateBootMenuOption
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
        call    LoadRampsIxFromEEPROM
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
        call    _Main_LoadSlideshowCtr_Tramp0
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
        movlw   SYSPAT_LPMENU_STOP_SLIDESHOW
        call    AnimateLongPress
        lsrf    WREG, W
        btfsc   STATUS, Z
        bra     _Main_PrevPat
        bcf     ModeFlags, MF_BIT_SLIDESHOW
        bra     _Main_ManuallySetBaP
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
        call    GetPatternAddress  ; Validate PatternIx
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
        call    _Main_LoadSlideshowCtr_Tramp0
        bra     _Main_ProgSetBaP
_Main_ModeBtnPressed:
        call    _Main_LoadSlideshowCtr_Tramp0
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
        call    WaitForReleaseOfAllButtons
        incfsz  FavouriteIx, W
        bra     _Main_AdvBank_Fav
_Main_AdvBank_Std:
        clrf    PatternIx
        btfsc   ModeFlags, MF_BIT_RESTRICTED
        bra     _Main_ProgSetBaP
        incf    BankIx, F
        call    GetPatternAddress  ; Validate BankIx
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
_Main_SkipCueInit:
        call    LoadPattern
        call    UpdateEffectiveRampsIx
        call    ApplyExternalRamps  ; (Harmless in case of cue)
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
        call    GetPatternAddress
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
        call    GetPatternAddress
_Main_SSAdv_Common:
        call    LoadPattern
        call    UpdateEffectiveRampsIx
        call    ApplyExternalRamps
        call    _Main_LoadSlideshowCtr_Tramp0
        bra     _Main_Loop

_Main_AnimateCue:
        pagesel AnimateCue
        call    AnimateCue
        movlp   0
        btfsc   ModeFlags, MF_BIT_CUE
        bra     _Main_Loop
        call    _Main_SetBankPatRampStuff
        ;movf    PatternAddr + 0, W
        ;movwf   FSR0L
        ;movf    PatternAddr + 1, W
        ;movwf   FSR0H
        call    LoadPattern
        call    UpdateEffectiveRampsIx
        call    ApplyExternalRamps
        bra     _Main_Loop

_Main_SetBankPatRampStuff:
        incfsz  FavouriteIx, W
        call    GetFavourite
        call    GetPatternAddress
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

_Main_LoadSlideshowCtr_Tramp0:
        pagesel _Main_LoadSlideshowCountdown
        call    _Main_LoadSlideshowCountdown
        movlp   0
        return


;------------------------------------------------------------------------------

Page0CodeEnd:
Page0WordsUsed = Page0CodeEnd - 0x0000
Page0WordsFree = 0x0800 - Page0CodeEnd

;==============================================================================
; Page 1 (0x0800..0x0FFF)
;==============================================================================

        ORG     0x0800

;------------------------------------------------------------------------------
; Memory clearing
;------------------------------------------------------------------------------


_Main_LoadSlideshowCountdown:
; In: W = Slideshow interval index
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

  if SLIDESHOW_INTV_1_PWM_CYCLES > 65535
    error "SLIDESHOW_INTV_1_PWM_CYCLES is too large."
  else
    if SLIDESHOW_INTV_1_PWM_CYCLES < 1
      error "SLIDESHOW_INTV_1_PWM_CYCLES is too small."
    endif
  endif
  if SLIDESHOW_INTV_2_PWM_CYCLES > 65535
    error "SLIDESHOW_INTV_2_PWM_CYCLES is too large."
  else
    if SLIDESHOW_INTV_2_PWM_CYCLES < 1
      error "SLIDESHOW_INTV_2_PWM_CYCLES is too small."
    endif
  endif
  if SLIDESHOW_INTV_3_PWM_CYCLES > 65535
    error "SLIDESHOW_INTV_3_PWM_CYCLES is too large."
  else
    if SLIDESHOW_INTV_3_PWM_CYCLES < 1
      error "SLIDESHOW_INTV_3_PWM_CYCLES is too small."
    endif
  endif

PM_SlideshowIntvTable:
        dw  0
        dw  SLIDESHOW_INTV_1_PWM_CYCLES
        dw  SLIDESHOW_INTV_2_PWM_CYCLES
        dw  SLIDESHOW_INTV_3_PWM_CYCLES


;------------------------------------------------------------------------------


GetPatternAddress_Page1:
; Validate BankIx & BankPatIx and return the pattern header address in PM.
;
; In: BankIx, PatternIx
; Out: FSR0 = Pattern header address in Program Memory
;      PatternRampIxA = Ramp pair index byte for palette modification
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

        ; Bank table pattern records are two Program Words each.
        lslf    PatternIx, W
        addwf   FSR0L, F
        movlw   0
        addwfc  FSR0H, F

        ; Bank pattern entry format:
        ;
        ;                    bit          p = Enumerated pattern index (9 bits)
        ;               DCBA98 76543210   A = Primary ramp index (5 bits)
        ;               ---------------   B = Secondary ramp index (5 bits)
        ; First Word:   AAAAAp pppppppp   M = Middle ramp index (4 bits)
        ; Second Word:  BBBBBe MMMMTTTT   T = Frame unit period modifier
        ;                                 e = External ramps mode

        call    ReadPMHighBits
        movwf   PatternRampIxA
        moviw   FSR0++
        movwf   PatternRampsMode  ; Temporarily store enum pattern index here.
        call    ReadPMHighBits
        movwf   Div7Rec + Div7_Quotient + 0 ; <<< (B ramp ix)
        movf    INDF0, W
        movwf   Div7Rec + Div7_Remainder + 0 ; <<< (F.U.P. modifier)
        movwf   Div7Rec + Div7_Quotient + 1 ; <<< (BG colour index)
        swapf   Div7Rec + Div7_Quotient + 1, F ; <<< (BG colour index)
        movlw   0x0F
        andwf   Div7Rec + Div7_Remainder + 0, F ; <<< (F.U.P. modifier)
        andwf   Div7Rec + Div7_Quotient + 1, F ; <<< (BG colour index)
        movlw   LOW(PM_PatAddrTable)
        movwf   FSR0L
        movlw   HIGH(PM_PatAddrTable)
        movwf   FSR0H
        lsrf    PatternRampIxA, F
        movlw   0
        addwfc  FSR0H, F
        movf    PatternRampsMode, W
        call    FetchPMWordFromTable
        bsf     FSR0H, 7
        clrf    PatternRampsMode
        lsrf    Div7Rec + Div7_Quotient + 0, F ; <<< (B ramp ix)
        rlf     PatternRampsMode, F
      btfsc  Div7Rec + Div7_Quotient + 0, 0 ; <<< (B ramp ix)
      bsf    PatternRampIxA, 5 ; <<< (for current 6-bit ramp pair index)
        movlw   FIRST_USER_RAMPS_BANK_IX
        subwf   BankIx, W
        btfss   STATUS, C
        retlw   0
        lslf    PatternRampsMode, W
        iorwf   PatternRampsMode, F
        retlw   0

        ;~ ;A pattern table for a bank used to contain pointers to the pattern
        ;~ ; headers. Now, a pattern table contains words each with a pattern
        ;~ ; address index in the lower 8 bits and a palette override ramp pair
        ;~ ; index in the upper 6 bits.
        ;~ movf    PatternIx, W
        ;~ call    FetchPMWordFromTable
        ;~ movf    FSR0H, W
        ;~ movwf   PatternRampIxA
        ;~ movf    FSR0L, W
        ;~ movwf   Scratch0
        ;~ movlw   LOW(PM_PatAddrTable)
        ;~ movwf   FSR0L
        ;~ movlw   HIGH(PM_PatAddrTable)
        ;~ movwf   FSR0H
        ;~ movf    Scratch0, W
        ;~ call    FetchPMWordFromTable
        ;~ bsf     FSR0H, 7
        ;~ retlw   0

        ;~ ; Bank pattern records are three bytes each.
        ;~ clrf    BAPMRec + BAPM_Status
        ;~ clrf    Div7Rec + Div7_Dividend + 1
        ;~ lslf    PatternIx, W
        ;~ movwf   Div7Rec + Div7_Dividend + 0
        ;~ rlf     Div7Rec + Div7_Dividend + 1, F
        ;~ movf    PatternIx, W
        ;~ addwf   Div7Rec + Div7_Dividend + 0, F
        ;~ movlw   0
        ;~ addwfc  Div7Rec + Div7_Dividend + 1, F
        ;~ call    AddBAPMOffset

        ;~ ; Bank pattern entry format:
        ;~ ;
        ;~ ;             bit
        ;~ ;          76543210
        ;~ ;   Byte0: pppppppp  p = Enumerated pattern index (9 bits)
        ;~ ;   Byte1: pueAAAAA  e = External Ramps flag (1 bit)
        ;~ ;   Byte2: 0CCBBBBB  u = User Ramps flag (1 bit)
        ;~ ;                    A = Primary ramp index (5 bits)
        ;~ ;                    B = Secondary ramp index (5 bits)
        ;~ ;                    C = Background colour index (2 bits)

        ;~ call    ReadBAPMByte
        ;~ movwf   Div7Rec + Div7_Dividend + 0  ; <<<<< epi
        ;~ call    ReadBAPMByte
        ;~ movwf   PatternRampsMode
        ;~ andlw   0x9F
        ;~ movwf   PatternRampIxA ; <<<  (A ramp ix)
        ;~ movlw   3 << 5
        ;~ andwf   PatternRampsMode, F
        ;~ swapf   PatternRampsMode, F
        ;~ lsrf    PatternRampsMode, F
        ;~ call    ReadBAPMByte
      ;~ btfsc  WREG, 7 ;<<< (for current 6-bit ramp pairs index)
      ;~ bsf    PatternRampIxA, 5 ; <<< "  "
        ;~ movwf   Div7Rec + Div7_Quotient + 0 ; <<< (B ramp ix)
        ;~ andlw   3 << 5
        ;~ movwf   Div7Rec + Div7_Quotient + 1 ; <<< (BG colour index)
        ;~ swapf   Div7Rec + Div7_Quotient + 1, F ; <<< (BG colour index)
        ;~ lsrf    Div7Rec + Div7_Quotient + 1, F ; <<< (BG colour index)
        ;~ movlw   31
        ;~ andwf   Div7Rec + Div7_Quotient + 0, F ; <<< (B ramp ix)

        ;~ ; Fetch the pattern header's address using a 9-bit index.
        ;~ ; (Bit 8 is rudely stashed in an unused bit of the A Ramp Index.)
        ;~ movlw   LOW(PM_PatAddrTable)
        ;~ movwf   FSR0L
        ;~ movlw   HIGH(PM_PatAddrTable)
        ;~ movwf   FSR0H
        ;~ btfsc   PatternRampIxA, 7
        ;~ incf    FSR0H, F
        ;~ bcf     PatternRampIxA, 7 ; <<<  (A ramp ix)
        ;~ movf    Div7Rec + Div7_Dividend + 0, W  ; <<<< epi
        ;~ call    FetchPMWordFromTable
        ;~ bsf     FSR0H, 7
        ;~ retlw   0


;------------------------------------------------------------------------------


ClearLinearMemory:
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


ClearCommonMemory:
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


ConditionalyInitEEPROM:
; If the lower poi configuration bytes in EEPROM are all 255s (unprogrammed),
; assume the whole EEPROM is uninitialised and prepare a sensible starting
; configuration.

        movlp   0x00
        movlw   EEPROM_LOW_CONFIG_AREA
        movwf   EEPROMPtr
        movlw   EEPROM_LOW_CONFIG_AREA_SIZE
        movwf   LoopCtr0
_CondInitE_ReadLoop:
        call    ReadEEPROMByte
        incf    WREG, F
        btfss   STATUS, Z
        retlw   0
        decfsz  LoopCtr0, F
        bra     _CondInitE_ReadLoop
        ; All 255s is an invalid state which implies a virgin EEPROM.
        ; First set the default brightness.
        movlw   EEPROM_Brightness
        movwf   EEPROMPtr
        movlw   DEFAULT_INTENSITY_IX
        call    WriteEEPROMByte
        ; Write a default set of restrictions.
        movlw   EEPROM_LockBits
        movwf   EEPROMPtr
        movlw   DEFAULT_LOCK_BITS
        call    WriteEEPROMByte
        ; Set a sensible default Last Used Pattern.
        ; That means also adjusting the LUP EWL status ring.
        ; The Favourites ring can be left as all 255s.
        movlw   LOW(_CondInitE_AddrsToClear)
        movwf   FSR0L
        movlw   HIGH(_CondInitE_AddrsToClear)
        movwf   FSR0H
        movlw   _CondInitE_AddrsToClear_End - _CondInitE_AddrsToClear
        movwf   LoopCtr0
_CondInitE_ClearLoop:
        moviw   FSR0++
        movwf   EEPROMPtr
        clrw
        call    WriteEEPROMByte
        decfsz  LoopCtr0, F
        bra     _CondInitE_ClearLoop
        retlw   0
_CondInitE_AddrsToClear:
        dw      EEPROM_RampsIndex
        dw      EEPROM_LUPStatusRing
        dw      EEPROM_LUPDataRing + 0
        dw      EEPROM_LUPDataRing + 1
_CondInitE_AddrsToClear_End:


;------------------------------------------------------------------------------
; Additional UI stuff
;------------------------------------------------------------------------------


AnimateCue:
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


AnimateRampsIxMenu:
; In: W = Default user ramp index
; Out: W = Selected User ramp index

        movwf   EffectiveRampIxA
        movlp   0x00
        movlw   SYSPAT_RAMPS_VIEWER
        call    GetEnumeratedSysPatAddr
        call    LoadPattern
_AnimRIxM_RampsLoop:
        call    ApplyExternalRamps
_AnimRIxM_Loop:
        call    FetchAnimFrame
        call    LoadLEDOCRs
        call    PerformPCPWMCycle
        call    AdvanceAnimCounters
        btfsc   PORT_REGBIT_FR(UPB_MODE_BUTTON)
        bra     _AnimRIxM_ButtonPress
        btfss   PORT_REGBIT_FR(UPB_PAT_BUTTON)
        bra     _AnimRIxM_Loop
_AnimRIxM_ButtonPress:
        call    RegisterButtonPress
        btfsc   WREG, INPUT_BIT_MODE
        bra     _AnimRIxM_ModeBtnPressed
        btfss   WREG, INPUT_BIT_PATTERN
        bra     _AnimRIxM_Loop
_AnimRIxM_PatBtnPressed:
        call    WaitForReleaseOfAllButtons
        incf    EffectiveRampIxA, F
        movlw   NUM_USER_RAMP_PAIRS
        subwf   EffectiveRampIxA, W
        btfsc   STATUS, C
        clrf    EffectiveRampIxA
        bra     _AnimRIxM_RampsLoop
_AnimRIxM_ModeBtnPressed:
        call    WaitForReleaseOfAllButtons
        movf    EffectiveRampIxA, W
        return


;------------------------------------------------------------------------------
; Palette for system cues and menus
;------------------------------------------------------------------------------


BAPM_Pal_System:
  resetenum
  bablock
  enumdat SYS_BLK,    dbvrgb, 0x000000  ; Black
  enumdat SYS_DGRY,   dbvrgb, 0x0B0808  ; Dark Grey
  enumdat SYS_RED,    dbvrgb, 0x2F0000  ; Red
  enumdat SYS_BLZ,    dbvrgb, 0x440E00  ; Orange
  enumdat SYS_YEL,    dbvrgb, 0x321400  ; Bogan Yellow
  enumdat SYS_GRN,    dbvrgb, 0x22CC00  ; Bright Green
  enumdat SYS_SGR,    dbvrgb, 0x009C2C  ; Sea Green
  enumdat SYS_BLU,    dbvrgb, 0x000035  ; Blue
  enumdat SYS_MAG,    dbvrgb, 0x770061  ; Magenta
  enumdat SYS_XWHT,   dbvrgb, 0xFFFFFF  ; Full White
  enumdat SYS_XRED,   dbvrgb, 0xFF0000  ; Full Red
  enumdat SYS_GRN1,   dbvrgb, 0x005000  ; Medium Green
  ;enumdat SYS_VIO,    dbvrgb, 0x1F0038  ; Violet
  enumdat SYS_SKY,    dbvrgb, 0x00102C  ; Sky Blue
  enumdat SYS_SGR1,   dbvrgb, 0x00340D  ; Medium Sea Green
  ;enumdat SYS_GRY,    dbvrgb, 0x657F80  ; Grey
  enumdat SYS_IIX4,   dbvrgb, 0x0C0C0C  ; Test of Intensity Index 4
  enumdat SYS_IIX3,   dbvrgb, 0x191919  ; Test of Intensity Index 3
  enumdat SYS_IIX2,   dbvrgb, 0x333333  ; Test of Intensity Index 2
  enumdat SYS_IIX1,   dbvrgb, 0x666666  ; Test of Intensity Index 1
  enumdat SYS_IIX0,   dbvrgb, 0xCCCCCC  ; Test of Intensity Index 5
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
        dbv2b    0, SYS_GRN
        endbab
PM_Pat_SpuriousError:
        pattern PATDF_4C, 2, 100, PATSF_BAPM_PMF, 2
        dw  BAPM_Pal_System, PM_Map_SpuriousError, PM_Frs_Err_Flash1LED

PM_Map_Err_InvalidFormat:
        bablock
        dbv2b    0, SYS_RED
        endbab
PM_Pat_Err_InvalidFormat:
        pattern PATDF_4C, 2, 100, PATSF_BAPM_PMF, 2
        dw  BAPM_Pal_System, PM_Map_Err_InvalidFormat, PM_Frs_Err_Flash1LED

PM_Map_Err_NullPalette:
        bablock
        dbv2b    0, SYS_YEL
        endbab
PM_Pat_Err_NullPalette:
        pattern PATDF_4C, 2, 100, PATSF_BAPM_PMF, 2
        dw  BAPM_Pal_System, PM_Map_Err_NullPalette, PM_Frs_Err_Flash1LED

PM_Map_Err_PatternTooLarge:
        bablock
        dbv2b    0, SYS_MAG
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
        pattern PATDF_256C, SYS_PALETTE_LENGTH, 10, PATSF_BAPM_PF, 21
        dw  BAPM_Pal_System, 0, 0
        bablock
        ; Frames with even period lengths are flickery.
        ; On
        pf256c   36, SYS_DGRY, SYS_DGRY, SYS_DGRY
        pf256c   15, SYS_DGRY, SYS_DGRY, SYS_DGRY
        ; Choose colour_ramps
        pf256c   36, SYS_MAG,  SYS_MAG,  SYS_MAG
        pf256c   13, SYS_MAG,  SYS_MAG,  SYS_MAG
        ; Choose intensity
        pf256c   36, SYS_BLU,  SYS_BLU,  SYS_BLU
        pf256c   13, SYS_BLU,  SYS_BLU,  SYS_BLU
        ; Choose Last Used Pattern mode
        pf256c   36, SYS_YEL,  SYS_YEL,  SYS_YEL
        pf256c   13, SYS_YEL,  SYS_YEL,  SYS_YEL
        ; Choose Restrictions
        pf256c  120, SYS_RED,  SYS_RED,  SYS_RED
        pf256c   25, SYS_RED,  SYS_RED,  SYS_RED
        ; Orientation and channel test (On)
        pf256c   12, SYS_RED,     0,        0
        pf256c   12,    0,     SYS_GRN,     0
        pf256c   12,    0,        0,     SYS_BLU
        ; Supply voltage stability stress test (On)
        pf256c   25, SYS_DGRY, SYS_DGRY, SYS_DGRY
        pf256c   25, SYS_XWHT, SYS_DGRY, SYS_DGRY
        pf256c   25, SYS_XWHT, SYS_XWHT, SYS_DGRY
        pf256c   25, SYS_XWHT, SYS_XWHT, SYS_XWHT
        ; Supply current stress test (On)
        pf256c   24, SYS_DGRY, SYS_DGRY, SYS_DGRY
        pf256c   24, SYS_XWHT, SYS_DGRY, SYS_DGRY
        pf256c   24, SYS_XWHT, SYS_XWHT, SYS_DGRY
        pf256c   24, SYS_XWHT, SYS_XWHT, SYS_XWHT
        endbab

PM_Pat_LPMenu_Mode:
        pattern PATDF_256C, SYS_PALETTE_LENGTH, 10, PATSF_BAPM_PF, 8
        dw  BAPM_Pal_System, 0, 0
        bablock
        ; Frames with even period lengths are flickery.
        ; Next Mode
        pf256c   16,    0,        0,        0
        pf256c    9, SYS_DGRY, SYS_DGRY, SYS_DGRY
        ; Jump to Favourites
        pf256c   16, SYS_BLU, SYS_SGR1, SYS_BLU
        pf256c    9, SYS_BLU, SYS_SGR1, SYS_BLU
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
        ; Frames with even period lengths are flickery.
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
        ; Frames with even period lengths are flickery.
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
        ; Frames with even period lengths are flickery.
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
        ; Frames with even period lengths are flickery.
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
        pattern PATDF_256C, SYS_PALETTE_LENGTH, 10, PATSF_BAPM_PF, 3
        dw  BAPM_Pal_System, 0, 0
        bablock
        ; Frames with even period lengths are flickery.
        ; Previous Pattern
        pf256c   16,    0,        0,        0
        pf256c   13, SYS_MAG,  SYS_MAG,  SYS_MAG
        ; Stop Slideshow
        pf256c    1, SYS_BLU,  SYS_BLU,  SYS_BLU
        endbab

; The submenus are manually advanced with button presses.

PM_Map_Menu_Intensity:
        bablock
        dbv6b     0, SYS_IIX4, SYS_IIX3, SYS_IIX2, SYS_IIX1, SYS_IIX0
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
        dbv4b    0, SYS_RED, SYS_YEL, SYS_SGR
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
        dbv5b    0, SYS_RED, SYS_YEL, SYS_SKY, SYS_GRN1
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
        dbv3b    0, SYS_DGRY, 8
        endbab
PM_Pat_Cue_Wrap:
        pattern PATDF_4C, 2, 12, PATSF_BAPM_PMF, 5
        dw  BAPM_Pal_System, PM_Map_Cue_Wrap, 0
        bablock
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
        dbv2b    0, SYS_SGR
        endbab
PM_Pat_Cue_EnterFavs:
        pattern PATDF_4C, 2, 15, PATSF_BAPM_PMF, 4
        dw  BAPM_Pal_System
        dw  PM_Map_Cue_EnterFavs
        dw  PM_Frs_Cue_EnterStdsOrFavs

PM_Map_Cue_EnterStds:
        bablock
        dbv2b    0, SYS_BLU
        endbab
PM_Pat_Cue_EnterStds:
        pattern PATDF_4C, 2, 15, PATSF_BAPM_PMF, 4
        dw  BAPM_Pal_System
        dw  PM_Map_Cue_EnterStds
        dw  PM_Frs_Cue_EnterStdsOrFavs

; One of the boot submenus is the colour ramp pair selector to be applied
; to the banks of patterns with ramp indices of 63 ("user ramps").

PM_Frs_RampsViewer:
        bablock
        pf16c     5,   1,  2,  3
        pf16c     1,   2,  3,  0
        pf16c     1,   3,  0,  0
        pf16c     1,   0,  0,  6
        pf16c     1,   0,  6,  5
        pf16c     5,   6,  5,  4
        pf16c     1,   0,  6,  5
        pf16c     1,   0,  0,  6
        pf16c     1,   3,  0,  0
        pf16c     1,   2,  3,  0
        endbab
PM_Pat_RampsViewer:
        pattern PATDF_16C, 7, 36, PATSF_BAPM_PMF, 10
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, PM_Frs_RampsViewer


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
  enumdat SYSPAT_RAMPS_VIEWER,            dw, PM_Pat_RampsViewer
  enumdat SYSPAT_MENU_INTENSITY,          dw, PM_Pat_Menu_Intensity
  enumdat SYSPAT_MENU_LOCK_LUP,           dw, PM_Pat_Menu_LockLUP
  enumdat SYSPAT_MENU_RESTRICTIONS,       dw, PM_Pat_Menu_Restrictions
  enumdat SYSPAT_CUE_WRAP,                dw, PM_Pat_Cue_Wrap
  enumdat SYSPAT_CUE_ENTER_FAVS,          dw, PM_Pat_Cue_EnterFavs
  enumdat SYSPAT_CUE_ENTER_STDS,          dw, PM_Pat_Cue_EnterStds


;------------------------------------------------------------------------------
; Basic palette, enumerated
;------------------------------------------------------------------------------


BAPM_Pal_Basic:
  resetenum
  bablock
  ; Greyscale
  enumdat BLK,     dbvrgb, 0x000000  ;  0: Black
  enumdat WHT2,    dbvrgb, 0x121514  ;  1: Grey
  enumdat WHT1,    dbvrgb, 0x3F4845  ;  2: Silver
  enumdat WHT,     dbvrgb, 0xB5CEC5  ;  3: Standard white
  enumdat XEN,     dbvrgb, 0xD0DEE4  ;  4: Xenon flash
  ; Main colours
  enumdat RED,     dbvrgb, 0xCC0000  ;  5: Red
  enumdat BLZ,     dbvrgb, 0xC81200  ;  6: Blaze Orange
  enumdat ORA,     dbvrgb, 0xE02D00  ;  7: Orange
  enumdat YEL,     dbvrgb, 0xEE6A00  ;  8: Yellow
  enumdat SNT,     dbvrgb, 0x559900  ;  9: Snot
  enumdat GRN,     dbvrgb, 0x00BB00  ; 10: Green
  enumdat SGR,     dbvrgb, 0x00AC2C  ; 11: Sea Green
  enumdat CYN,     dbvrgb, 0x008899  ; 12: Cyan
  enumdat SKY,     dbvrgb, 0x003799  ; 13: Sky Blue
  enumdat BLU,     dbvrgb, 0x0000BB  ; 14: Blue
  enumdat PUR,     dbvrgb, 0x4800CC  ; 15: Purple
  enumdat VIO,     dbvrgb, 0x7000C4  ; 16: Violet
  enumdat MAG,     dbvrgb, 0xAA0090  ; 17: Magenta
  enumdat CER,     dbvrgb, 0xD2002C  ; 18: Cerise
  enumdat PNK,     dbvrgb, 0xCC2235  ; 19: Pink
  ; Medium intensity colours
  enumdat RED1,    dbvrgb, 0x5C0000  ; 20: Medium Red
  enumdat BLZ1,    dbvrgb, 0x5A0800  ; 21: Medium Blaze Orange
  enumdat ORA1,    dbvrgb, 0x651400  ; 22: Medium Orange
  enumdat YEL1,    dbvrgb, 0x6B3000  ; 23: Medium Yellow
  enumdat SNT1,    dbvrgb, 0x264500  ; 24: Medium Snot
  enumdat GRN1,    dbvrgb, 0x005400  ; 25: Medium Green
  enumdat SGR1,    dbvrgb, 0x004D14  ; 26: Medium Sea Green
  enumdat CYN1,    dbvrgb, 0x003D45  ; 27: Medium Cyan
  enumdat SKY1,    dbvrgb, 0x001945  ; 28: Medium Sky Blue
  enumdat BLU1,    dbvrgb, 0x000054  ; 29: Medium Blue
  enumdat PUR1,    dbvrgb, 0x20005C  ; 30: Medium Purple
  enumdat VIO1,    dbvrgb, 0x320058  ; 31: Medium Violet
  enumdat MAG1,    dbvrgb, 0x4D0041  ; 32: Medium Magenta
  enumdat CER1,    dbvrgb, 0x5F0014  ; 33: Medium Cerise
  enumdat PNK1,    dbvrgb, 0x5C0F18  ; 34: Medium Pink
  ; Low intensity colours
  enumdat RED2,    dbvrgb, 0x100000  ; 35: Dark Red
  enumdat BLZ2,    dbvrgb, 0x100100  ; 36: Dark Blaze Orange
  enumdat ORA2,    dbvrgb, 0x120400  ; 37: Dark Orange
  enumdat YEL2,    dbvrgb, 0x130800  ; 38: Dark Yellow
  enumdat SNT2,    dbvrgb, 0x070C00  ; 39: Dark Snot
  enumdat GRN2,    dbvrgb, 0x000F00  ; 40: Dark Green
  enumdat SGR2,    dbvrgb, 0x000E04  ; 41: Dark Sea Green
  enumdat CYN2,    dbvrgb, 0x000B0C  ; 42: Dark Cyan
  enumdat SKY2,    dbvrgb, 0x00040C  ; 43: Dark Sky Blue
  enumdat BLU2,    dbvrgb, 0x00000F  ; 44: Dark Blue
  enumdat PUR2,    dbvrgb, 0x060010  ; 45: Dark Purple
  enumdat VIO2,    dbvrgb, 0x090010  ; 46: Dark Violet
  enumdat MAG2,    dbvrgb, 0x0E000C  ; 47: Dark Magenta
  enumdat CER2,    dbvrgb, 0x110004  ; 48: Dark Cerise
  enumdat PNK2,    dbvrgb, 0x100304  ; 49: Dark Pink
  ; Special colours
  ;enumdat GRN3,    dbvrgb, 0x100304  ; 50: : Darkest Green
  endbab
BASIC_PALETTE_LENGTH equ ENUMIX


;------------------------------------------------------------------------------
; Ramp pairs (palette mappings to be applied to suitable patterns)
;------------------------------------------------------------------------------


; Dual chromatic aberrations
BAPM_Ramps_Default:
BAPM_Ramps_Fire_Ice:
        bablock
        dbv7b    0, ORA, BLZ1, RED2, CYN, SKY1, BLU2
        endbab
BAPM_Ramps_Orange_Violet:
        bablock
        dbv7b    0, ORA, ORA1, ORA2, VIO, VIO1, VIO2
        endbab
BAPM_Ramps_White_Ice:
        bablock
        dbv7b    0, WHT, WHT1, WHT2, CYN, SKY1, BLU2
        endbab
BAPM_Ramps_Ice_Strawberry:
        bablock
        dbv7b    0, CYN, SKY1, BLU2, PNK, CER1, RED2
        endbab
BAPM_Ramps_Gold_Silver:
        bablock
        dbv7b    0, YEL, ORA1, BLZ2, WHT, WHT1, WHT2
        endbab
; Non-black backgrounds
BAPM_Ramps_PurpleHaze:
        bablock
        dbv7b    MAG2, ORA, BLZ, RED1, BLU, PUR1, VIO1
        endbab
BAPM_Ramps_SpectralViolence:
        bablock
        dbv7b    BLZ2, SKY, YEL, CER1, BLZ, GRN1, BLU1
        endbab
BAPM_Ramps_RedMist:
        bablock
        dbv7b    RED2, BLZ, RED, RED1, BLZ, RED, RED2
        endbab
; Dual plain colours
BAPM_Ramps_Blue_SkyBlue:
        bablock
        dbv7b    0, BLU, BLU1, BLU2, SKY, SKY1, SKY2
        endbab
BAPM_Ramps_Red_SkyBlue:
        bablock
        dbv7b    0, RED, RED1, RED2, SKY, SKY1, SKY2
        endbab
BAPM_Ramps_Cerise_Green:
        bablock
        dbv7b    0, CER, CER1, CER2, GRN, GRN1, GRN2
        endbab
BAPM_Ramps_BlazeOrange_SkyBlue:
        bablock
        dbv7b    0, BLZ, BLZ1, BLZ2, SKY, SKY1, SKY2
        endbab
BAPM_Ramps_Yellow_SkyBlue:
        bablock
        dbv7b    0, YEL, YEL1, YEL2, SKY1, SKY1, SKY2
        endbab
BAPM_Ramps_Red_Cerise:
        bablock
        dbv7b    0, RED, RED1, RED2, CER, CER1, CER2
        endbab
BAPM_Ramps_SkyBlue_Green:
        bablock
        dbv7b    0, SKY, SKY1, SKY2, GRN, GRN1, GRN2
        endbab
BAPM_Ramps_Red_White:
        bablock
        dbv7b    0, RED, RED1, RED2, WHT, WHT1, WHT2
        endbab
BAPM_Ramps_Orange_White:
        bablock
        dbv7b    0, ORA, ORA1, ORA2, WHT, WHT1, WHT2
        endbab
BAPM_Ramps_Green_White:
        bablock
        dbv7b    0, GRN, GRN1, GRN2, WHT, WHT1, WHT2
        endbab
BAPM_Ramps_SkyBlue_White:
        bablock
        dbv7b    0, SKY, SKY1, SKY2, WHT, WHT1, WHT2
        endbab
BAPM_Ramps_Violet_White:
        bablock
        dbv7b    0, VIO, VIO1, VIO2, WHT, WHT1, WHT2
        endbab
; Single chromatic aberrations
BAPM_Ramps_Blurple:
        bablock
        dbv7b    0, BLU, VIO1, MAG2, BLU, VIO1, MAG2
        endbab
BAPM_Ramps_Strawberry:
        bablock
        dbv7b    0, PNK, CER1, RED2, PNK, CER1, RED2
        endbab
BAPM_Ramps_Gold:
        bablock
        dbv7b    0, YEL, ORA1, BLZ2, YEL, ORA1, BLZ2
        endbab
BAPM_Ramps_Mint:
        bablock
        dbv7b    0, WHT, SGR, GRN2, WHT, SGR, GRN2
        endbab
BAPM_Ramps_Arc:
        bablock
        dbv7b    0, XEN, PUR, VIO2, XEN, PUR, VIO2
        endbab
BAPM_Ramps_Ice:
        bablock
        dbv7b    0, CYN, SKY1, BLU2, CYN, SKY1, BLU2
        endbab
BAPM_Ramps_Fire:
        bablock
        dbv7b    0, ORA, BLZ1, RED2, ORA, BLZ1, RED2
        endbab
BAPM_Ramps_Bunsen:
        bablock
        dbv7b    0, PUR, PUR1, BLU2, PUR, PUR1, BLU2
        endbab
; Single plain colours
BAPM_Ramps_Red:
        bablock
        dbv7b    0, RED, RED1, RED2, RED, RED1, RED2
        endbab
BAPM_Ramps_BlazeOrange:
        bablock
        dbv7b    0, BLZ, BLZ1, BLZ2, BLZ, BLZ1, BLZ2
        endbab
BAPM_Ramps_Orange:
        bablock
        dbv7b    0, ORA, ORA1, ORA2, ORA, ORA1, ORA2
        endbab
BAPM_Ramps_Yellow:
        bablock
        dbv7b    0, YEL, YEL1, YEL2, YEL, YEL1, YEL2
        endbab
BAPM_Ramps_Snot:
        bablock
        dbv7b    0, SNT, SNT1, SNT2, SNT, SNT1, SNT2
        endbab
BAPM_Ramps_Green:
        bablock
        dbv7b    0, GRN, GRN1, GRN2, GRN, GRN1, GRN2
        endbab
BAPM_Ramps_SeaGreen:
        bablock
        dbv7b    0, SGR, SGR1, SGR2, SGR, SGR1, SGR2
        endbab
BAPM_Ramps_Cyan:
        bablock
        dbv7b    0, CYN, CYN1, CYN2, CYN, CYN1, CYN2
        endbab
BAPM_Ramps_SkyBlue:
        bablock
        dbv7b    0, SKY, SKY1, SKY2, SKY, SKY1, SKY2
        endbab
BAPM_Ramps_Blue:
        bablock
        dbv7b    0, BLU, BLU1, BLU2, BLU, BLU1, BLU2
        endbab
BAPM_Ramps_Purple:
        bablock
        dbv7b    0, PUR, PUR1, PUR2, PUR, PUR1, PUR2
        endbab
BAPM_Ramps_Violet:
        bablock
        dbv7b    0, VIO, VIO1, VIO2, VIO, VIO1, VIO2
        endbab
BAPM_Ramps_Magenta:
        bablock
        dbv7b    0, MAG, MAG1, MAG2, MAG, MAG1, MAG2
        endbab
BAPM_Ramps_Cerise:
        bablock
        dbv7b    0, CER, CER1, CER2, CER, CER1, CER2
        endbab
BAPM_Ramps_Pink:
        bablock
        dbv7b    0, PNK, PNK1, PNK2, PNK, PNK1, PNK2
        endbab
BAPM_Ramps_White:
        bablock
        dbv7b    0, WHT, WHT1, WHT2, WHT, WHT1, WHT2
        endbab
; Non-selectable ramps (because they are not really ramps)
BAPM_Ramps_Tri_RGB:
        bablock
        dbv4b   BLK, RED, GRN, BLU
        endbab
BAPM_Ramps_Tri_RWB:
        bablock
        dbv4b   BLK, RED, WHT, BLU
        endbab
BAPM_Ramps_Tri_CMY:
        bablock
        dbv4b   BLK, CYN, MAG, YEL
        endbab
BAPM_Ramps_Tri_BWR:
        bablock
        dbv4b   BLK, BLU, WHT, RED
        endbab
BAPM_Ramps_Tri_RevRYG:
        bablock
        dbv4b   BLK, GRN1, YEL, RED
        endbab
BAPM_Ramps_Tri_RevGWO:
        bablock
        dbv4b   BLK, ORA1, WHT, GRN1
        endbab
BAPM_Ramps_Tri_RevCWS:
        bablock
        dbv4b   BLK, SKY, WHT, CER
        endbab
BAPM_Ramps_Tri_RWR:
        bablock
        dbv4b   BLK, RED, WHT, RED
        endbab
BAPM_Ramps_Tri_GWG:
        bablock
        dbv4b   BLK, GRN1, WHT, GRN1
        endbab
BAPM_Ramps_Tri_CWC:
        bablock
        dbv4b   BLK, CYN, WHT, CYN
        endbab
BAPM_Ramps_Tri_BWB:
        bablock
        dbv4b   BLK, BLU1, WHT, BLU1
        endbab
BAPM_Ramps_Tri_RevGWR:
        bablock
        dbv4b   BLK, RED, WHT, GRN1
        endbab
BAPM_Ramps_Tri_RevRPB:
        bablock
        dbv4b   BLK, BLU, PNK, RED
        endbab
BAPM_Ramps_Tri_RevCYB:
        bablock
        dbv4b   BLK, BLU, YEL, CER
        endbab
BAPM_Ramps_Tri_CSB:
        bablock
        dbv4b   BLK, CYN, SKY, BLU
        endbab


;------------------------------------------------------------------------------
; Ramp pairs address table, enumerated
;------------------------------------------------------------------------------


PM_RampAddrTable:
  resetenum
  ; Dual chromatic aberrations
  enumdat RAMPS_FIRE_ICE,         dw, BAPM_Ramps_Fire_Ice
  enumdat RAMPS_ORANGE_VIOLET,    dw, BAPM_Ramps_Orange_Violet
  enumdat RAMPS_WHITE_ICE,        dw, BAPM_Ramps_White_Ice
  enumdat RAMPS_ICE_STRAWBERRY,   dw, BAPM_Ramps_Ice_Strawberry
  ; Non-black backgrounds
  enumdat RAMPS_PURPLE_HAZE,      dw, BAPM_Ramps_PurpleHaze
  enumdat RAMPS_SPECTRAL_VIOLENCE,dw, BAPM_Ramps_SpectralViolence
  enumdat RAMPS_RED_MIST,         dw, BAPM_Ramps_RedMist
  ; Dual plain colours
  enumdat RAMPS_BLUE_SKYBLUE,     dw, BAPM_Ramps_Blue_SkyBlue
  enumdat RAMPS_RED_SKYBLUE,      dw, BAPM_Ramps_Red_SkyBlue
  enumdat RAMPS_CERISE_GREEN,     dw, BAPM_Ramps_Cerise_Green
  enumdat RAMPS_BLAZOR_SKYBLUE,   dw, BAPM_Ramps_BlazeOrange_SkyBlue
  enumdat RAMPS_YELLOW_SKYBLUE,   dw, BAPM_Ramps_Yellow_SkyBlue
  enumdat RAMPS_RED_CERISE,       dw, BAPM_Ramps_Red_Cerise
  enumdat RAMPS_SKYBLUE_GREEN,    dw, BAPM_Ramps_SkyBlue_Green
  enumdat RAMPS_RED_WHITE,        dw, BAPM_Ramps_Red_White
  enumdat RAMPS_ORANGE_WHITE,     dw, BAPM_Ramps_Orange_White
  enumdat RAMPS_GREEN_WHITE,      dw, BAPM_Ramps_Green_White
  enumdat RAMPS_SKYBLUE_WHITE,    dw, BAPM_Ramps_SkyBlue_White
  enumdat RAMPS_VIOLET_WHITE,     dw, BAPM_Ramps_Violet_White
  ; Single chromatic aberrations
  enumdat RAMPS_BLURPLE,          dw, BAPM_Ramps_Blurple
  enumdat RAMPS_STRAWBERRY,       dw, BAPM_Ramps_Strawberry
  enumdat RAMPS_GOLD,             dw, BAPM_Ramps_Gold
  enumdat RAMPS_MINT,             dw, BAPM_Ramps_Mint
  enumdat RAMPS_ARC,              dw, BAPM_Ramps_Arc
  enumdat RAMPS_ICE,              dw, BAPM_Ramps_Ice
  enumdat RAMPS_FIRE,             dw, BAPM_Ramps_Fire
  enumdat RAMPS_GOLD_SILVER,      dw, BAPM_Ramps_Gold_Silver
  enumdat RAMPS_BUNSEN,           dw, BAPM_Ramps_Bunsen
  ; Single plain colours
  enumdat RAMPS_RED,              dw, BAPM_Ramps_Red
  enumdat RAMPS_BLAZOR,           dw, BAPM_Ramps_BlazeOrange
  enumdat RAMPS_ORANGE,           dw, BAPM_Ramps_Orange
  enumdat RAMPS_YELLOW,           dw, BAPM_Ramps_Yellow
  enumdat RAMPS_SNOT,             dw, BAPM_Ramps_Snot
  enumdat RAMPS_GREEN,            dw, BAPM_Ramps_Green
  enumdat RAMPS_SEAGREEN,         dw, BAPM_Ramps_SeaGreen
  enumdat RAMPS_CYAN,             dw, BAPM_Ramps_Cyan
  enumdat RAMPS_SKYBLUE,          dw, BAPM_Ramps_SkyBlue
  enumdat RAMPS_BLUE,             dw, BAPM_Ramps_Blue
  enumdat RAMPS_PURPLE,           dw, BAPM_Ramps_Purple
  enumdat RAMPS_VIOLET,           dw, BAPM_Ramps_Violet
  enumdat RAMPS_MAGENTA,          dw, BAPM_Ramps_Magenta
  enumdat RAMPS_CERISE,           dw, BAPM_Ramps_Cerise
  enumdat RAMPS_PINK,             dw, BAPM_Ramps_Pink
  enumdat RAMPS_WHITE,            dw, BAPM_Ramps_White
NUM_USER_RAMP_PAIRS equ ENUMIX
  ; Tricolours
  enumdat RAMPS_TRI_REV_CYB,      dw, BAPM_Ramps_Tri_RevCYB
  enumdat RAMPS_TRI_REV_CWS,      dw, BAPM_Ramps_Tri_RevCWS
  enumdat RAMPS_TRI_REV_RPB,      dw, BAPM_Ramps_Tri_RevRPB
  enumdat RAMPS_TRI_RGB,          dw, BAPM_Ramps_Tri_RGB
  enumdat RAMPS_TRI_RWB,          dw, BAPM_Ramps_Tri_RWB
  enumdat RAMPS_TRI_CMY,          dw, BAPM_Ramps_Tri_CMY
  enumdat RAMPS_TRI_BWR,          dw, BAPM_Ramps_Tri_BWR
  enumdat RAMPS_TRI_REV_RYG,      dw, BAPM_Ramps_Tri_RevRYG
  enumdat RAMPS_TRI_REV_GWR,      dw, BAPM_Ramps_Tri_RevGWR
  enumdat RAMPS_TRI_REV_GWO,      dw, BAPM_Ramps_Tri_RevGWO
  enumdat RAMPS_TRI_RWR,          dw, BAPM_Ramps_Tri_RWR
  enumdat RAMPS_TRI_GWG,          dw, BAPM_Ramps_Tri_GWG
  enumdat RAMPS_TRI_CWC,          dw, BAPM_Ramps_Tri_CWC
  enumdat RAMPS_TRI_BWB,          dw, BAPM_Ramps_Tri_BWB
  enumdat RAMPS_TRI_CSB,          dw, BAPM_Ramps_Tri_CSB
NUM_RAMP_PAIRS equ ENUMIX


;------------------------------------------------------------------------------
; Patterns
;------------------------------------------------------------------------------


BAPM_Map_SixSolidColoursPlusBW:
        bablock
        dbv8b   BLK, GRN, YEL, SKY, RED, CER, SGR, WHT
        endbab

BAPM_Map_SixSolidColoursPlusBX:
        bablock
        dbv8b   BLK, GRN, YEL, SKY, RED, CER, SGR, XEN
        endbab

; Triangular rainbow fade pattern
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

BAPM_Frs_TriRainbow:
        bablock
        pf24b     1, 0, 0x0000FF, 0x00FF00, 0xFF0000
        pf24b   255, 1, 0x0100FF, 0x00FF01, 0xFF0100
        pf24b   255, 1, 0xFF0100, 0x0100FF, 0x00FF01
        pf24b   254, 1, 0x00FF01, 0xFF0100, 0x0100FF
        endbab

BAPM_Frs_CMYTriRainbow:
        bablock
        pf24b     1, 0, 0x00AAAA, 0xAA00AA, 0xAAAA00
        pf24b   160, 1, 0x01FF00, 0x0001FF, 0xFF0001
        pf24b   160, 1, 0x0001FF, 0xFF0001, 0x01FF00
        pf24b   159, 1, 0xFF0001, 0x01FF00, 0x0001FF
        endbab

BAPM_Frs_LagTriRainbow:
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

BAPM_Frs_UnisonTriRainbow:
        bablock
        pf24b     1, 0, 0xCC0033, 0xCC0033, 0xCC0033
        pf24b    51, 1, 0x0100FF, 0x0100FF, 0x0100FF
        pf24b   255, 1, 0xFF0100, 0xFF0100, 0xFF0100
        pf24b   255, 1, 0x00FF01, 0x00FF01, 0x00FF01
        pf24b   203, 1, 0x0100FF, 0x0100FF, 0x0100FF
        endbab

PM_Pat_TriRainbow:
        pattern PATDF_24B, 0, 1, PATSF_BAPM_F, 4
        dw  0, 0, BAPM_Frs_TriRainbow

PM_Pat_CMYTriRainbow:
        pattern PATDF_24B, 0, 2, PATSF_BAPM_F, 4
        dw  0, 0, BAPM_Frs_CMYTriRainbow

PM_Pat_LagTriRainbow:
        pattern PATDF_24B, 0, 4, PATSF_BAPM_F, 10
        dw  0, 0, BAPM_Frs_LagTriRainbow

PM_Pat_UnisonTriRainbow:
        pattern PATDF_24B, 0, 15, PATSF_BAPM_F, 5
        dw  0, 0, BAPM_Frs_UnisonTriRainbow

;BAPM_Frs_HexRainbow:
;        bablock
;        pf24b     1, 0, 0xCC0000, 0x00CC00, 0x0000CC
;        pf24b   204, 1, 0x000100, 0x000001, 0x010000
;        pf24b   204, 1, 0xFF0000, 0x00FF00, 0x0000FF
;        pf24b   204, 1, 0x000001, 0x010000, 0x000100
;        pf24b   204, 1, 0x00FF00, 0x0000FF, 0xFF0000
;        pf24b   204, 1, 0x010000, 0x000100, 0x000001
;        pf24b   203, 1, 0x0000FF, 0xFF0000, 0x00FF00
;        endbab
;
;PM_Pat_HexRainbow:
;        pattern PATDF_24B, 0, 3, PATSF_BAPM_F, 7
;        dw  0, 0, BAPM_Frs_HexRainbow

PM_Pat_Solid:
        pattern PATDF_4C, 4, 100, PATSF_BAPM_PMF, 1
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, 0
        bablock
        pf4c      1,   1,  1,  1
        endbab

PM_Map_SolidSteps:
        bablock
        dbv6b   RED, SGR, PUR, BLZ, CYN, VIO
        dbv6b   GRN, ORA, BLU, MAG, SNT, SKY
        dbv4b   CER, YEL, PNK, WHT
        endbab
PM_Frs_SolidSteps:
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
PM_Pat_SolidSteps_Fast:
        pattern PATDF_16C, 16, 1, PATSF_BAPM_PMF, 16
        dw BAPM_Pal_Basic, PM_Map_SolidSteps, PM_Frs_SolidSteps
PM_Pat_SolidSteps_Slow:
        pattern PATDF_16C, 16, 222, PATSF_BAPM_PMF, 16
        dw BAPM_Pal_Basic, PM_Map_SolidSteps, PM_Frs_SolidSteps

PM_Frs_Flashy:
        bablock
        pf16c     1,   1,  1,  1
        pf16c     1,   0,  0,  0
        pf16c     1,   4,  4,  4
        pf16c     1,   0,  0,  0
        endbab
PM_Pat_Flash_T1:
        pattern PATDF_16C, 7, 1, PATSF_BAPM_PMF, 4
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, PM_Frs_Flashy
PM_Pat_Flash_T2:
        pattern PATDF_16C, 7, 2, PATSF_BAPM_PMF, 4
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, PM_Frs_Flashy
PM_Pat_Flash_T3:
        pattern PATDF_16C, 7, 3, PATSF_BAPM_PMF, 4
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, PM_Frs_Flashy
PM_Pat_Flash_T4:
        pattern PATDF_16C, 7, 4, PATSF_BAPM_PMF, 4
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, PM_Frs_Flashy

PM_Frs_Alternate:
        bablock
        pf16c     1,   1,  1,  1
        pf16c     1,   4,  4,  4
        endbab
PM_Pat_Alternate_T3:
        pattern PATDF_16C, 7, 3, PATSF_BAPM_PMF, 2
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, PM_Frs_Alternate
PM_Pat_Alternate_T5:
        pattern PATDF_16C, 7, 5, PATSF_BAPM_PMF, 2
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, PM_Frs_Alternate
PM_Pat_Alternate_T12:
        pattern PATDF_16C, 7, 12, PATSF_BAPM_PMF, 2
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, PM_Frs_Alternate

PM_Frs_RimWeightedFlash:
        bablock
        pf16c     1,   3,  2,  1
        pf16c     1,   0,  0,  0
        pf16c     1,   6,  5,  4
        pf16c     1,   0,  0,  0
        endbab
PM_Pat_RimWeightedSolid:
        pattern PATDF_16C, 7, 1, PATSF_BAPM_PMF, 1
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, PM_Frs_RimWeightedFlash
PM_Pat_RimWeightedFlash_T1:
        pattern PATDF_16C, 7, 1, PATSF_BAPM_PMF, 4
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, PM_Frs_RimWeightedFlash
PM_Pat_RimWeightedFlash_T2:
        pattern PATDF_16C, 7, 2, PATSF_BAPM_PMF, 4
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, PM_Frs_RimWeightedFlash
PM_Pat_RimWeightedFlash_T3:
        pattern PATDF_16C, 7, 3, PATSF_BAPM_PMF, 4
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, PM_Frs_RimWeightedFlash
PM_Pat_RimWeightedFlash_T4:
        pattern PATDF_16C, 7, 4, PATSF_BAPM_PMF, 4
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, PM_Frs_RimWeightedFlash

PM_Frs_SoftFlash:
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
        endbab
PM_Pat_SoftFlash_T1:
        pattern PATDF_16C, 7, 1, PATSF_BAPM_PMF, 12
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, PM_Frs_SoftFlash
PM_Pat_SoftFlash_T2:
        pattern PATDF_16C, 7, 2, PATSF_BAPM_PMF, 12
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, PM_Frs_SoftFlash

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
        endbab
PM_Pat_Dogs_T1:
        pattern PATDF_16C, 7, 1, PATSF_BAPM_PMF, 10
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_Dogs
PM_Pat_Dogs_T4:
        pattern PATDF_16C, 7, 4, PATSF_BAPM_PMF, 10
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_Dogs

BAPM_Frs_SoftDogs:
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
PM_Pat_SoftDogs_T1:
        pattern PATDF_16C, 7, 1, PATSF_BAPM_PMF, 10
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_SoftDogs
PM_Pat_SoftDogs_T4:
        pattern PATDF_16C, 7, 4, PATSF_BAPM_PMF, 10
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_SoftDogs

BAPM_Frs_Squares:
        bablock
        pf16c     4,   1,  1,  1
        pf16c     8,   1,  0,  1
        pf16c     4,   1,  1,  1
        pf16c     8,   0,  0,  0
        pf16c     4,   4,  4,  4
        pf16c     8,   4,  0,  4
        pf16c     4,   4,  4,  4
        pf16c     8,   0,  0,  0
        pf16c     4,   3,  3,  3
        pf16c     8,   3,  0,  3
        pf16c     4,   3,  3,  3
        pf16c     8,   0,  0,  0
        pf16c     4,   2,  2,  2
        pf16c     8,   2,  0,  2
        pf16c     4,   2,  2,  2
        pf16c     8,   0,  0,  0
        pf16c     4,   5,  5,  5
        pf16c     8,   5,  0,  5
        pf16c     4,   5,  5,  5
        pf16c     8,   0,  0,  0
        ;pf16c     4,   6,  6,  6
        ;pf16c     8,   6,  0,  6
        ;pf16c     4,   6,  6,  6
        ;pf16c     8,   0,  0,  0
        endbab
PM_Pat_Squares_Fixed5:
        pattern PATDF_16C, 7, 1, PATSF_BAPM_PMF, 4 * 5
        dw BAPM_Pal_Basic, BAPM_Map_SixSolidColoursPlusBW, BAPM_Frs_Squares
PM_Pat_Squares_User2:
        pattern PATDF_16C, 7, 1, PATSF_BAPM_PMF, 4 * 2
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_Squares

BAPM_Frs_QuantumSlalom:
        bablock
        pf16c     1,   7,  7,  7
        pf16c     2,   7,  0,  0
        pf16c     2,   7,  0,  1
        pf16c     2,   7,  0,  0
        pf16c     1,   7,  7,  7
        pf16c     2,   0,  0,  7
        pf16c     2,   5,  0,  7
        pf16c     2,   0,  0,  7
        pf16c     1,   7,  7,  7
        pf16c     2,   7,  0,  0
        pf16c     2,   7,  0,  2
        pf16c     2,   7,  0,  0
        pf16c     1,   7,  7,  7
        pf16c     2,   0,  0,  7
        pf16c     2,   3,  0,  7
        pf16c     2,   0,  0,  7
        pf16c     1,   7,  7,  7
        pf16c     2,   7,  0,  0
        pf16c     2,   7,  0,  4
        pf16c     2,   7,  0,  0
        pf16c     1,   7,  7,  7
        pf16c     2,   0,  0,  7
        pf16c     2,   6,  0,  7
        pf16c     2,   0,  0,  7
        endbab
PM_Pat_QuantumSlalom_Fixed6:
        pattern PATDF_16C, 8, 2, PATSF_BAPM_PMF, 4 * 6
        dw BAPM_Pal_Basic
        dw BAPM_Map_SixSolidColoursPlusBW
        dw BAPM_Frs_QuantumSlalom
PM_Pat_QuantumSlalom_User2:
        pattern PATDF_16C, 8, 2, PATSF_BAPM_PMF, 4 * 2
        dw BAPM_Pal_Basic
        dw BAPM_Map_SixSolidColoursPlusBW
        dw BAPM_Frs_QuantumSlalom

BAPM_Frs_Terminals:
        bablock
        pf16c     2,   0,  0,  1
        pf16c     3,   0,  0,  0
        pf16c     2,   5,  0,  0
        pf16c     3,   0,  0,  0
        pf16c     2,   0,  0,  2
        pf16c     3,   0,  0,  0
        pf16c     2,   3,  0,  0
        pf16c     3,   0,  0,  0
        pf16c     2,   0,  0,  4
        pf16c     3,   0,  0,  0
        pf16c     2,   6,  0,  0
        pf16c     3,   0,  0,  0
        endbab
PM_Pat_Terminals_Fixed6:
        pattern PATDF_16C, 8, 2, PATSF_BAPM_PMF, 2 * 6
        dw BAPM_Pal_Basic
        dw BAPM_Map_SixSolidColoursPlusBW
        dw BAPM_Frs_Terminals
PM_Pat_Terminals_User2:
        pattern PATDF_16C, 8, 2, PATSF_BAPM_PMF, 2 * 2
        dw BAPM_Pal_Basic
        dw BAPM_Map_SixSolidColoursPlusBW
        dw BAPM_Frs_Terminals

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
        pf16c     1,   3,  0,  0
        pf16c     1,   3,  3,  0
        pf16c     1,   3,  3,  3
        pf16c     1,   3,  3,  0
        pf16c     1,   3,  0,  0
        pf16c     1,   0,  0,  0
        pf16c     1,   2,  0,  0
        pf16c     1,   2,  2,  0
        pf16c     1,   2,  2,  2
        pf16c     1,   2,  2,  0
        pf16c     1,   2,  0,  0
        pf16c     1,   0,  0,  0
        pf16c     1,   5,  0,  0
        pf16c     1,   5,  5,  0
        pf16c     1,   5,  5,  5
        pf16c     1,   5,  5,  0
        pf16c     1,   5,  0,  0
        pf16c     1,   0,  0,  0
        ;pf16c     1,   6,  0,  0
        ;pf16c     1,   6,  6,  0
        ;pf16c     1,   6,  6,  6
        ;pf16c     1,   6,  6,  0
        ;pf16c     1,   6,  0,  0
        ;pf16c     1,   0,  0,  0
        endbab
PM_Pat_TrianglesNearMe_Fixed5:
        pattern PATDF_16C, 7, 4, PATSF_BAPM_PMF, 6 * 5
        dw BAPM_Pal_Basic, BAPM_Map_SixSolidColoursPlusBW, BAPM_Frs_TrianglesNearMe
PM_Pat_TrianglesNearMe_User2:
        pattern PATDF_16C, 7, 4, PATSF_BAPM_PMF, 6 * 2
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_TrianglesNearMe

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
PM_Pat_ZigZag_T1:
        pattern PATDF_16C, 7, 1, PATSF_BAPM_PMF, 4
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_ZigZagVs
PM_Pat_ZigZag_T4:
        pattern PATDF_16C, 7, 4, PATSF_BAPM_PMF, 4
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_ZigZagVs
PM_Pat_ConsummateVs_T1:
        pattern PATDF_16C, 7, 1, PATSF_BAPM_PMF, 10
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_ZigZagVs
PM_Pat_ConsummateVs_T4:
        pattern PATDF_16C, 7, 4, PATSF_BAPM_PMF, 10
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_ZigZagVs

BAPM_Frs_Step:
        bablock
        pf4c      3,   1,  0,  0
        pf4c      1,   1,  1,  1
        pf4c      3,   0,  0,  1
        pf4c      1,   1,  1,  1
        endbab
PM_Pat_Step_T1:
        pattern PATDF_4C, 4, 1, PATSF_BAPM_PMF, 4
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_Step
PM_Pat_Step_T2:
        pattern PATDF_4C, 4, 2, PATSF_BAPM_PMF, 4
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_Step
PM_Pat_Step_T3:
        pattern PATDF_4C, 4, 3, PATSF_BAPM_PMF, 4
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_Step
PM_Pat_Step_T4:
        pattern PATDF_4C, 4, 4, PATSF_BAPM_PMF, 4
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_Step
PM_Pat_Step_T5:
        pattern PATDF_4C, 4, 5, PATSF_BAPM_PMF, 4
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_Step

BAPM_Frs_Spokes:
        bablock
        pf4c      1,   1,  1,  1
        pf4c      1,   1,  0,  0
        endbab
PM_Pat_Spokes_T1:
        pattern PATDF_4C, 4, 1, PATSF_BAPM_PMF, 2
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_Spokes
PM_Pat_Spokes_T2:
        pattern PATDF_4C, 4, 2, PATSF_BAPM_PMF, 2
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_Spokes
PM_Pat_Spokes_T3:
        pattern PATDF_4C, 4, 3, PATSF_BAPM_PMF, 2
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_Spokes
PM_Pat_Spokes_T4:
        pattern PATDF_4C, 4, 4, PATSF_BAPM_PMF, 2
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_Spokes

BAPM_Frs_Spokes2:
        bablock
        pf16c     1,   2,  0,  0
        pf16c     2,   1,  1,  1
        pf16c     1,   2,  0,  0
        pf16c     1,   5,  0,  0
        pf16c     2,   4,  4,  4
        pf16c     1,   5,  0,  0
        endbab
PM_Pat_Spokes2_T1:
        pattern PATDF_16C, 7, 1, PATSF_BAPM_PMF, 6
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_Spokes2
PM_Pat_Spokes2_T2:
        pattern PATDF_16C, 7, 2, PATSF_BAPM_PMF, 6
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_Spokes2

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
        endbab
PM_Pat_WavySpokes:
        pattern PATDF_16C, 7, 1, PATSF_BAPM_PMF, 16
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_WavySpokes

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
        endbab
PM_Pat_Chevrons:
        pattern PATDF_16C, 7, 1, PATSF_BAPM_PMF, 24
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_Chevrons

BAPM_Frs_Weave:
        bablock
        pf16c     1,   3,  5,  3
        pf16c     1,   2,  6,  2
        pf16c     1,   1,  0,  1
        pf16c     1,   2,  6,  2
        pf16c     1,   3,  5,  3
        pf16c     1,   0,  4,  0
        endbab
PM_Pat_Weave_T1:
        pattern PATDF_16C, 7, 1, PATSF_BAPM_PMF, 6
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_Weave
PM_Pat_Weave_T2:
        pattern PATDF_16C, 7, 2, PATSF_BAPM_PMF, 6
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_Weave

BAPM_Frs_LumCrawl:
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
PM_Pat_LumCrawl_T1:
        pattern PATDF_16C, 7, 1, PATSF_BAPM_PMF, 32
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_LumCrawl
PM_Pat_LumCrawl_T2:
        pattern PATDF_16C, 7, 2, PATSF_BAPM_PMF, 32
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_LumCrawl

BAPM_Frs_ChequerK:
        bablock
        pf16c     1,   1,  0,  1
        pf16c     1,   0,  1,  0
        endbab
PM_Pat_ChequerK_T3:
        pattern PATDF_16C, 8, 3, PATSF_BAPM_PMF, 2
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_ChequerK
PM_Pat_ChequerK_T6:
        pattern PATDF_16C, 8, 6, PATSF_BAPM_PMF, 2
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_ChequerK

BAPM_Frs_ChequerW:
        bablock
        pf16c     1,   1,  7,  1
        pf16c     1,   7,  1,  7
        endbab
PM_Pat_ChequerW_T3:
        pattern PATDF_16C, 8, 3, PATSF_BAPM_PMF, 2
        dw BAPM_Pal_Basic, BAPM_Map_SixSolidColoursPlusBW, BAPM_Frs_ChequerW
PM_Pat_ChequerW_T6:
        pattern PATDF_16C, 8, 6, PATSF_BAPM_PMF, 2
        dw BAPM_Pal_Basic, BAPM_Map_SixSolidColoursPlusBW, BAPM_Frs_ChequerW

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

PM_Pat_AWL:  ; Aircraft Warning Light
        pattern PATDF_4C, 4, 1, PATSF_BAPM_PMF, 1
        dw BAPM_Pal_Basic, BAPM_Ramps_Red, 0
        bablock
        pf4c      1,   0,  0,  1
        endbab

BAPM_Frs_TriConc:
        bablock
        pf4c      1,   1,  2,  3
        pf4c      1,   0,  0,  0
        pf4c      1,   1,  2,  3
        pf4c      1,   1,  2,  3
        endbab
BAPM_Pat_TriC_Solid:
        pattern PATDF_4C, 4, 1, PATSF_BAPM_PMF, 1
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_TriConc
BAPM_Pat_TriC_Flash_T4:
        pattern PATDF_4C, 4, 4, PATSF_BAPM_PMF, 2
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_TriConc
BAPM_Pat_TriC_Flash_T8:
        pattern PATDF_4C, 4, 8, PATSF_BAPM_PMF, 2
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_TriConc
BAPM_Pat_TriC_Dashes_T6:
        pattern PATDF_4C, 4, 6, PATSF_BAPM_PMF, 4
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_TriConc

BAPM_Frs_TriRad:
        bablock
        pf4c      1,   1,  1,  1
        pf4c      1,   2,  2,  2
        pf4c      1,   3,  3,  3
        pf4c      1,   0,  0,  0
        endbab
BAPM_Pat_TriR_Solid_T7:
        pattern PATDF_4C, 4, 7, PATSF_BAPM_PMF, 3
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_TriRad
BAPM_Pat_TriR_Spaced_T5:
        pattern PATDF_4C, 4, 5, PATSF_BAPM_PMF, 4
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_TriRad
BAPM_Pat_TriR_Spaced_T9:
        pattern PATDF_4C, 4, 9, PATSF_BAPM_PMF, 4
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_TriRad

BAPM_Frs_TriChaser:
        bablock
        pf4c      1,   1,  2,  3
        pf4c      1,   3,  1,  2
        pf4c      1,   2,  3,  1
        endbab
BAPM_Pat_TriChaser:
        pattern PATDF_4C, 4, 148, PATSF_BAPM_PMF, 3
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_TriChaser

BAPM_Frs_BlurStep:
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
PM_Pat_BlurStep:
        pattern PATDF_16C, 7, 2, PATSF_BAPM_PMF, 12
        dw BAPM_Pal_Basic, BAPM_Map_SixSolidColoursPlusBW, BAPM_Frs_BlurStep

BAPM_Frs_IntenseDecay:
        bablock
        pf16c     1,   7,  7,  7
        pf16c     1,   1,  1,  1
        pf16c     1,   2,  2,  2
        pf16c     1,   3,  3,  3
        pf16c     1,   0,  0,  0
        pf16c     1,   0,  0,  0
        pf16c     1,   7,  7,  7
        pf16c     1,   4,  4,  4
        pf16c     1,   5,  5,  5
        pf16c     1,   6,  6,  6
        pf16c     1,   0,  0,  0
        pf16c     1,   0,  0,  0
        endbab
PM_Pat_IntenseDecay:
        pattern PATDF_16C, 8, 2, PATSF_BAPM_PMF, 12
        dw BAPM_Pal_Basic
        dw BAPM_Map_SixSolidColoursPlusBX
        dw BAPM_Frs_IntenseDecay

BAPM_Frs_Bookends:
        bablock
        pf16c     1,   3,  3,  3
        pf16c     1,   2,  2,  2
        pf16c     1,   1,  1,  1
        pf16c     1,   1,  1,  1
        pf16c     1,   4,  4,  4
        pf16c     1,   7,  7,  7
        pf16c     1,   5,  5,  5
        pf16c     1,   6,  6,  6
        pf16c     1,   0,  0,  0
        pf16c     1,   0,  0,  0
        endbab
PM_Pat_Bookends:
        pattern PATDF_16C, 7, 1, PATSF_BAPM_PMF, 10
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_Bookends

BAPM_Frs_Slides:
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
PM_Pat_Slides:
        pattern PATDF_16C, 7, 15, PATSF_BAPM_PMF, 26
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_Slides

num_frames = 0
morse_dash macro
        pf4c      3,   1,  1,  1
        pf4c      1,   0,  0,  0
num_frames = num_frames + 2
  endm
morse_dot macro
        pf4c      1,   2,  2,  2
        pf4c      1,   0,  0,  0
num_frames = num_frames + 2
  endm
morse_letter_space macro
        pf4c      2,   0,  0,  0
num_frames = num_frames + 1
  endm
morse_word_space macro
        pf4c      3,   0,  0,  0
        pf4c      3,   0,  0,  0
num_frames = num_frames + 2
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

INCLUDE_MORSE equ 0

  if INCLUDE_MORSE

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

BAPM_Frs_Morse:
        bablock
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
        morse_word_space
        morse_word_space
        endbab
PM_Pat_Morse_PoV:
        pattern PATDF_4C, 4, 1, PATSF_BAPM_PMF, num_frames
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_Morse
PM_Pat_Morse_Fast:
        pattern PATDF_4C, 4, 33, PATSF_BAPM_PMF, num_frames
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_Morse
PM_Pat_Morse_Slow:
        pattern PATDF_4C, 4, 90, PATSF_BAPM_PMF, num_frames
        dw BAPM_Pal_Basic, BAPM_Ramps_Default, BAPM_Frs_Morse
  endif


;------------------------------------------------------------------------------
; Pattern address table, enumerated
;------------------------------------------------------------------------------


PM_PatAddrTable:
  resetenum
  enumdat PAT_UNISON_TRI_RAINBOW, dw, PM_Pat_UnisonTriRainbow
  enumdat PAT_TRI_RAINBOW,        dw, PM_Pat_TriRainbow
  enumdat PAT_CMY_TRI_RAINBOW,    dw, PM_Pat_CMYTriRainbow
  enumdat PAT_LAG_TRI_RAINBOW,    dw, PM_Pat_LagTriRainbow
  enumdat PAT_SOLID_STEPS_FAST,   dw, PM_Pat_SolidSteps_Fast
  enumdat PAT_SOLID_STEPS_SLOW,   dw, PM_Pat_SolidSteps_Slow
  enumdat PAT_SOLID,              dw, PM_Pat_Solid
  enumdat PAT_FLASH_T1,           dw, PM_Pat_Flash_T1
  enumdat PAT_FLASH_T2,           dw, PM_Pat_Flash_T2
  enumdat PAT_FLASH_T3,           dw, PM_Pat_Flash_T3
  enumdat PAT_FLASH_T4,           dw, PM_Pat_Flash_T4
  enumdat PAT_RIM_WTD_SOLID,      dw, PM_Pat_RimWeightedSolid
  enumdat PAT_RIM_WTD_FLASH_T1,   dw, PM_Pat_RimWeightedFlash_T1
  enumdat PAT_RIM_WTD_FLASH_T2,   dw, PM_Pat_RimWeightedFlash_T2
  enumdat PAT_RIM_WTD_FLASH_T3,   dw, PM_Pat_RimWeightedFlash_T3
  enumdat PAT_RIM_WTD_FLASH_T4,   dw, PM_Pat_RimWeightedFlash_T4
  enumdat PAT_ALTERNATE_T3,       dw, PM_Pat_Alternate_T3
  enumdat PAT_ALTERNATE_T5,       dw, PM_Pat_Alternate_T5
  enumdat PAT_ALTERNATE_T12,      dw, PM_Pat_Alternate_T12
  enumdat PAT_SOFT_FLASH_T1,      dw, PM_Pat_SoftFlash_T1
  enumdat PAT_SOFT_FLASH_T2,      dw, PM_Pat_SoftFlash_T2
  enumdat PAT_DOGS_T1,            dw, PM_Pat_Dogs_T1
  enumdat PAT_DOGS_T4,            dw, PM_Pat_Dogs_T4
  enumdat PAT_SOFT_DOGS_T1,       dw, PM_Pat_SoftDogs_T1
  enumdat PAT_SOFT_DOGS_T4,       dw, PM_Pat_SoftDogs_T4
  enumdat PAT_SQUARES_FIXED5,     dw, PM_Pat_Squares_Fixed5
  enumdat PAT_SQUARES_USER2,      dw, PM_Pat_Squares_User2
  enumdat PAT_QUANTUM_SLALOM_F6,  dw, PM_Pat_QuantumSlalom_Fixed6
  enumdat PAT_QUANTUM_SLALOM_U2,  dw, PM_Pat_QuantumSlalom_User2
  enumdat PAT_TERMINALS_FIXED6,   dw, PM_Pat_Terminals_Fixed6
  enumdat PAT_TERMINALS_USER2,    dw, PM_Pat_Terminals_User2
  enumdat PAT_TRIANGLES_NEAR_ME,  dw, PM_Pat_TrianglesNearMe_Fixed5
  enumdat PAT_TNM_USER2,          dw, PM_Pat_TrianglesNearMe_User2
  enumdat PAT_ZIGZAG_T1,          dw, PM_Pat_ZigZag_T1
  enumdat PAT_ZIGZAG_T4,          dw, PM_Pat_ZigZag_T4
  enumdat PAT_CONSUMMATE_VS_T1,   dw, PM_Pat_ConsummateVs_T1
  enumdat PAT_CONSUMMATE_VS_T4,   dw, PM_Pat_ConsummateVs_T4
  enumdat PAT_STEP_T1,            dw, PM_Pat_Step_T1
  enumdat PAT_STEP_T2,            dw, PM_Pat_Step_T2
  enumdat PAT_STEP_T3,            dw, PM_Pat_Step_T3
  enumdat PAT_STEP_T4,            dw, PM_Pat_Step_T4
  enumdat PAT_STEP_T5,            dw, PM_Pat_Step_T5
  enumdat PAT_SPOKES_T1,          dw, PM_Pat_Spokes_T1
  enumdat PAT_SPOKES_T2,          dw, PM_Pat_Spokes_T2
  enumdat PAT_SPOKES_T3,          dw, PM_Pat_Spokes_T3
  enumdat PAT_SPOKES_T4,          dw, PM_Pat_Spokes_T4
  enumdat PAT_SPOKES2_T1,         dw, PM_Pat_Spokes2_T1
  enumdat PAT_SPOKES2_T2,         dw, PM_Pat_Spokes2_T2
  enumdat PAT_WAVY_SPOKES,        dw, PM_Pat_WavySpokes
  enumdat PAT_CHEVRONS,           dw, PM_Pat_Chevrons
  enumdat PAT_WEAVE_T1,           dw, PM_Pat_Weave_T1
  enumdat PAT_WEAVE_T2,           dw, PM_Pat_Weave_T2
  enumdat PAT_LUM_CRAWL_T1,       dw, PM_Pat_LumCrawl_T1
  enumdat PAT_LUM_CRAWL_T2,       dw, PM_Pat_LumCrawl_T2
  enumdat PAT_CHEQUER_K_T3,       dw, PM_Pat_ChequerK_T3
  enumdat PAT_CHEQUER_K_T6,       dw, PM_Pat_ChequerK_T6
  enumdat PAT_CHEQUER_W_T3,       dw, PM_Pat_ChequerW_T3
  enumdat PAT_CHEQUER_W_T6,       dw, PM_Pat_ChequerW_T6
  enumdat PAT_BY_YOUR_COMMAND,    dw, PM_Pat_ByYourCommand
  enumdat PAT_AWL,                dw, PM_Pat_AWL
  enumdat PAT_BLUR_STEP,          dw, PM_Pat_BlurStep
  enumdat PAT_INTENSE_DECAY,      dw, PM_Pat_IntenseDecay
  enumdat PAT_BOOKENDS,           dw, PM_Pat_Bookends
  enumdat PAT_SLIDES,             dw, PM_Pat_Slides
  if INCLUDE_MORSE
    enumdat PAT_MORSE_POV,          dw, PM_Pat_Morse_PoV
    enumdat PAT_MORSE_FAST,         dw, PM_Pat_Morse_Fast
    enumdat PAT_MORSE_SLOW,         dw, PM_Pat_Morse_Slow
  endif
  enumdat PAT_TRIC_SOLID,         dw, BAPM_Pat_TriC_Solid
  enumdat PAT_TRIC_FLASH_T4,      dw, BAPM_Pat_TriC_Flash_T4
  enumdat PAT_TRIC_FLASH_T8,      dw, BAPM_Pat_TriC_Flash_T8
  enumdat PAT_TRIC_DASHES_T6,     dw, BAPM_Pat_TriC_Dashes_T6
  enumdat PAT_TRIR_SOLID_T7,      dw, BAPM_Pat_TriR_Solid_T7
  enumdat PAT_TRIR_SPACED_T5,     dw, BAPM_Pat_TriR_Spaced_T5
  enumdat PAT_TRIR_SPACED_T9,     dw, BAPM_Pat_TriR_Spaced_T9
  enumdat PAT_TRI_CHASER,         dw, BAPM_Pat_TriChaser


;------------------------------------------------------------------------------
; Pattern banks
;------------------------------------------------------------------------------


; Whether a pattern's colours are fixed or modifiable with External Ramps
; is indicated by the External ramps mode bit in the bank pattern record.
; (The old system of "ER" display formats is no longer used). The pattern's
; Frame Unit Period can be modified here with any non-zero FUP in a bank's
; pattern reference, saving the creation of mostly identicial pattern headers.

; Bank pattern entry format:
;
;                    bit          p = Enumerated pattern index (9 bits)
;               DCBA98 76543210   A = Primary ramp index (5 bits)
;               ---------------   B = Secondary ramp index (5 bits)
; First Word:   AAAAAp pppppppp   M = Middle ramp index (4 bits)
; Second Word:  BBBBBe MMMMTTTT   T = Frame unit period modifier
;                                 e = External ramps mode

pat_fixed macro pat_id, fup
    dw  (pat_id) & 0x1FF
    dw  ((fup) & 15)
  endm

pat_eramps macro pat_id, ramps_id, fup
    dw  (((ramps_id) & 31) << 9) | ((pat_id) & 0x1FF)
    dw  ((((ramps_id) >> 5) & 31) << 9) | 256 | (((0) & 15) << 4) | ((fup)&15)
  endm

pat_uramps macro pat_id, fup
    dw  (((RAMPS_USER) & 31) << 9) | ((pat_id) & 0x1FF)
    dw  ((((RAMPS_USER) >> 5) & 31) << 9) | 256 | (((0) & 15) << 4) | ((fup) & 15)
  endm

;~ pat_fixed macro pat_id
    ;~ dba3b (pat_id) & 0xFF, ((pat_id >> 1) & 0x80), 0
  ;~ endm

;~ pat_eramps macro pat_id, ramps_id
    ;~ dbab  (pat_id) & 255
    ;~ dbab  (((pat_id) >> 1) & 0x80) | (0x1 << 5) | ((ramps_id) & 31)
    ;~ dbab  (((0) & 3) << 5) | ((0) & 31)      | (((ramps_id) >> 5) << 7) ;<<<
  ;~ endm

;~ pat_uramps macro pat_id
    ;~ dbab  (pat_id) & 255
    ;~ dbab  (((pat_id) >> 1) & 0x80) | (0x2 << 5) | ((RAMPS_USER) & 31)
    ;~ dbab  0                                  | (((RAMPS_USER) >> 5) << 7) ;<<<
  ;~ endm

PM_PatTable_Bank0:
        dw  NUM_PATS_IN_BANK0
        ; Rainbows and solid colours
        pat_fixed   PAT_UNISON_TRI_RAINBOW, 0
        pat_fixed   PAT_TRI_RAINBOW, 0
        pat_fixed   PAT_CMY_TRI_RAINBOW, 0
        pat_fixed   PAT_LAG_TRI_RAINBOW, 0
        pat_fixed   PAT_SOLID_STEPS_SLOW, 0
        pat_fixed   PAT_SOLID_STEPS_FAST, 0
        pat_eramps  PAT_SOLID, RAMPS_RED, 0
        pat_eramps  PAT_SOLID, RAMPS_BLAZOR, 0
        pat_eramps  PAT_SOLID, RAMPS_ORANGE, 0
        pat_eramps  PAT_SOLID, RAMPS_YELLOW, 0
        pat_eramps  PAT_SOLID, RAMPS_SNOT, 0
        pat_eramps  PAT_SOLID, RAMPS_GREEN, 0
        pat_eramps  PAT_SOLID, RAMPS_SEAGREEN, 0
        pat_eramps  PAT_SOLID, RAMPS_CYAN, 0
        pat_eramps  PAT_SOLID, RAMPS_SKYBLUE, 0
        pat_eramps  PAT_SOLID, RAMPS_BLUE, 0
        pat_eramps  PAT_SOLID, RAMPS_PURPLE, 0
        pat_eramps  PAT_SOLID, RAMPS_VIOLET, 0
        pat_eramps  PAT_SOLID, RAMPS_MAGENTA, 0
        pat_eramps  PAT_SOLID, RAMPS_CERISE, 0
        pat_eramps  PAT_SOLID, RAMPS_PINK, 0
NUM_PATS_IN_BANK0 equ ($ - PM_PatTable_Bank0 - 1) / 2

PM_PatTable_Bank1:
        dw  NUM_PATS_IN_BANK1
        ; Tricolours - Concentric
        pat_eramps  PAT_TRIC_SOLID, RAMPS_TRI_REV_CYB, 0
        pat_eramps  PAT_TRIC_SOLID, RAMPS_TRI_REV_CWS, 0
        pat_eramps  PAT_TRIC_SOLID, RAMPS_TRI_REV_RPB, 0
        pat_eramps  PAT_TRIC_SOLID, RAMPS_TRI_RGB, 0
        pat_eramps  PAT_TRIC_SOLID, RAMPS_TRI_RWB, 0
        pat_eramps  PAT_TRIC_SOLID, RAMPS_TRI_CMY, 0
        pat_eramps  PAT_TRIC_SOLID, RAMPS_TRI_BWR, 0
        pat_eramps  PAT_TRIC_SOLID, RAMPS_TRI_REV_RYG, 0
        pat_eramps  PAT_TRIC_SOLID, RAMPS_TRI_REV_GWR, 0
        pat_eramps  PAT_TRIC_SOLID, RAMPS_TRI_REV_GWO, 0
        pat_eramps  PAT_TRIC_SOLID, RAMPS_TRI_RWR, 0
        pat_eramps  PAT_TRIC_SOLID, RAMPS_TRI_GWG, 0
        pat_eramps  PAT_TRIC_SOLID, RAMPS_TRI_CWC, 0
        pat_eramps  PAT_TRIC_SOLID, RAMPS_TRI_BWB, 0
        pat_eramps  PAT_TRIC_SOLID, RAMPS_TRI_CSB, 0
        pat_eramps  PAT_TRIC_FLASH_T8, RAMPS_TRI_REV_CYB, 0
        pat_eramps  PAT_TRIC_FLASH_T8, RAMPS_TRI_REV_CWS, 0
        pat_eramps  PAT_TRIC_FLASH_T8, RAMPS_TRI_REV_RPB, 0
        pat_eramps  PAT_TRIC_FLASH_T8, RAMPS_TRI_RGB, 0
        pat_eramps  PAT_TRIC_FLASH_T8, RAMPS_TRI_RWB, 0
        pat_eramps  PAT_TRIC_FLASH_T8, RAMPS_TRI_CMY, 0
        pat_eramps  PAT_TRIC_FLASH_T8, RAMPS_TRI_BWR, 0
        pat_eramps  PAT_TRIC_FLASH_T8, RAMPS_TRI_REV_RYG, 0
        pat_eramps  PAT_TRIC_FLASH_T8, RAMPS_TRI_REV_GWR, 0
        pat_eramps  PAT_TRIC_FLASH_T8, RAMPS_TRI_REV_GWO, 0
        pat_eramps  PAT_TRIC_FLASH_T8, RAMPS_TRI_RWR, 0
        pat_eramps  PAT_TRIC_FLASH_T8, RAMPS_TRI_GWG, 0
        pat_eramps  PAT_TRIC_FLASH_T8, RAMPS_TRI_CWC, 0
        pat_eramps  PAT_TRIC_FLASH_T8, RAMPS_TRI_BWB, 0
        pat_eramps  PAT_TRIC_FLASH_T8, RAMPS_TRI_CSB, 0
        pat_eramps  PAT_TRIC_FLASH_T4, RAMPS_TRI_REV_CYB, 0
        pat_eramps  PAT_TRIC_FLASH_T4, RAMPS_TRI_REV_CWS, 0
        pat_eramps  PAT_TRIC_FLASH_T4, RAMPS_TRI_REV_RPB, 0
        pat_eramps  PAT_TRIC_FLASH_T4, RAMPS_TRI_RGB, 0
        pat_eramps  PAT_TRIC_FLASH_T4, RAMPS_TRI_RWB, 0
        pat_eramps  PAT_TRIC_FLASH_T4, RAMPS_TRI_CMY, 0
        pat_eramps  PAT_TRIC_FLASH_T4, RAMPS_TRI_BWR, 0
        pat_eramps  PAT_TRIC_FLASH_T4, RAMPS_TRI_REV_RYG, 0
        pat_eramps  PAT_TRIC_FLASH_T4, RAMPS_TRI_REV_GWR, 0
        pat_eramps  PAT_TRIC_FLASH_T4, RAMPS_TRI_REV_GWO, 0
        pat_eramps  PAT_TRIC_FLASH_T4, RAMPS_TRI_RWR, 0
        pat_eramps  PAT_TRIC_FLASH_T4, RAMPS_TRI_GWG, 0
        pat_eramps  PAT_TRIC_FLASH_T4, RAMPS_TRI_CWC, 0
        pat_eramps  PAT_TRIC_FLASH_T4, RAMPS_TRI_BWB, 0
        pat_eramps  PAT_TRIC_FLASH_T4, RAMPS_TRI_CSB, 0
        pat_eramps  PAT_TRIC_DASHES_T6, RAMPS_TRI_REV_CYB, 0
        pat_eramps  PAT_TRIC_DASHES_T6, RAMPS_TRI_REV_CWS, 0
        pat_eramps  PAT_TRIC_DASHES_T6, RAMPS_TRI_REV_RPB, 0
        pat_eramps  PAT_TRIC_DASHES_T6, RAMPS_TRI_RGB, 0
        pat_eramps  PAT_TRIC_DASHES_T6, RAMPS_TRI_RWB, 0
        pat_eramps  PAT_TRIC_DASHES_T6, RAMPS_TRI_CMY, 0
        pat_eramps  PAT_TRIC_DASHES_T6, RAMPS_TRI_BWR, 0
        pat_eramps  PAT_TRIC_DASHES_T6, RAMPS_TRI_REV_RYG, 0
        pat_eramps  PAT_TRIC_DASHES_T6, RAMPS_TRI_REV_GWR, 0
        pat_eramps  PAT_TRIC_DASHES_T6, RAMPS_TRI_REV_GWO, 0
        pat_eramps  PAT_TRIC_DASHES_T6, RAMPS_TRI_RWR, 0
        pat_eramps  PAT_TRIC_DASHES_T6, RAMPS_TRI_GWG, 0
        pat_eramps  PAT_TRIC_DASHES_T6, RAMPS_TRI_CWC, 0
        pat_eramps  PAT_TRIC_DASHES_T6, RAMPS_TRI_BWB, 0
        pat_eramps  PAT_TRIC_DASHES_T6, RAMPS_TRI_CSB, 0
NUM_PATS_IN_BANK1 equ ($ - PM_PatTable_Bank1 - 1) / 2

PM_PatTable_Bank2:
        dw  NUM_PATS_IN_BANK2
        ; Tricolours - Chasers and radial bars
        pat_eramps  PAT_TRI_CHASER, RAMPS_TRI_REV_CYB, 0
        pat_eramps  PAT_TRI_CHASER, RAMPS_TRI_REV_CWS, 0
        pat_eramps  PAT_TRI_CHASER, RAMPS_TRI_REV_RPB, 0
        pat_eramps  PAT_TRI_CHASER, RAMPS_TRI_RGB, 0
        pat_eramps  PAT_TRI_CHASER, RAMPS_TRI_RWB, 0
        pat_eramps  PAT_TRI_CHASER, RAMPS_TRI_CMY, 0
        pat_eramps  PAT_TRI_CHASER, RAMPS_TRI_BWR, 0
        pat_eramps  PAT_TRI_CHASER, RAMPS_TRI_REV_RYG, 0
        pat_eramps  PAT_TRI_CHASER, RAMPS_TRI_REV_GWR, 0
        pat_eramps  PAT_TRI_CHASER, RAMPS_TRI_REV_GWO, 0
        pat_eramps  PAT_TRI_CHASER, RAMPS_TRI_RWR, 0
        pat_eramps  PAT_TRI_CHASER, RAMPS_TRI_GWG, 0
        pat_eramps  PAT_TRI_CHASER, RAMPS_TRI_CWC, 0
        pat_eramps  PAT_TRI_CHASER, RAMPS_TRI_BWB, 0
        pat_eramps  PAT_TRI_CHASER, RAMPS_TRI_CSB, 0
        pat_eramps  PAT_TRIR_SOLID_T7, RAMPS_TRI_REV_CYB, 0
        pat_eramps  PAT_TRIR_SOLID_T7, RAMPS_TRI_REV_CWS, 0
        pat_eramps  PAT_TRIR_SOLID_T7, RAMPS_TRI_REV_RPB, 0
        pat_eramps  PAT_TRIR_SOLID_T7, RAMPS_TRI_RGB, 0
        pat_eramps  PAT_TRIR_SOLID_T7, RAMPS_TRI_RWB, 0
        pat_eramps  PAT_TRIR_SOLID_T7, RAMPS_TRI_CMY, 0
        pat_eramps  PAT_TRIR_SOLID_T7, RAMPS_TRI_BWR, 0
        pat_eramps  PAT_TRIR_SOLID_T7, RAMPS_TRI_REV_RYG, 0
        pat_eramps  PAT_TRIR_SOLID_T7, RAMPS_TRI_REV_GWR, 0
        pat_eramps  PAT_TRIR_SOLID_T7, RAMPS_TRI_REV_GWO, 0
        pat_eramps  PAT_TRIR_SOLID_T7, RAMPS_TRI_RWR, 0
        pat_eramps  PAT_TRIR_SOLID_T7, RAMPS_TRI_GWG, 0
        pat_eramps  PAT_TRIR_SOLID_T7, RAMPS_TRI_CWC, 0
        pat_eramps  PAT_TRIR_SOLID_T7, RAMPS_TRI_BWB, 0
        pat_eramps  PAT_TRIR_SOLID_T7, RAMPS_TRI_CSB, 0
        pat_eramps  PAT_TRIR_SPACED_T9, RAMPS_TRI_REV_CYB, 0
        pat_eramps  PAT_TRIR_SPACED_T9, RAMPS_TRI_REV_CWS, 0
        pat_eramps  PAT_TRIR_SPACED_T9, RAMPS_TRI_REV_RPB, 0
        pat_eramps  PAT_TRIR_SPACED_T9, RAMPS_TRI_RGB, 0
        pat_eramps  PAT_TRIR_SPACED_T9, RAMPS_TRI_RWB, 0
        pat_eramps  PAT_TRIR_SPACED_T9, RAMPS_TRI_CMY, 0
        pat_eramps  PAT_TRIR_SPACED_T9, RAMPS_TRI_BWR, 0
        pat_eramps  PAT_TRIR_SPACED_T9, RAMPS_TRI_REV_RYG, 0
        pat_eramps  PAT_TRIR_SPACED_T9, RAMPS_TRI_REV_GWR, 0
        pat_eramps  PAT_TRIR_SPACED_T9, RAMPS_TRI_REV_GWO, 0
        pat_eramps  PAT_TRIR_SPACED_T9, RAMPS_TRI_RWR, 0
        pat_eramps  PAT_TRIR_SPACED_T9, RAMPS_TRI_GWG, 0
        pat_eramps  PAT_TRIR_SPACED_T9, RAMPS_TRI_CWC, 0
        pat_eramps  PAT_TRIR_SPACED_T9, RAMPS_TRI_BWB, 0
        pat_eramps  PAT_TRIR_SPACED_T9, RAMPS_TRI_CSB, 0
        pat_eramps  PAT_TRIR_SPACED_T5, RAMPS_TRI_REV_CYB, 0
        pat_eramps  PAT_TRIR_SPACED_T5, RAMPS_TRI_REV_CWS, 0
        pat_eramps  PAT_TRIR_SPACED_T5, RAMPS_TRI_REV_RPB, 0
        pat_eramps  PAT_TRIR_SPACED_T5, RAMPS_TRI_RGB, 0
        pat_eramps  PAT_TRIR_SPACED_T5, RAMPS_TRI_RWB, 0
        pat_eramps  PAT_TRIR_SPACED_T5, RAMPS_TRI_CMY, 0
        pat_eramps  PAT_TRIR_SPACED_T5, RAMPS_TRI_BWR, 0
        pat_eramps  PAT_TRIR_SPACED_T5, RAMPS_TRI_REV_RYG, 0
        pat_eramps  PAT_TRIR_SPACED_T5, RAMPS_TRI_REV_GWR, 0
        pat_eramps  PAT_TRIR_SPACED_T5, RAMPS_TRI_REV_GWO, 0
        pat_eramps  PAT_TRIR_SPACED_T5, RAMPS_TRI_RWR, 0
        pat_eramps  PAT_TRIR_SPACED_T5, RAMPS_TRI_GWG, 0
        pat_eramps  PAT_TRIR_SPACED_T5, RAMPS_TRI_CWC, 0
        pat_eramps  PAT_TRIR_SPACED_T5, RAMPS_TRI_BWB, 0
        pat_eramps  PAT_TRIR_SPACED_T5, RAMPS_TRI_CSB, 0
NUM_PATS_IN_BANK2 equ ($ - PM_PatTable_Bank2 - 1) / 2

PM_PatTable_Bank3:
        dw  NUM_PATS_IN_BANK3
        ; One colour
        pat_eramps  PAT_ZIGZAG_T4, RAMPS_VIOLET, 0
        pat_eramps  PAT_ZIGZAG_T1, RAMPS_ORANGE, 0
        pat_eramps  PAT_ZIGZAG_T1, RAMPS_CYAN, 0
        pat_eramps  PAT_STEP_T5, RAMPS_SEAGREEN, 0
        pat_eramps  PAT_STEP_T4, RAMPS_VIOLET, 0
        pat_eramps  PAT_STEP_T3, RAMPS_BLUE, 0
        pat_eramps  PAT_STEP_T2, RAMPS_YELLOW, 0
        pat_eramps  PAT_STEP_T1, RAMPS_CYAN, 0
        pat_eramps  PAT_CHEQUER_K_T6, RAMPS_SKYBLUE, 0
        pat_eramps  PAT_CHEQUER_K_T3, RAMPS_MAGENTA, 0
        pat_eramps  PAT_CHEQUER_W_T6, RAMPS_RED, 0
        pat_eramps  PAT_CHEQUER_W_T3, RAMPS_BLUE, 0
        pat_eramps  PAT_SPOKES_T4, RAMPS_VIOLET, 0
        pat_eramps  PAT_SPOKES_T3, RAMPS_RED, 0
        pat_eramps  PAT_SPOKES_T2, RAMPS_YELLOW, 0
        pat_eramps  PAT_SPOKES_T1, RAMPS_SKYBLUE, 0
NUM_PATS_IN_BANK3 equ ($ - PM_PatTable_Bank3 - 1) / 2

PM_PatTable_Bank4:
        dw  NUM_PATS_IN_BANK4
        ; Two colours
        pat_eramps  PAT_FLASH_T4, RAMPS_RED_WHITE, 0
        pat_eramps  PAT_FLASH_T4, RAMPS_VIOLET, 0
        pat_eramps  PAT_FLASH_T4, RAMPS_GREEN, 0
        pat_eramps  PAT_FLASH_T3, RAMPS_RED_CERISE, 0
        pat_eramps  PAT_FLASH_T2, RAMPS_YELLOW_SKYBLUE, 0
        pat_eramps  PAT_FLASH_T1, RAMPS_RED_SKYBLUE, 0
        pat_eramps  PAT_FLASH_T1, RAMPS_SKYBLUE, 0
        pat_eramps  PAT_ALTERNATE_T12, RAMPS_ORANGE_VIOLET, 0
        pat_eramps  PAT_ALTERNATE_T12, RAMPS_GREEN_WHITE, 0
        pat_eramps  PAT_ALTERNATE_T5, RAMPS_SKYBLUE_WHITE, 0
        pat_eramps  PAT_ALTERNATE_T3, RAMPS_RED_SKYBLUE, 0
        pat_eramps  PAT_SQUARES_USER2, RAMPS_RED_WHITE, 0
        pat_eramps  PAT_QUANTUM_SLALOM_U2, RAMPS_CERISE_GREEN, 0
        pat_eramps  PAT_TERMINALS_USER2, RAMPS_RED_SKYBLUE, 0
        pat_eramps  PAT_DOGS_T4, RAMPS_ORANGE_VIOLET, 0
        pat_eramps  PAT_DOGS_T4, RAMPS_FIRE_ICE, 0
        pat_eramps  PAT_DOGS_T1, RAMPS_SKYBLUE_WHITE, 0
        pat_eramps  PAT_DOGS_T4, RAMPS_YELLOW_SKYBLUE, 0
        pat_eramps  PAT_DOGS_T1, RAMPS_ICE_STRAWBERRY, 0
        pat_eramps  PAT_DOGS_T1, RAMPS_SKYBLUE_GREEN, 0
        pat_eramps  PAT_TNM_USER2, RAMPS_BLAZOR_SKYBLUE, 0
        pat_eramps  PAT_CONSUMMATE_VS_T4, RAMPS_YELLOW_SKYBLUE, 0
        pat_eramps  PAT_CONSUMMATE_VS_T1, RAMPS_FIRE_ICE, 0
        pat_eramps  PAT_CONSUMMATE_VS_T1, RAMPS_SKYBLUE_WHITE, 0
NUM_PATS_IN_BANK4 equ ($ - PM_PatTable_Bank4 - 1) / 2

PM_PatTable_Bank5:
        dw  NUM_PATS_IN_BANK5
        ; Two colour ramps, Set A
        pat_eramps  PAT_WEAVE_T2, RAMPS_FIRE_ICE, 0
        pat_eramps  PAT_WEAVE_T1, RAMPS_ICE_STRAWBERRY, 0
        pat_eramps  PAT_WEAVE_T2, RAMPS_SPECTRAL_VIOLENCE, 0
        pat_eramps  PAT_WEAVE_T2, RAMPS_SKYBLUE, 0
        pat_eramps  PAT_SOFT_FLASH_T2, RAMPS_BLURPLE, 0
        pat_eramps  PAT_SOFT_FLASH_T1, RAMPS_SEAGREEN, 0
        pat_eramps  PAT_SOFT_FLASH_T2, RAMPS_GOLD_SILVER, 0
        pat_eramps  PAT_SOFT_FLASH_T1, RAMPS_FIRE_ICE, 0
        pat_eramps  PAT_SOFT_FLASH_T1, RAMPS_ARC, 0
        pat_eramps  PAT_SOFT_DOGS_T4, RAMPS_SPECTRAL_VIOLENCE, 0
        pat_eramps  PAT_SOFT_DOGS_T4, RAMPS_BLURPLE, 0
        pat_eramps  PAT_SOFT_DOGS_T4, RAMPS_ICE_STRAWBERRY, 0
        pat_eramps  PAT_SOFT_DOGS_T1, RAMPS_SKYBLUE_WHITE, 0
        pat_eramps  PAT_SOFT_DOGS_T1, RAMPS_FIRE_ICE, 0
        pat_eramps  PAT_RIM_WTD_SOLID, RAMPS_YELLOW_SKYBLUE, 0
        pat_eramps  PAT_RIM_WTD_SOLID, RAMPS_ICE, 0
        pat_eramps  PAT_RIM_WTD_SOLID, RAMPS_MINT, 0
        pat_eramps  PAT_RIM_WTD_SOLID, RAMPS_BLURPLE, 0
        pat_eramps  PAT_WAVY_SPOKES, RAMPS_FIRE_ICE, 0
        pat_eramps  PAT_WAVY_SPOKES, RAMPS_RED_CERISE, 0
        pat_eramps  PAT_WAVY_SPOKES, RAMPS_BLUE_SKYBLUE, 0
        pat_eramps  PAT_WAVY_SPOKES, RAMPS_SEAGREEN, 0
        pat_eramps  PAT_WAVY_SPOKES, RAMPS_BLURPLE, 0
        pat_eramps  PAT_WAVY_SPOKES, RAMPS_SKYBLUE_WHITE, 0
NUM_PATS_IN_BANK5 equ ($ - PM_PatTable_Bank5 - 1) / 2

PM_PatTable_Bank6:
        dw  NUM_PATS_IN_BANK6
        ; Two colour ramps, Set B
        pat_eramps  PAT_LUM_CRAWL_T2, RAMPS_FIRE_ICE, 0
        pat_eramps  PAT_LUM_CRAWL_T1, RAMPS_BLURPLE, 0
        pat_eramps  PAT_LUM_CRAWL_T1, RAMPS_BLAZOR, 0
        pat_eramps  PAT_RIM_WTD_FLASH_T4, RAMPS_FIRE_ICE, 0
        pat_eramps  PAT_RIM_WTD_FLASH_T3, RAMPS_GOLD_SILVER, 0
        pat_eramps  PAT_RIM_WTD_FLASH_T2, RAMPS_BLURPLE, 0
        pat_eramps  PAT_RIM_WTD_FLASH_T1, RAMPS_ICE, 0
        pat_eramps  PAT_BOOKENDS, RAMPS_FIRE_ICE, 0
        pat_eramps  PAT_BOOKENDS, RAMPS_ICE_STRAWBERRY, 0
        pat_eramps  PAT_CHEVRONS, RAMPS_SKYBLUE_WHITE, 0
        pat_eramps  PAT_CHEVRONS, RAMPS_GOLD_SILVER, 0
        pat_eramps  PAT_CHEVRONS, RAMPS_RED_MIST, 0
        pat_eramps  PAT_CHEVRONS, RAMPS_BUNSEN, 0
        pat_eramps  PAT_CHEVRONS, RAMPS_RED_WHITE, 0
        pat_eramps  PAT_BLUR_STEP, RAMPS_SKYBLUE_WHITE, 0
        pat_eramps  PAT_BLUR_STEP, RAMPS_GOLD_SILVER, 0
        pat_eramps  PAT_INTENSE_DECAY, RAMPS_VIOLET, 0
        pat_eramps  PAT_INTENSE_DECAY, RAMPS_FIRE_ICE, 0
        pat_eramps  PAT_INTENSE_DECAY, RAMPS_BUNSEN, 0
        pat_eramps  PAT_SPOKES2_T2, RAMPS_BLAZOR_SKYBLUE, 0
        pat_eramps  PAT_SPOKES2_T1, RAMPS_YELLOW_SKYBLUE, 0
NUM_PATS_IN_BANK6 equ ($ - PM_PatTable_Bank6 - 1) / 2

PM_PatTable_Bank7:
        dw  NUM_PATS_IN_BANK7
        ; Miscellaneous
        pat_fixed   PAT_BY_YOUR_COMMAND, 0  ; Warns of special banks ahead
        pat_eramps  PAT_AWL, RAMPS_RED, 0
        pat_fixed   PAT_SQUARES_FIXED5, 0
        pat_fixed   PAT_QUANTUM_SLALOM_F6, 0
        pat_fixed   PAT_TERMINALS_FIXED6, 0
        pat_fixed   PAT_TRIANGLES_NEAR_ME, 0
        pat_eramps  PAT_SLIDES, RAMPS_BLAZOR_SKYBLUE, 0
        pat_eramps  PAT_SLIDES, RAMPS_SKYBLUE_GREEN, 0
  if INCLUDE_MORSE
        pat_eramps  PAT_MORSE_POV, RAMPS_GREEN, 0
        pat_eramps  PAT_MORSE_FAST, RAMPS_STRAWBERRY, 0
        pat_eramps  PAT_MORSE_SLOW, RAMPS_FIRE, 0
  endif
NUM_PATS_IN_BANK7 equ ($ - PM_PatTable_Bank7 - 1) / 2

PM_PatTable_Bank8:
        dw  NUM_PATS_IN_BANK8
        ; User-settable colour - One colour/ramp
        pat_uramps  PAT_SOLID, 0  ; Convenience
        pat_uramps  PAT_ZIGZAG_T4, 0
        pat_uramps  PAT_ZIGZAG_T1, 0
        pat_uramps  PAT_STEP_T5, 0
        pat_uramps  PAT_STEP_T4, 0
        pat_uramps  PAT_STEP_T3, 0
        pat_uramps  PAT_STEP_T2, 0
        pat_uramps  PAT_STEP_T1, 0
        pat_uramps  PAT_CHEQUER_K_T6, 0
        pat_uramps  PAT_CHEQUER_K_T3, 0
        pat_uramps  PAT_CHEQUER_W_T6, 0
        pat_uramps  PAT_CHEQUER_W_T3, 0
        pat_uramps  PAT_SPOKES_T4, 0
        pat_uramps  PAT_SPOKES_T3, 0
        pat_uramps  PAT_SPOKES_T2, 0
        pat_uramps  PAT_SPOKES_T1, 0
        pat_uramps  PAT_AWL, 0
NUM_PATS_IN_BANK8 equ ($ - PM_PatTable_Bank8 - 1) / 2

PM_PatTable_Bank9:
        dw  NUM_PATS_IN_BANK9
        ; User-settable colour - Two colours/ramps
        pat_uramps  PAT_FLASH_T4, 0
        pat_uramps  PAT_FLASH_T3, 0
        pat_uramps  PAT_FLASH_T2, 0
        pat_uramps  PAT_FLASH_T1, 0
        pat_uramps  PAT_ALTERNATE_T12, 0
        pat_uramps  PAT_ALTERNATE_T5, 0
        pat_uramps  PAT_ALTERNATE_T3, 0
        pat_uramps  PAT_SQUARES_USER2, 0
        pat_uramps  PAT_QUANTUM_SLALOM_U2, 0
        pat_uramps  PAT_TERMINALS_USER2, 0
        pat_uramps  PAT_DOGS_T4, 0
        pat_uramps  PAT_DOGS_T1, 0
        pat_uramps  PAT_RIM_WTD_FLASH_T4, 0
        pat_uramps  PAT_RIM_WTD_FLASH_T3, 0
        pat_uramps  PAT_RIM_WTD_FLASH_T2, 0
        pat_uramps  PAT_RIM_WTD_FLASH_T1, 0
        pat_uramps  PAT_TNM_USER2, 0
        pat_uramps  PAT_CONSUMMATE_VS_T4, 0
        pat_uramps  PAT_CONSUMMATE_VS_T1, 0
        pat_uramps  PAT_WEAVE_T2, 0
        pat_uramps  PAT_WEAVE_T1, 0
        pat_uramps  PAT_SOFT_FLASH_T2, 0
        pat_uramps  PAT_SOFT_FLASH_T1, 0
        pat_uramps  PAT_SOFT_DOGS_T4, 0
        pat_uramps  PAT_SOFT_DOGS_T1, 0
        pat_uramps  PAT_RIM_WTD_SOLID, 0
        pat_uramps  PAT_WAVY_SPOKES, 0
        pat_uramps  PAT_LUM_CRAWL_T2, 0
        pat_uramps  PAT_LUM_CRAWL_T1, 0
        pat_uramps  PAT_BOOKENDS, 0
        pat_uramps  PAT_CHEVRONS, 0
        pat_uramps  PAT_BLUR_STEP, 0
        pat_uramps  PAT_INTENSE_DECAY, 0
        pat_uramps  PAT_SLIDES, 0
        pat_uramps  PAT_SPOKES2_T2, 0
        pat_uramps  PAT_SPOKES2_T1, 0
NUM_PATS_IN_BANK9 equ ($ - PM_PatTable_Bank9 - 1) / 2


;------------------------------------------------------------------------------


PM_BankTable:
        dw  NUM_BANKS
        dw  PM_PatTable_Bank0  ; Rainbows and solid colours
        dw  PM_PatTable_Bank1  ; Tricolours - Concentric
        dw  PM_PatTable_Bank2  ; Tricolours - Chasers and radial bars
        dw  PM_PatTable_Bank3  ; One colour
        dw  PM_PatTable_Bank4  ; Two colours
        dw  PM_PatTable_Bank5  ; Two colour ramps, Set A
        dw  PM_PatTable_Bank6  ; Two colour ramps, Set B
        dw  PM_PatTable_Bank7  ; Miscellaneous
FIRST_USER_RAMPS_BANK_IX equ ($ - PM_BankTable - 1)
        dw  PM_PatTable_Bank8  ; User-settable colour - One colour/ramp
        dw  PM_PatTable_Bank9  ; User-settable colour - Two colours/ramps
NUM_BANKS equ ($ - PM_BankTable - 1)


;------------------------------------------------------------------------------

Pages1to3CodeEnd:
Pages1to3WordsUsed = Pages1to3CodeEnd - 0x0800
Pages1to3WordsFree = 0x2000 - Pages1to3CodeEnd
 if Pages1to3WordsFree < 0x0800
   error "Less than 512 words free!"
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

; Current:      New: Fav.R.   Bank P.
;   rrrrmmmm       CCCCmmmm    pppppppp       AAAAAp pppppppp
;   rrpppppp       BApppppp    p  AAAAA       BBBBBe CCCCtttt
;                  BBBBAAAA       BBBBB
;
;                  13
;   PATIX:9, RA:5,RB:5,BGIx:2, USER:1
;
; TO DO: Implement the rest of the two 5-bit ramp system.
;
; Centre: W,Y,P,G,M,S

