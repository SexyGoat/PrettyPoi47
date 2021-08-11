PrettyPoi47 Firmware for a Ninja 3-RGBLED Stick Poi sold by Home of Poi
-------------------------------------------------------------------------------


(c) Copyright 2021, Daniel Neville, creamygoat@gmail.com


This file is part of PrettyPoi47.

PrettyPoi47 is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

PrettyPoi47 is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with PrettyPoi47.


-------------------------------------------------------------------------------
Building the Hex Files
-------------------------------------------------------------------------------


Assembling the firmware source code into an Intel Hex file requires gpasm,
a macro assembler, and the header files for Microchip's PIC series of chips.

Both gpasm and the header files are in the Ubuntu "gputils" package.
(You may need to install "build-essential" and "libusb++-dev".)

The makefile for ShinyPoi is set up to assemble for the PIC16F1847 chip:

  make shiny47.hex
  make all

You may want to inspect the pin assignment in shinypoi.asm is correct for
the poi you have.


-------------------------------------------------------------------------------
Programming the MCU
-------------------------------------------------------------------------------


Burning the hex file to the MCU requires pk2cmd, available from Microchip.

Build and install pk2cmd from source. Add the appropriate entry to the PATH
environment variable so that pk2cmd can be executed from anywhere. The pk2cmd
command is probably located in /usr/local/bin and owned by root but with the
SUID bit set. The list of supported devices, horribly out of date, is an
especially horrible executable file /usr/share/pk2/PK2DeviceFile.dat, owned
by root.

The pk2cmd can be tested by using the autodetect feature:

  pk2cmd -p

You may need to add yourself to the "dialout" group so you can access the
PicKit2 USB programmer.

To program the PIC16F1827 or the PIC16F1847 chip, issue the appropriate
makefile command:

  make burn27

      or

  make burn47

An ugly quirk of pk2cmd is its requirement for an absolute pathname for
a hex file. The makefile uses the ${CURDIR} construct to deal with that.
Another ugly quirk is the requirement for no spacing between a option
indicator and the option value. Some extra twiddling may be necessary on
a non Unix-like OS.


-------------------------------------------------------------------------------
Electrical Connections
-------------------------------------------------------------------------------


The Ninja poi has vias on its PCB for In-System Programming (or In-Circuit
Serial Programming as Microchip calls it) by means of a suitable programmer
such as Microchip's PicKit2 (or a clone).

Five probes will be needed to connect the vias to the programmer. A suitable
probe can be constructed by soldering one end of a solderless breadboard
jumper wire to the socket of a wire-wrap post taken from from a wire-wrap pin
socket strip, then gently sandpapering the square edges to a taper. A bit of
heat-shrink insulation won't go amiss.

The other ends of the jumper wires _could_ go into the PicKit2, but they
would probably be a bit loose. There is always the risk of mis-wiring or
even shorting while faffing about with the connections during programming
and testing. It's far better to stick those jumper wire ends into a neat
row (parallel to the central groove) of a wee 170 tie point solderless
breadboard. (Remember to leave room for the the unused "auxiliary" pin
from the PicKit2.) Poke the double-ended 6-pin header in another breadboard
row sharing the same half-columns and attach the 6-wire cable to the PicKit2.
The breadboard easily grips both pin headers and the usual 22 AWG tinned
breadboard jumper wires and serves as a sort of anchor and strain relief to
keep wires from flying.

It is a good idea to remove the battery when programming the MCU on the poi.


-------------------------------------------------------------------------------
Wiring for the Old Series poi (with the PIC16F1827 MCU)
;
; This diagram is only applicable in the case of a PIC16F1847 installed where
; a PIC16F1827 would normally be.
;
-------------------------------------------------------------------------------



                                               Old Series Ninja poi
                                               Buttons and MCU side
                                      +------------------------------------ -
              Microchip               |           D1  D3   (o)    C1   C2
           PIC16F1827-I/SS            |                       (o)
                 MCU                  |               (2) (5)      +------+
                                      |              __________    |      |
         +----------------+           | B1   B2     [____MCU___](4)|[LED3]|
 LATCH  1|RA2          RA1|20 SENSE   |                            |      |
    B3  2|RA3          RA0|19 PATTERN |            (1) (o)         +------+
    R3  3|RA4          RA7|18 MODE    |              (3) (o)   C3
        4|~MCLR/RA5    RA6|17         |         (o)
        5|VSS          VDD|16         +------------------------------------ -
        6|VSS          VDD|15             (Vias are numbered for PicKit2)
    G3  7|RB0  ICSPDAT/RB7|14
    B2  8|RB1  ICSPCLK/RB6|13 B1             PICKit2 programmer
    R2  9|RB2          RB5|12 R1      -----------+  ____
    G2 10|RB3          RB4|11 G1                >|1 MCLR (12V)
         +----------------+            PICKit2   |2 VDD = +5V
                                      programmer |3 VSS = GND
                                        from     |4 ICSPDAT
                                      Microchip  |5 ICSPCLK
                                      (or clone) |6 AUX (Not used)
                                      -----------+


;------------------------------------------------------------------------------
; Wiring for the New Series poi (with the PIC16F1847 MCU)
;
; This is the usual target for PrettyPoi47.
;
;------------------------------------------------------------------------------


                                                 Ninja poi NS-2 1.2 PCB
                                                     Component side
            PIC16F1847-I/SS                 +----------------------------- -
                  MCU                       |(o)VSS  (o)VDD=+5V
          +----------------+                |  =GND          ____ +------+
  LATCH  1|RA2          RA1|20 SENSE        |       [MCU] (o)MCLR |[LED3]|
     B1  2|RA3          RA0|19 PATTERN      |                     +------+
     R1  3|RA4          RA7|18 MODE         |     ICSPDAT(o)  ICSPCLK(o)
         4|~MCLR/RA5    RA6|17              +----------------------------- -
         5|VSS          VDD|16
         6|VSS          VDD|15                     PICKit2 programmer
     G1  7|RB0  ICSPDAT/RB7|14              -----------+  ____
     B2  8|RB1  ICSPCLK/RB6|13 B3                     >|1 MCLR (12V)
     R2  9|RB2          RB5|12 R3            PICKit2   |2 VDD = +5V
     G2 10|RB3          RB4|11 G3           programmer |3 VSS = GND
          +----------------+                  from     |4 ICSPDAT
                                            Microchip  |5 ICSPCLK
                                                       |6 AUX (Not used)
                                            -----------+

