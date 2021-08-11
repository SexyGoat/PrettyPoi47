# Makefile for PrettyPoi47 (prettypoi.asm)

# (c) Copyright 2021, Daniel Neville

# This file is part of PrettyPoi47.
#
# PrettyPoi47 is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# PrettyPoi47 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with PrettyPoi47. If not, see <https://www.gnu.org/licenses/>.


.PHONY = help clean all burn read showeep


# The main output files, aside from their extensions, have a common filename.
MAIN_STEM := prettypoi

# Files to "burn" into MCU memories
HEX_FILE:= ${MAIN_STEM}.hex
DUMP_FILE := dump.hex

# Assembler
# gpasm is in the Ubuntu gputils package.
ASM := gpasm
# Programmer
# pk2cmd is available from Microchip
PROG := pk2cmd

help:
	@echo "Custom code for 3-RGBLED Ninja poi from Home of Poi"
	@echo "Target MCU: PIC16F1847"
	@echo "make help:        Display this message."
	@echo "make clean:       Remove generated files such as object files."
	@echo "make all:         Build ${HEX_FILE} for the PIC16F1847."
	@echo "make burn:        Program the PIC16F1847 MCU with ${PROG}."
	@echo "make read:        Read the PIC16F1847 and save to ${DUMP_FILE}."
	@echo "make showeep:     Display the PIC16F1847's EEPROM."

clean:
	rm -f ${MAIN_STEM}.o
	rm -f ${MAIN_STEM}.elf
	rm -f ${MAIN_STEM}.cod
	rm -f ${MAIN_STEM}.lst
	rm -f ${HEX_FILE}
	rm -f ${DUMP_FILE}

all: ${HEX_FILE}

${HEX_FILE}: ${MAIN_STEM}.asm $(wildcard *.inc) $(wildcard *.h)
	${ASM} -o ${HEX_FILE} ${MAIN_STEM}.asm
	@echo "Hex file" '"'"${HEX_FILE}"'"' "is ready to burn"\
		"(with" '"make burn"'")."

burn: ${HEX_FILE}
	${PROG} -PPIC16F1847 -M -F${CURDIR}/${HEX_FILE}

read:
	${PROG} -PPIC16F1847 -GF${CURDIR}/${DUMP_FILE}

showeep:
	${PROG} -PPIC16F1847 -GE0-FF
