DIR := /mnt/c/Users/Spiro
LIB := ./OSVVM/VHDL_LIBS/GHDL-2.0.0

default: OSVVM
	ghdl -a --std=08 alu.vhd
	ghdl -a --std=08 -P$(LIB) alu_tb.vhd
	ghdl -e --std=08 -P$(LIB) ALU_TB
	ghdl -r --std=08 -P$(LIB) ALU_TB --wave=wave.ghw

OSVVM:
	mkdir OSVVM && cd OSVVM && printf "%s\n%s\n" \
		"source $(DIR)/OsvvmLibraries/Scripts/StartUp.tcl" \
		"build  $(DIR)/OsvvmLibraries/" | tclsh

clean:
	rm -f *.zip *.ps *.pdf *.cf *.ghw

.PHONY: clean default
