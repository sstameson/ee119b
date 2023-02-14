default:
	ghdl -a --std=08 alu.vhd
	ghdl -a --std=08 reg.vhd
	ghdl -a --std=08 mau93.vhd
	ghdl -a --std=08 opcodes.vhd
	ghdl -a --std=08 avrcpue.vhd
	ghdl -a --std=08 avr_tb.vhd
	ghdl -e --std=08 AVR_CPU_TB
	ghdl -r --std=08 AVR_CPU_TB --stop-time=10000ns --wave=wave.ghw

lst2test:
	echo hello

clean:
	rm -f *.zip *.ps *.pdf *.cf *.ghw lst2test
