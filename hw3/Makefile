default:
	ghdl -a --std=08 alu.vhd
	ghdl -a --std=08 alu_tb.vhd
	ghdl -e --std=08 ALU_TB
	ghdl -r --std=08 ALU_TB --stop-time=3000ns --wave=wave.ghw

clean:
	rm -f *.zip *.ps *.pdf *.cf *.ghw

.PHONY: clean default
