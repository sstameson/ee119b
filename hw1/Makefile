default:
	ghdl -a --std=93 CORDICCalc.vhd
	ghdl -e CORDICCalc_TB
	ghdl -r CORDICCalc_TB --stop-time=3000ns --wave=wave.ghw

clean:
	rm -f *.zip *.ps *.pdf *.cf *.ghw
