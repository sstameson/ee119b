default:
	ghdl -a --std=93 CORDICCalc.vhd
	ghdl -a --std=93 CORDICCalc_TB.vhd
	ghdl -e CORDICCalc_TB
	ghdl -r CORDICCalc_TB --stop-time=3000ns --wave=wave.ghw

report:
	vim -c "syntax off" -c "hardcopy > CORDICCalc.ps" -c q CORDICCalc.vhd
	vim -c "syntax off" -c "hardcopy > CORDICCalc_TB.ps" -c q CORDICCalc_TB.vhd
	vim -c "syntax off" -c "hardcopy > report.ps" -c q report.md
	gs -dQUIET -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -sOutputFile=hw0.pdf \
		CORDICCalc.ps report.ps CORDICCalc_TB.ps

zip:
	zip -r hw0.zip CORDICCalc.vhd CORDICCalc_TB.vhd CORDICCalc.syr CORDICCalc.twr report.md

clean:
	rm -f *.zip *.ps *.pdf *.cf *.ghw
