default:
	python3 asm.py > avr_test1.asm
	vim -c "syntax off" -c "hardcopy > avr_test1.ps" -c "q" avr_test1.asm
	gs -dQUIET -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -sOutputFile=hw2.pdf \
		avr_test1.ps

clean:
	rm -f *.zip *.ps *.pdf *.cf *.ghw
