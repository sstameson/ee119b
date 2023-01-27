default:
	python3 asm.py > avr_test.asm
	vim -c "syntax off" -c "hardcopy > avr_test.ps" -c "q" avr_test.asm
	gs -dQUIET -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -sOutputFile=hw2.pdf \
		avr_test.ps

clean:
	rm -f *.zip *.ps *.pdf *.cf *.ghw
