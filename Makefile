all:
	ca65 *.asm -o cart.o -g
	ld65 -C nes.cfg cart.o -o cart.nes -Ln cart.lbl
	node generateDebugLabels.js

clean:
	rm *.o *.nes *.fdb

run:
	fceux cart.nes