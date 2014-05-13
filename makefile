
.PHONY: all test clean

all:
	rm -f *.o
	rm -f dscanner
	dmd -g -w \
	-ofdscanner \
	backtrace.d \
	dlang_helper.d \
	main.d \
	stats.d \
	imports.d \
	highlighter.d \
	ctags.d \
	astprinter.d \
	formatter.d \
	outliner.d \
	std/*.d \
	std/d/*.d \
	analysis/*.d

test:
	rm -f *.o
	rm -f test
	dmd -g -w -unittest \
	-oftest \
	backtrace.d \
	dlang_helper.d \
	main.d \
	stats.d \
	imports.d \
	highlighter.d \
	ctags.d \
	astprinter.d \
	formatter.d \
	outliner.d \
	std/*.d \
	std/d/*.d \
	analysis/*.d

	./test

clean:
	rm -f *.o
	rm -f dscanner
	rm -f test




