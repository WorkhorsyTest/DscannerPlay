
.PHONY: all test clean

all:
	rm -f *.o
	rm -f dscanner
	dmd -g -w \
	-ofdscanner \
	backtrace.d \
	dlang_helper.d \
	stats.d \
	imports.d \
	highlighter.d \
	ctags.d \
	astprinter.d \
	formatter.d \
	outliner.d \
	std/*.d \
	std/d/*.d \
	analysis/*.d \
	main.d

fuck: clean
	dmd -c -g -w -unittest backtrace.d -Hfbacktrace.di
	dmd -c -g -w -unittest dlang_helper.d -Hfdlang_helper.di

	dmd -c -g -w -unittest std/allocator.d -Hfstd/allocator.di
	dmd -c -g -w -unittest std/lexer.d -Hfstd/lexer.di
	dmd -c -g -w -unittest std/d/codegen.d -Hfstd/d/codegen.di
	dmd -c -g -w -unittest std/d/ast.d std/d/codegen.di -Hfstd/d/ast.di
	dmd -c -g -w -unittest std/d/lexer.d -Hfstd/d/lexer.di
	dmd -c -g -w -unittest std/d/parser.d -Hfstd/d/parser.di

	dmd -c -g -w -unittest stats.d -Hfstats.di

	dmd -c -g -w -unittest imports.d -Hfimports.di
	dmd -c -g -w -unittest highlighter.d -Hfhighlighter.di
	dmd -c -g -w -unittest ctags.d -Hfctags.di
	dmd -c -g -w -unittest astprinter.d -Hfastprinter.di
	dmd -c -g -w -unittest formatter.d -Hfformatter.di
	dmd -c -g -w -unittest outliner.d -Hfoutliner.di
	#dmd -c -g -w -unittest main.d backtrace.di -Hfmain.di


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
	rm -f *.di

	rm -f std/*.o
	rm -f std/*.di

	rm -f std/d/*.o
	rm -f std/d/*.di

	rm -f analysis/*.o
	rm -f analysis/*.di

	rm -f dscanner
	rm -f test




