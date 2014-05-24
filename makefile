
.PHONY: all test clean

DFLAGS = -w -g -unittest
DC = dmd

all: clean
	$(DC) -g -w \
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

backtrace.o: backtrace.d
	$(DC) -c $(DFLAGS) backtrace.d -ofbacktrace.o -H -Hd.

dlang_helper.o: dlang_helper.d
	$(DC) -c $(DFLAGS) dlang_helper.d -ofdlang_helper.o -H -Hd.

std/allocator.o: std/allocator.d
	$(DC) -c $(DFLAGS) std/allocator.d -ofstd/allocator.o -H -Hdstd/

std/lexer.o: std/lexer.d
	$(DC) -c $(DFLAGS) std/lexer.d -ofstd/lexer.o -H -Hdstd/

std/d/codegen.o: std/d/codegen.d
	$(DC) -c $(DFLAGS) std/d/codegen.d -ofstd/d/codegen.o -H -Hdstd/d/

std/d/ast.o: std/d/ast.d
	$(DC) -c $(DFLAGS) std/d/ast.d -ofstd/d/ast.o -H -Hdstd/d/ -Istd/d/

std/d/lexer.o: std/d/lexer.d
	$(DC) -c $(DFLAGS) std/d/lexer.d -ofstd/d/lexer.o -H -Hdstd/d/

std/d/parser.o: std/d/parser.d
	$(DC) -c $(DFLAGS) std/d/parser.d -ofstd/d/parser.o -H -Hdstd/d/

std/d/inspect.o: std/d/inspect.d
	$(DC) -c $(DFLAGS) std/d/inspect.d -ofstd/d/inspect.o -H -Hdstd/d/

analysis/base.o: analysis/base.d
	$(DC) -c $(DFLAGS) analysis/base.d -ofanalysis/base.o -H -Hdanalysis/

analysis/run.o: analysis/run.d
	$(DC) -c $(DFLAGS) analysis/run.d -ofanalysis/run.o -H -Hdanalysis/

analysis/helpers.o: analysis/helpers.d
	$(DC) -c $(DFLAGS) analysis/helpers.d -ofanalysis/helpers.o -H -Hdanalysis/ -Ianalysis/ -Istd/d/ -I.

analysis/scope_frame.o: analysis/scope_frame.d
	$(DC) -c $(DFLAGS) analysis/scope_frame.d -ofanalysis/scope_frame.o -H -Hdanalysis/

analysis/scope_analyzer.o: analysis/scope_analyzer.d
	$(DC) -c $(DFLAGS) analysis/scope_analyzer.d -ofanalysis/scope_analyzer.o -H -Hdanalysis/

analysis/check_compare.o: analysis/check_compare.d
	$(DC) -c $(DFLAGS) analysis/check_compare.d -ofanalysis/check_compare.o -H -Hdanalysis/

analysis/check_string_format.o: analysis/check_string_format.d
	$(DC) -c $(DFLAGS) analysis/check_string_format.d -ofanalysis/check_string_format.o -H -Hdanalysis/

analysis/check_name_clash.o: analysis/check_name_clash.d
	$(DC) -c $(DFLAGS) analysis/check_name_clash.d -ofanalysis/check_name_clash.o -H -Hdanalysis/

analysis/check_unused.o: analysis/check_unused.d
	$(DC) -c $(DFLAGS) analysis/check_unused.d -ofanalysis/check_unused.o -H -Hdanalysis/

analysis/check_size_t.o: analysis/check_size_t.d
	$(DC) -c $(DFLAGS) analysis/check_size_t.d -ofanalysis/check_size_t.o -H -Hdanalysis/

analysis/constructors.o: analysis/constructors.d
	$(DC) -c $(DFLAGS) analysis/constructors.d -ofanalysis/constructors.o -H -Hdanalysis/

analysis/del.o: analysis/del.d
	$(DC) -c $(DFLAGS) analysis/del.d -ofanalysis/del.o -H -Hdanalysis/

analysis/enumarrayliteral.o: analysis/enumarrayliteral.d
	$(DC) -c $(DFLAGS) analysis/enumarrayliteral.d -ofanalysis/enumarrayliteral.o -H -Hdanalysis/

analysis/fish.o: analysis/fish.d
	$(DC) -c $(DFLAGS) analysis/fish.d -ofanalysis/fish.o -H -Hdanalysis/

analysis/ifelsesame.o: analysis/ifelsesame.d
	$(DC) -c $(DFLAGS) analysis/ifelsesame.d -ofanalysis/ifelsesame.o -H -Hdanalysis/

analysis/numbers.o: analysis/numbers.d
	$(DC) -c $(DFLAGS) analysis/numbers.d -ofanalysis/numbers.o -H -Hdanalysis/

analysis/objectconst.o: analysis/objectconst.d
	$(DC) -c $(DFLAGS) analysis/objectconst.d -ofanalysis/objectconst.o -H -Hdanalysis/

analysis/package.o: analysis/package.d
	$(DC) -c $(DFLAGS) analysis/package.d -ofanalysis/package.o -H -Hdanalysis/

analysis/pokemon.o: analysis/pokemon.d
	$(DC) -c $(DFLAGS) analysis/pokemon.d -ofanalysis/pokemon.o -H -Hdanalysis/

analysis/range.o: analysis/range.d
	$(DC) -c $(DFLAGS) analysis/range.d -ofanalysis/range.o -H -Hdanalysis/

analysis/style.o: analysis/style.d
	$(DC) -c $(DFLAGS) analysis/style.d -ofanalysis/style.o -H -Hdanalysis/

analysis/unused.o: analysis/unused.d
	$(DC) -c $(DFLAGS) analysis/unused.d -ofanalysis/unused.o -H -Hdanalysis/

stats.o: stats.d
	$(DC) -c $(DFLAGS) stats.d -ofstats.o -H -Hd.

imports.o: imports.d
	$(DC) -c $(DFLAGS) imports.d -ofimports.o -H -Hd.

highlighter.o: highlighter.d
	$(DC) -c $(DFLAGS) highlighter.d -ofhighlighter.o -H -Hd.

ctags.o: ctags.d
	$(DC) -c $(DFLAGS) ctags.d -ofctags.o -H -Hd.

astprinter.o: astprinter.d
	$(DC) -c $(DFLAGS) astprinter.d -ofastprinter.o -H -Hd.

formatter.o: formatter.d
	$(DC) -c $(DFLAGS) formatter.d -offormatter.o -H -Hd.

outliner.o: outliner.d
	$(DC) -c $(DFLAGS) outliner.d -ofoutliner.o -H -Hd.

main.o: main.d
	$(DC) -c $(DFLAGS) main.d -ofmain.o -H -Hd. -I.

testSuite: backtrace.o \
dlang_helper.o \
dlang_helper.o \
std/allocator.o \
std/lexer.o \
std/d/codegen.o \
std/d/ast.o \
std/d/lexer.o \
std/d/parser.o \
std/d/inspect.o \
analysis/base.o \
analysis/run.o \
analysis/helpers.o \
analysis/scope_frame.o \
analysis/scope_analyzer.o \
analysis/check_compare.o \
analysis/check_string_format.o \
analysis/check_name_clash.o \
analysis/check_unused.o \
analysis/check_size_t.o \
analysis/constructors.o \
analysis/del.o \
analysis/enumarrayliteral.o \
analysis/fish.o \
analysis/ifelsesame.o \
analysis/numbers.o \
analysis/objectconst.o \
analysis/package.o \
analysis/pokemon.o \
analysis/range.o \
analysis/style.o \
analysis/unused.o \
stats.o \
imports.o \
highlighter.o \
ctags.o \
astprinter.o \
formatter.o \
outliner.o \
main.o
	$(DC) $(DFLAGS) -oftestSuite \
	std/*.o \
	std/d/*.o \
	analysis/*.o \
	*.o

test: testSuite
	./testSuite

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
	rm -f testSuite




