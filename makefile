
.PHONY: all test clean
DC = dmd

all: DFLAGS = -w -g -O -release
all: OUTFILE = dscanner

test: DFLAGS = -w -g -unittest
test: OUTFILE = testSuite

all: actualBuild
	@echo "Build complete"

test: actualBuild
	@echo "Running tests ..."
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

actualBuild: ${PREFIX}backtrace.o \
${PREFIX}dlang_helper.o \
${PREFIX}dlang_helper.o \
std/${PREFIX}allocator.o \
std/${PREFIX}lexer.o \
std/d/${PREFIX}codegen.o \
std/d/${PREFIX}ast.o \
std/d/${PREFIX}lexer.o \
std/d/${PREFIX}parser.o \
std/d/${PREFIX}inspect.o \
analysis/${PREFIX}base.o \
analysis/${PREFIX}run.o \
analysis/${PREFIX}helpers.o \
analysis/${PREFIX}scope_frame.o \
analysis/${PREFIX}scope_analyzer.o \
analysis/${PREFIX}check_compare.o \
analysis/${PREFIX}check_string_format.o \
analysis/${PREFIX}check_name_clash.o \
analysis/${PREFIX}check_unused.o \
analysis/${PREFIX}check_size_t.o \
analysis/${PREFIX}constructors.o \
analysis/${PREFIX}del.o \
analysis/${PREFIX}enumarrayliteral.o \
analysis/${PREFIX}fish.o \
analysis/${PREFIX}ifelsesame.o \
analysis/${PREFIX}numbers.o \
analysis/${PREFIX}objectconst.o \
analysis/${PREFIX}package.o \
analysis/${PREFIX}pokemon.o \
analysis/${PREFIX}range.o \
analysis/${PREFIX}style.o \
analysis/${PREFIX}unused.o \
analysis/${PREFIX}duplicate_attribute.o \
${PREFIX}stats.o \
${PREFIX}imports.o \
${PREFIX}highlighter.o \
${PREFIX}ctags.o \
${PREFIX}astprinter.o \
${PREFIX}formatter.o \
${PREFIX}outliner.o \
${PREFIX}main.o
	${DC} ${DFLAGS} -of${OUTFILE} \
	std/${PREFIX}*.o \
	std/d/${PREFIX}*.o \
	analysis/${PREFIX}*.o \
	${PREFIX}*.o

${PREFIX}backtrace.o: backtrace.d
	${DC} -c ${DFLAGS} backtrace.d -of${PREFIX}backtrace.o -H -Hd.

${PREFIX}dlang_helper.o: dlang_helper.d
	${DC} -c ${DFLAGS} dlang_helper.d -of${PREFIX}dlang_helper.o -H -Hd.

std/${PREFIX}allocator.o: std/allocator.d
	${DC} -c ${DFLAGS} std/allocator.d -ofstd/${PREFIX}allocator.o -H -Hdstd/

std/${PREFIX}lexer.o: std/lexer.d
	${DC} -c ${DFLAGS} std/lexer.d -ofstd/${PREFIX}lexer.o -H -Hdstd/

std/d/${PREFIX}codegen.o: std/d/codegen.d
	${DC} -c ${DFLAGS} std/d/codegen.d -ofstd/d/${PREFIX}codegen.o -H -Hdstd/d/

std/d/${PREFIX}ast.o: std/d/ast.d
	${DC} -c ${DFLAGS} std/d/ast.d -ofstd/d/${PREFIX}ast.o -H -Hdstd/d/ -Istd/d/

std/d/${PREFIX}lexer.o: std/d/lexer.d
	${DC} -c ${DFLAGS} std/d/lexer.d -ofstd/d/${PREFIX}lexer.o -H -Hdstd/d/

std/d/${PREFIX}parser.o: std/d/parser.d
	${DC} -c ${DFLAGS} std/d/parser.d -ofstd/d/${PREFIX}parser.o -H -Hdstd/d/

std/d/${PREFIX}inspect.o: std/d/inspect.d
	${DC} -c ${DFLAGS} std/d/inspect.d -ofstd/d/${PREFIX}inspect.o -H -Hdstd/d/

analysis/${PREFIX}base.o: analysis/base.d
	${DC} -c ${DFLAGS} analysis/base.d -ofanalysis/${PREFIX}base.o -H -Hdanalysis/

analysis/${PREFIX}run.o: analysis/run.d
	${DC} -c ${DFLAGS} analysis/run.d -ofanalysis/${PREFIX}run.o -H -Hdanalysis/

analysis/${PREFIX}helpers.o: analysis/helpers.d
	${DC} -c ${DFLAGS} analysis/helpers.d -ofanalysis/${PREFIX}helpers.o -H -Hdanalysis/ -Ianalysis/ -Istd/d/ -I.

analysis/${PREFIX}scope_frame.o: analysis/scope_frame.d
	${DC} -c ${DFLAGS} analysis/scope_frame.d -ofanalysis/${PREFIX}scope_frame.o -H -Hdanalysis/

analysis/${PREFIX}scope_analyzer.o: analysis/scope_analyzer.d
	${DC} -c ${DFLAGS} analysis/scope_analyzer.d -ofanalysis/${PREFIX}scope_analyzer.o -H -Hdanalysis/

analysis/${PREFIX}check_compare.o: analysis/check_compare.d
	${DC} -c ${DFLAGS} analysis/check_compare.d -ofanalysis/${PREFIX}check_compare.o -H -Hdanalysis/

analysis/${PREFIX}check_string_format.o: analysis/check_string_format.d
	${DC} -c ${DFLAGS} analysis/check_string_format.d -ofanalysis/${PREFIX}check_string_format.o -H -Hdanalysis/

analysis/${PREFIX}check_name_clash.o: analysis/check_name_clash.d
	${DC} -c ${DFLAGS} analysis/check_name_clash.d -ofanalysis/${PREFIX}check_name_clash.o -H -Hdanalysis/

analysis/${PREFIX}check_unused.o: analysis/check_unused.d
	${DC} -c ${DFLAGS} analysis/check_unused.d -ofanalysis/${PREFIX}check_unused.o -H -Hdanalysis/

analysis/${PREFIX}check_size_t.o: analysis/check_size_t.d
	${DC} -c ${DFLAGS} analysis/check_size_t.d -ofanalysis/${PREFIX}check_size_t.o -H -Hdanalysis/

analysis/${PREFIX}constructors.o: analysis/constructors.d
	${DC} -c ${DFLAGS} analysis/constructors.d -ofanalysis/${PREFIX}constructors.o -H -Hdanalysis/

analysis/${PREFIX}del.o: analysis/del.d
	${DC} -c ${DFLAGS} analysis/del.d -ofanalysis/${PREFIX}del.o -H -Hdanalysis/

analysis/${PREFIX}enumarrayliteral.o: analysis/enumarrayliteral.d
	${DC} -c ${DFLAGS} analysis/enumarrayliteral.d -ofanalysis/${PREFIX}enumarrayliteral.o -H -Hdanalysis/

analysis/${PREFIX}fish.o: analysis/fish.d
	${DC} -c ${DFLAGS} analysis/fish.d -ofanalysis/${PREFIX}fish.o -H -Hdanalysis/

analysis/${PREFIX}ifelsesame.o: analysis/ifelsesame.d
	${DC} -c ${DFLAGS} analysis/ifelsesame.d -ofanalysis/${PREFIX}ifelsesame.o -H -Hdanalysis/

analysis/${PREFIX}numbers.o: analysis/numbers.d
	${DC} -c ${DFLAGS} analysis/numbers.d -ofanalysis/${PREFIX}numbers.o -H -Hdanalysis/

analysis/${PREFIX}objectconst.o: analysis/objectconst.d
	${DC} -c ${DFLAGS} analysis/objectconst.d -ofanalysis/${PREFIX}objectconst.o -H -Hdanalysis/

analysis/${PREFIX}package.o: analysis/package.d
	${DC} -c ${DFLAGS} analysis/package.d -ofanalysis/${PREFIX}package.o -H -Hdanalysis/

analysis/${PREFIX}pokemon.o: analysis/pokemon.d
	${DC} -c ${DFLAGS} analysis/pokemon.d -ofanalysis/${PREFIX}pokemon.o -H -Hdanalysis/

analysis/${PREFIX}range.o: analysis/range.d
	${DC} -c ${DFLAGS} analysis/range.d -ofanalysis/${PREFIX}range.o -H -Hdanalysis/

analysis/${PREFIX}style.o: analysis/style.d
	${DC} -c ${DFLAGS} analysis/style.d -ofanalysis/${PREFIX}style.o -H -Hdanalysis/

analysis/${PREFIX}unused.o: analysis/unused.d
	${DC} -c ${DFLAGS} analysis/unused.d -ofanalysis/${PREFIX}unused.o -H -Hdanalysis/

analysis/${PREFIX}duplicate_attribute.o: analysis/duplicate_attribute.d
	${DC} -c ${DFLAGS} analysis/duplicate_attribute.d -ofanalysis/${PREFIX}duplicate_attribute.o -H -Hdanalysis/

${PREFIX}stats.o: stats.d
	${DC} -c ${DFLAGS} stats.d -of${PREFIX}stats.o -H -Hd.

${PREFIX}imports.o: imports.d
	${DC} -c ${DFLAGS} imports.d -of${PREFIX}imports.o -H -Hd.

${PREFIX}highlighter.o: highlighter.d
	${DC} -c ${DFLAGS} highlighter.d -of${PREFIX}highlighter.o -H -Hd.

${PREFIX}ctags.o: ctags.d
	${DC} -c ${DFLAGS} ctags.d -of${PREFIX}ctags.o -H -Hd.

${PREFIX}astprinter.o: astprinter.d
	${DC} -c ${DFLAGS} astprinter.d -of${PREFIX}astprinter.o -H -Hd.

${PREFIX}formatter.o: formatter.d
	${DC} -c ${DFLAGS} formatter.d -of${PREFIX}formatter.o -H -Hd.

${PREFIX}outliner.o: outliner.d
	${DC} -c ${DFLAGS} outliner.d -of${PREFIX}outliner.o -H -Hd.

${PREFIX}main.o: main.d
	${DC} -c ${DFLAGS} main.d -of${PREFIX}main.o -H -Hd. -I.






