module analysis.run;

import std.stdio;
import std.array;
import std.conv;
import std.algorithm;
import std.range;
import std.array;
import std.string;
import std.file;

import std.d.lexer;
import std.d.parser;
import std.d.ast;

import analysis.base;
import analysis.style;
import analysis.enumarrayliteral;
import analysis.pokemon;
import analysis.del;
import analysis.fish;
import analysis.numbers;
import analysis.objectconst;
import analysis.range;
import analysis.ifelsesame;
import analysis.unused;
import analysis.constructors;
import analysis.check_compare;
import analysis.check_name_clash;
import analysis.check_size_t;
import analysis.check_string_format;
import analysis.check_unused;

enum AnalyzerCheck : int {
	style_check = 0x1,
	enum_array_literal_check = 0x2,
	exception_check = 0x4,
	delete_check = 0x8,
	float_operator_check = 0x10,
	number_style_check = 0x20,
	object_const_check = 0x40,
	backwards_range_check = 0x80,
	if_else_same_check = 0x100,
	constructor_check = 0x200,
	unused_variable_check = 0x400,
	compare_check = 0x800,
	size_t_check = 0x1000,
	unused_check = 0x2000,
	name_clash_check = 0x4000,
	check_string_format = 0x8000,
	all = 0xFFFF
}

void messageFunction(string fileName, size_t line, size_t column, string message,
	bool isError)
{
	stderr.writefln("%s(%d:%d)[%s]: %s", fileName, line, column,
		isError ? "error" : "warn", message);
}

void syntaxCheck(File output, string[] fileNames)
{
	analyze(output, fileNames, AnalyzerCheck.all, false);
}

void load_phobos_module_data() {
/*
	// Get all the phobos library files
	string[] fileNames;
	string phobosDir = "/usr/include/dmd/phobos/std/";
	foreach (string fileName; dirEntries(phobosDir, SpanMode.shallow, true)) {
		if (fileName.endsWith(".d"))
			fileNames ~= fileName;
	}
	// FIXME: Hard coded to just a few modules for now ...
	fileNames = [
		"/usr/include/dmd/phobos/std/algorithm.d",
		"/usr/include/dmd/phobos/std/array.d",
		"/usr/include/dmd/phobos/std/ascii.d",
		"/usr/include/dmd/phobos/std/base64.d",
		"/usr/include/dmd/phobos/std/bigint.d"
	];
	fileNames = [];

	// Load each file as a module
	foreach (fileName; fileNames) {
		stderr.writefln("!!!!!!!!!!!!!! loading phobos file: %s", fileName);
		analysis.helpers.load_module(fileName);
	}
*/
}

// For multiple files
void analyze(File output, string[] fileNames, AnalyzerCheck analyzers, bool staticAnalyze = true)
{
	foreach (fileName; fileNames)
	{
		File f = File(fileName);
		auto code = uninitializedArray!(ubyte[])(to!size_t(f.size));
		f.rawRead(code);

		string[] results = analyze(fileName, code, analyzers, staticAnalyze);
		output.writeln(results.join("\n"));
	}
}

// For a string
string[] analyze(string fileName, ubyte[] code, AnalyzerCheck analyzers, bool staticAnalyze = true)
{
	import std.parallelism;

	load_phobos_module_data();

	auto lexer = byToken(code);
	auto app = appender!(typeof(lexer.front)[])();
	while (!lexer.empty)
	{
		app.put(lexer.front);
		lexer.popFront();
	}

	foreach (message; lexer.messages)
	{
		messageFunction(fileName, message.line, message.column, message.message,
			message.isError);
	}

	ParseAllocator p = new ParseAllocator;
	Module m = parseModule(app.data, fileName, p, &messageFunction);

	if (!staticAnalyze)
		return null;

	BaseAnalyzer[] checks;

	if (analyzers & AnalyzerCheck.style_check) checks ~= new StyleChecker(fileName);
	if (analyzers & AnalyzerCheck.enum_array_literal_check) checks ~= new EnumArrayLiteralCheck(fileName);
	if (analyzers & AnalyzerCheck.exception_check) checks ~= new PokemonExceptionCheck(fileName);
	if (analyzers & AnalyzerCheck.delete_check) checks ~= new DeleteCheck(fileName);
	if (analyzers & AnalyzerCheck.float_operator_check) checks ~= new FloatOperatorCheck(fileName);
	if (analyzers & AnalyzerCheck.number_style_check) checks ~= new NumberStyleCheck(fileName);
	if (analyzers & AnalyzerCheck.object_const_check) checks ~= new ObjectConstCheck(fileName);
	if (analyzers & AnalyzerCheck.backwards_range_check) checks ~= new BackwardsRangeCheck(fileName);
	if (analyzers & AnalyzerCheck.if_else_same_check) checks ~= new IfElseSameCheck(fileName);
	if (analyzers & AnalyzerCheck.constructor_check) checks ~= new ConstructorCheck(fileName);
	if (analyzers & AnalyzerCheck.unused_variable_check) checks ~= new UnusedVariableCheck(fileName);
	if (analyzers & AnalyzerCheck.compare_check) checks ~= new CompareCheck(fileName);
	if (analyzers & AnalyzerCheck.name_clash_check) checks ~= new NameClashCheck(fileName);
	if (analyzers & AnalyzerCheck.size_t_check) checks ~= new SizeTCheck(fileName);
	if (analyzers & AnalyzerCheck.check_string_format) checks ~= new CheckStringFormat(fileName);
	if (analyzers & AnalyzerCheck.unused_check) checks ~= new UnusedCheck(fileName);

	foreach (check; checks)
	{
		check.callVisit(m);
	}

	MessageSet set = new MessageSet;
	foreach (check; checks)
		foreach (message; check.messages)
			set.insert(message);

	string[] results;
	foreach (message; set[])
		results ~= "%s(%d:%d)[warn]: %s".format(message.fileName, message.line,
			message.column, message.message);
	p.deallocateAll();
	return results;
}

