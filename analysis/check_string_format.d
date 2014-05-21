// Copyright (c) 2014, Matthew Brennan Jones <matthew.brennan.jones@gmail.com>
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.check_string_format;

import std.stdio;
import std.array;
import std.algorithm;
import std.string;
import std.stdint;
import std.regex;

import std.d.ast;
import std.d.inspect;
import std.d.lexer;
import analysis.base;
import analysis.helpers;
import analysis.scope_analyzer;


// http://dlang.org/phobos/std_format.html
const string[] FORMAT_STRINGS = [
	"%s", // string
	"%d", // integer
	"%f", // float
	"%F", // float uppper case
	"%b", // binary
	"%x", // hexadecimal lower case
	"%X", // hexadecimal upper case
	"%o", // octal
	"%c", // character
	"%e", // float 2x
	"%E", // float 2x upper case
	"%g", // float but no superfluous . and zeros lower case
	"%G", // float but no superfluous . and zeros upper case
	"%a", // floating point hexadecimal lower case
	"%A", // floating point hexadecimal upper case
];

const auto WRITE_F_FUNCTIONS = 
ModuleFunctionSet("std.stdio", [
	"writef", 
	"writefln"
]);

const auto WRITE_FUNCTIONS = 
ModuleFunctionSet("std.stdio", [
	"write", 
	"writeln"
]);

const auto STRING_FUNCTIONS = 
ModuleFunctionSet("std.string", [
	"format", 
	"sformat"
]);

const auto FORMAT_FUNCTIONS = 
ModuleFunctionSet("std.format", [
	"formattedWrite"
]);

string get_full_function_name(string func_name) {
	auto func_sets = [
		WRITE_F_FUNCTIONS, 
		WRITE_FUNCTIONS, 
		STRING_FUNCTIONS, 
		FORMAT_FUNCTIONS
	];

	foreach (func_set; func_sets) {
		string full_name = getFunctionFullName(func_set, func_name);
		if (full_name) {
			return full_name;
		}
	}

	return null;
}


// FIXME: This does not work when calling function using UFC
/**
 * Checks for improper use of format functions. Including:
 * 1. Number of format strings and arguments don't match
 * 2. Using write instead of writef
 * 3. Using writeln instead of writefln
 * 4. Using %c with non char types
 * 5. Using %d, %b, %x, %o with non integer/bool/char types
 * 6. Using %f, %e, %g, %a with non float types
 *
 * Functions that are checked:
 * std.stdio.writef
 * std.stdio.writefln
 * std.string.format
 * std.string.sformat
 * std.format.formattedWrite
 *
 * FIXME: Make these functions work too:
 * // http://dlang.org/phobos/std_format.html
 * std.format.formattedRead
 * std.format.FormatSpec
 * std.format.singleSpec
 * std.format.formatValue
 * std.format.unformatValue
 **/
class CheckStringFormat : ScopeAnalyzer {
	alias visit = ScopeAnalyzer.visit;

	this(string fileName) {
		super(fileName, false);
	}

	override void visit(const FunctionCallExpression funcCallExp) {
		funcCallExp.accept(this);

		// Get the function name and args
		string func_name = getFunctionCallName(funcCallExp);

		// Just return if not one of the functions to test
		string full_func_name = get_full_function_name(func_name);
		if (full_func_name is null)
			return;

		// Get all the arguments passed to the function
		TokenData[] token_args = getFunctionCallArguments(funcCallExp);

		// Get the format string and arguments
		size_t arg_offset = 0;
		if (full_func_name == "std.string.sformat" || full_func_name == "std.format.formattedWrite") {
			arg_offset = 1;
		}
		// Just return if there are not enough args, or the format arg is not a string
		if (token_args.length < arg_offset+1 || token_args[arg_offset].typeData.name != "string")
			return;
		string string_with_formats = token_args[arg_offset].value;
		size_t line = token_args[arg_offset].line;
		size_t column = token_args[arg_offset].column;
		TokenData[] args = [];
		if (token_args.length > arg_offset+1)
			args = token_args[arg_offset+1 .. $];

		// Get all the format strings
		auto any_format = regex(r"\%\w");
		auto matches = std.regex.matchAll(string_with_formats, any_format);
		size_t matches_length = 0;
		foreach (match; matches)
			matches_length++; // FIXME: There has to be a better way to get the length

		// Check for wrong function EG: write instead of writef
		if (WRITE_FUNCTIONS.hasFunction(func_name)) {
			if (matches_length && args.length) {
				string message = "Function '%s' does not expect format strings.".format(func_name);
				addErrorMessage(line, column, message);
			}
			return;
		}

		// Make sure the number of format strings matches the number of arguments
		if (matches_length != args.length) {
			string message = "Found %d format strings, but there were %d arguments.".format(
				matches_length, args.length);
			addErrorMessage(line, column, message);
			return;
		}

		// Make sure the format strings will work with the data types
		size_t n = 0;
		foreach (match; matches) {
			string format_pattern = match[0];
			string arg_type = args[n].typeData.name;
			string message = null;

			switch(format_pattern) {
				case "%s": // string
					// Everything work with string
					break;
				case "%c": // character
					if (CHAR_TYPES.find(arg_type).empty) {
						message = "Format '%s' expects an char type, not '%s'.".format(format_pattern, arg_type);
					}
					break;
				case "%d": // integer
				case "%b": // binary
				case "%x": // hexadecimal
				case "%X": // hexadecimal upper case
				case "%o": // octal
					if (INTEGER_TYPES.find(arg_type).empty && 
						BOOL_TYPES.find(arg_type).empty && 
						CHAR_TYPES.find(arg_type).empty) {
						message = "Format '%s' expects an integer/bool/char type, not '%s'.".format(format_pattern, arg_type);
					}
					break;
				case "%f": // float
				case "%F": // float uppper case
				case "%e": // float 2x
				case "%E": // float 2x upper case
				case "%g": // float but no superfluous . and zeros
				case "%G": // float but no superfluous . and zeros uppper case
				case "%a": // floating point hexadecimal
				case "%A": // floating point hexadecimal upper case
					if (FLOAT_TYPES.find(arg_type).empty) {
						message = "Format '%s' expects a float type, not '%s'.".format(format_pattern, arg_type);
					}
					break;
				default:
					break;
			}

			// There was an error, so print it and return
			if (message) {
				addErrorMessage(line, column, message);
				return;
			}
			n++;
		}
	}
}

unittest {
	assertAnalyzerWarnings(q{
		void test_write() {
			import std.stdio;

			// Control
			std.stdio.writef("total: %d", 400); // Called with full namespace
			writef("total: %d", 400); // Called with import namespace
			//FIXME: "total: %d".writef(400); // Called with UFC

			// No argument
			writefln("%s"); // [warn]: Found 1 format strings, but there were 0 arguments.

			// No formats
			writefln("", 99); // [warn]: Found 0 format strings, but there were 1 arguments.

			// Too many arguments
			writefln("%s", 1, 2, 3); // [warn]: Found 1 format strings, but there were 3 arguments.

			// Too few arguments
			writefln("%s, %d, %s", 1); // [warn]: Found 3 format strings, but there were 1 arguments.

			// Incompatible format
			writefln("%d", "blah"); // [warn]: Format '%d' expects an integer/bool/char type, not 'string'.
			writefln("%f", 3); // [warn]: Format '%f' expects a float type, not 'int'.

			// writeln instead of writefln
			writeln("%d", 3); // [warn]: Function 'writeln' does not expect format strings.

			// write instead of writef
			write("%s", "name"); // [warn]: Function 'write' does not expect format strings.

			// The thing being written just happends to look like a format string.
			writeln("%d"); // ok
			write("%s"); // ok
		}

		void test_format() {
			import std.string;

			// Control
			std.string.format("total: %d", 400); // Called with full namespace
			format("total: %d", 400); // Called with import namespace
			//FIXME: "total: %d".format(400); // Called with UFC

			// Incompatible format
			format("%d", "blah"); // [warn]: Format '%d' expects an integer/bool/char type, not 'string'.
			std.string.format("%f", 3); // [warn]: Format '%f' expects a float type, not 'int'.
		}

		void test_sformat() {
			import std.string;
			char[100] buf;
			char[] output;

			// Control
			output = std.string.sformat(buf, "%d", 1);
			output = sformat(buf, "%d", 1);

			// No args and no format
			output = sformat(buf, "%d"); // [warn]: Found 1 format strings, but there were 0 arguments.
			output = std.string.sformat(buf, "", 88); // [warn]: Found 0 format strings, but there were 1 arguments.
		}

		void test_formatted_write() {
			import std.array;
			import std.format;

			// Control
			auto writer = appender!string();
			formattedWrite(writer, "%d", 77);
			std.format.formattedWrite(writer, "%s.", "blah");

			// No args and no format
			formattedWrite(writer, "%d", "blah"); // [warn]: Format '%d' expects an integer/bool/char type, not 'string'.
			std.format.formattedWrite(writer, "%f.", "3"); // [warn]: Format '%f' expects a float type, not 'string'.
		}
	}c, analysis.run.AnalyzerCheck.check_string_format);

	stderr.writeln("Unittest for CheckStringFormat passed.");
}

