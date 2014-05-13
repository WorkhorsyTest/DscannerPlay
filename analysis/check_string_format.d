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
import analysis.walking_analyzer;
version(none) {
/*
FIXME: Make this work with:
 // http://dlang.org/phobos/std_string.html#.sformat
 std.string.sformat

 // http://dlang.org/phobos/std_format.html
 std.format.formattedWrite
 std.format.formattedRead
 std.format.FormatSpec
 std.format.singleSpec
 std.format.formatValue
 std.format.unformatValue
*/

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

const auto FORMAT_FUNCTIONS = 
ModuleFunctionSet("std.string", [
	"format"
]);



// FIXME: Works with everything but UFC
/**
 * Checks for improper use of std.stdio.writefln and std.string.format
 */
class CheckStringFormat : BaseWalkingAnalyzer {
	alias visit = BaseWalkingAnalyzer.visit;

	this(string fileName) {
		super(fileName, false);
	}

	override void visit(const FunctionCallExpression funcCallExp, VisitMode visit_mode) {
		if(visit_mode != VisitMode.on_start)
			return;

		// Get the function name and args
		string func_name = get_function_call_name(funcCallExp);

		// Just return if not one of the functions to test
		if(!WRITE_F_FUNCTIONS.has_function(func_name)
			&& !WRITE_FUNCTIONS.has_function(func_name)
			&& !FORMAT_FUNCTIONS.has_function(func_name)) {
			return;
		}

		// Get all the arguments passed to the function
		TokenData[] token_args = get_function_call_arguments(funcCallExp);

		// Just return if less than 1 arg, or the first arg is not a string
		if(token_args.length < 1 || token_args[0].type_data.name != "string")
			return;

		// Get the format string and arguments
		string string_with_formats = token_args[0].value;
		size_t line = token_args[0].line;
		size_t column = token_args[0].column;
		TokenData[] args = [];
		if(token_args.length > 1)
			args = token_args[1 .. $];

		// Get all the format strings
		auto any_format = regex(r"\%\w");
		auto matches = std.regex.matchAll(string_with_formats, any_format);
		size_t matches_length = 0;
		foreach(match; matches)
			matches_length++; // FIXME: There has to be a better way to get the length

		// Check for wrong function EG: write instead of writef
		if(WRITE_FUNCTIONS.has_function(func_name)) {
			if(matches_length && args.length) {
				string message = "Function '%s' does not expect format strings.".format(func_name);
				addErrorMessage(line, column, message);
			}
			return;
		}

		// Make sure the number of format strings matches the number of arguments
		if(matches_length != args.length) {
			string message = "Found %d format strings, but there were %d arguments.".format(
				matches_length, args.length);
			addErrorMessage(line, column, message);
			return;
		}

		// Make sure the format strings will work with the data types
		size_t n = 0;
		foreach(match; matches) {
			string format_pattern = match[0];
			string arg_type = args[n].type_data.name;
			string message = null;

			switch(format_pattern) {
				case "%s": // string
					// Everything work with string
					break;
				case "%c": // character
					if(CHAR_TYPES.find(arg_type).empty) {
						message = "Format '%s' expects an char type, not '%s'.".format(format_pattern, arg_type);
					}
					break;
				case "%d": // integer
				case "%b": // binary
				case "%x": // hexadecimal
				case "%X": // hexadecimal upper case
				case "%o": // octal
					if(INTEGER_TYPES.find(arg_type).empty && 
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
					if(FLOAT_TYPES.find(arg_type).empty) {
						message = "Format '%s' expects a float type, not '%s'.".format(format_pattern, arg_type);
					}
					break;
				default:
					break;
			}

			// There was an error, so print it and return
			if(message) {
				addErrorMessage(line, column, message);
				return;
			}
			n++;
		}
	}
}

unittest {
	should_warn(q{
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

			// No argument
			std.string.format("%s"); // [warn]: Found 1 format strings, but there were 0 arguments.

			// No formats
			std.string.format("", 99); // [warn]: Found 0 format strings, but there were 1 arguments.

			// Too many arguments
			std.string.format("%s", 1, 2, 3); // [warn]: Found 1 format strings, but there were 3 arguments.
			format("%s", 1, 2, 3); // [warn]: Found 1 format strings, but there were 3 arguments.

			// Too few arguments
			std.string.format("%s, %d, %s", 1); // [warn]: Found 3 format strings, but there were 1 arguments.

			// Incompatible format
			std.string.format("%d", "blah"); // [warn]: Format '%d' expects an integer/bool/char type, not 'string'.
			std.string.format("%f", 3); // [warn]: Format '%f' expects a float type, not 'int'.
		}
	}c, analysis.run.AnalyzerCheck.check_string_format);

	stderr.writeln("Unittest for CheckStringFormat passed.");
}
}
