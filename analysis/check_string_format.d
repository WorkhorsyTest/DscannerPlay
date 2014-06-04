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
import analysis.expressions;
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

string getFullFunctionName(string funcName)
{
	auto funcSets = [
		WRITE_F_FUNCTIONS,
		WRITE_FUNCTIONS,
		STRING_FUNCTIONS,
		FORMAT_FUNCTIONS
	];

	foreach (funcSet; funcSets)
	{
		string fullName = funcSet.getFunctionFullName(funcName);
		if (fullName)
		{
			return fullName;
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
class CheckStringFormat : ScopeAnalyzer
{
	alias visit = ScopeAnalyzer.visit;

	this(string fileName)
	{
		super(fileName);
	}

	override void visit(const FunctionCallExpression funcCallExp)
	{
		funcCallExp.accept(this);

		// Get the function name and args
		string funcName = getFunctionCallName(funcCallExp);

		// Just return if it failed to get the function name
		if (!funcName)
			return;

		// Just return if not one of the functions to test
		string fullFuncName = getFullFunctionName(funcName);
		if (fullFuncName is null)
			return;

		// Get all the arguments passed to the function
		TokenData[] tokenArgs = getFunctionCallArguments(funcCallExp);

		// Get the format string and arguments
		size_t argOffset = 0;
		if (fullFuncName == "std.string.sformat" || fullFuncName == "std.format.formattedWrite")
		{
			argOffset = 1;
		}
		// Just return if there are not enough args, or the format arg is not a string
		if (tokenArgs.length < argOffset+1 || tokenArgs[argOffset].typeData.name != "string")
			return;
		string stringWithFormats = tokenArgs[argOffset].value;
		size_t line = tokenArgs[argOffset].line;
		size_t column = tokenArgs[argOffset].column;
		TokenData[] args = [];
		if (tokenArgs.length > argOffset+1)
			args = tokenArgs[argOffset+1 .. $];

		// Get all the format strings
		auto anyFormat = regex(r"\%\w");
		auto matches = std.regex.matchAll(stringWithFormats, anyFormat);
		size_t matchesLength = 0;
		foreach (match; matches)
			matchesLength++; // FIXME: There has to be a better way to get the length

		// Check for wrong function EG: write instead of writef
		if (WRITE_FUNCTIONS.hasFunction(funcName))
		{
			if (matchesLength && args.length)
			{
				string message = "Function '%s' does not expect format strings.".format(funcName);
				addErrorMessage(line, column, message);
			}
			return;
		}

		// Make sure the number of format strings matches the number of arguments
		if (matchesLength != args.length)
		{
			string message = "Found %d format strings, but there were %d arguments.".format(
				matchesLength, args.length);
			addErrorMessage(line, column, message);
			return;
		}

		// Make sure the format strings will work with the data types
		size_t n = 0;
		foreach (match; matches)
		{
			string formatPattern = match[0];
			string argType = args[n].typeData.name;
			string message = null;

			switch (formatPattern)
			{
				case "%s": // string
					// Everything work with string
					break;
				case "%c": // character
					if (CHAR_TYPES.find(argType).empty)
					{
						message = "Format '%s' expects an char type, not '%s'.".format(formatPattern, argType);
					}
					break;
				case "%d": // integer
				case "%b": // binary
				case "%x": // hexadecimal
				case "%X": // hexadecimal upper case
				case "%o": // octal
					if (INTEGER_TYPES.find(argType).empty
						&& BOOL_TYPES.find(argType).empty
						&& CHAR_TYPES.find(argType).empty)
					{
						message = "Format '%s' expects an integer/bool/char type, not '%s'.".format(formatPattern, argType);
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
					if (FLOAT_TYPES.find(argType).empty)
					{
						message = "Format '%s' expects a float type, not '%s'.".format(formatPattern, argType);
					}
					break;
				default:
					break;
			}

			// There was an error, so print it and return
			if (message)
			{
				addErrorMessage(line, column, message);
				return;
			}
			n++;
		}
	}
}

unittest
{
	assertAnalyzerWarnings(q{
		void testWrite()
		{
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

		void testFormat()
		{
			import std.string;

			// Control
			std.string.format("total: %d", 400); // Called with full namespace
			format("total: %d", 400); // Called with import namespace
			//FIXME: "total: %d".format(400); // Called with UFC

			// Incompatible format
			format("%d", "blah"); // [warn]: Format '%d' expects an integer/bool/char type, not 'string'.
			std.string.format("%f", 3); // [warn]: Format '%f' expects a float type, not 'int'.
		}

		void testSformat()
		{
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

		void testFormattedWrite()
		{
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

