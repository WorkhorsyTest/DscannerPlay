// Copyright (c) 2014, Matthew Brennan Jones <matthew.brennan.jones@gmail.com>
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.helpers;

import std.stdio;
import std.array;
import std.string;
import std.stdint;
import std.conv;
import dlang_helper;

import std.d.ast;
import std.d.lexer;


void should_warn(string code, analysis.run.AnalyzerCheck analyzers, string file=__FILE__, size_t line=__LINE__) {
	import analysis.run;

	// Reset everything
//	stack_frame_clear_everything();

	// Run the code and get any warnings
	string[] raw_warnings = analyze("test", cast(ubyte[]) code, analyzers);
	string[] code_lines = code.split("\n");

	// Get the warnings ordered by line
	string[size_t] warnings;
	for(size_t i=0; i<raw_warnings.length; ++i) {
		size_t warn_line = line - 1 + std.conv.to!size_t(raw_warnings[i].between("test(", ":"));
		warnings[warn_line] = raw_warnings[i].after(")");
//		stderr.writefln("!!! warnings[%d] = \"%s\"", warn_line, warnings[warn_line]);
	}

	// Get all the messages from the comments in the code
	string[size_t] messages;
	foreach(i, code_line; code_lines) {
		// Skip if no [warn] comment
		if(code_line.indexOf("// [warn]:") == -1)
			continue;

		// Skip if there is no comment or code
		string code_part = code_line.before("// ");
		string comment_part = code_line.after("// ");
		if(!code_part.length || !comment_part.length)
			continue;

		// Get the line of this code line
		size_t line_no = i + line;

		// Get the message
//		stderr.writefln("!!! message[%d] = \"%s\"", line_no, comment_part);
		messages[line_no] = comment_part;
	}

	// Throw an assert error if any messages are not listed in the warnings
	foreach(line_no, message; messages) {
//		stderr.writefln("!!!!!! messages[%d] : %s", line_no, messages[line_no]);
		// No warning
		if(line_no !in warnings) {
			string errors = "Expected warning:\n%s\nFrom source code at (%s:?):\n%s".format(
				messages[line_no], 
				line_no, 
				code_lines[line_no - line]
			);
			throw new core.exception.AssertError(errors, file, line_no);
		// Different warning
		} else if(warnings[line_no] != messages[line_no]) {
			string errors = "Expected warning:\n%s\nBut was:\n%s\nFrom source code at (%s:?):\n%s".format(
				messages[line_no], 
				warnings[line_no], 
				line_no, 
				code_lines[line_no - line]
			);
			throw new core.exception.AssertError(errors, file, line_no);
		}
	}

	// Throw an assert error if there were any warnings that were not expected
	string[] unexpected_warnings;
	foreach(line_no, warning; warnings) {
//		stderr.writefln("!!!!!! warnings[%d] : %s", line_no, warning);
		// Unexpected warning
		if(line_no !in messages) {
			unexpected_warnings ~= "%s\nFrom source code at (%s:?):\n%s".format(
				warning, 
				line_no, 
				code_lines[line_no - line]
			);
		}
	}
	if(unexpected_warnings.length) {
		string message = "Unexpected warnings:\n" ~ unexpected_warnings.join("\n");
		throw new core.exception.AssertError(message, file, line);
	}
}

