// Copyright (c) 2014, Matthew Brennan Jones <matthew.brennan.jones@gmail.com>
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module std.d.codegen;

import std.stdio;
import std.string;
import std.stdint;

template typeNames(fields ...) {
	private string getTypeNames() {
		string[] result;
		
		foreach(field; fields) {
			result ~= field.stringof;
		}
		return std.string.join(result, ".");
	}
	immutable typeNames = getTypeNames();
}

template callOnActualType(string ifClause, string elseClause, string types) {
	private string generateFunction() {
		string[] result;

		foreach(n, type; std.string.split(types, ".")) {
			string condition = n ? "else if" : "if";
			result ~= condition ~ " (auto actual = cast(const " ~ type ~ ") unknown) {\n" ~ ifClause ~ "\n}";
		}

		result ~= "else {\n" ~ elseClause ~ ";\n}";

		return std.string.join(result, "\n");
	}

	immutable callOnActualType = generateFunction();
}


