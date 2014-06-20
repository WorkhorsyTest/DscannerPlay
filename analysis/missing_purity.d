// Copyright (c) 2014, Matthew Brennan Jones <matthew.brennan.jones@gmail.com>
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.missing_purity;

import std.stdio;
import std.string;
import std.d.ast;
import std.d.lexer;
import analysis.base;
import analysis.ast_helpers;
import analysis.manager;
import analysis.scope_frame;
import analysis.scope_analyzer;


/**
 * FIXME
 */
class MissingPurityCheck : ScopeAnalyzer
{
	alias visit = ScopeAnalyzer.visit;

	this(string fileName)
	{
		super(fileName);
		//loadPhobosModuleData();
	}

	void loadPhobosModuleData()
	{
		import std.file;

		// Get all the phobos library files
		string[] fileNames;
		string phobosDir = "/usr/include/dmd/phobos/std/";
		foreach (string fileName; dirEntries(phobosDir, SpanMode.shallow, true))
		{
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
		//fileNames = [];

		// Load each file as a module
		foreach (fileName; fileNames)
		{
			stderr.writefln("!!!!!!!!!!!!!! loading phobos file: %s", fileName);
			scopeManager.loadModule(fileName);
		}
	}

	override void visit(const FunctionDeclaration node)
	{
		auto data = getFunctionData(scopeManager.scope_, node);
		writefln("!!! function name:%s, isPure:%s", data.name, data.isPure);

		// FIXME: Here we need to walk over all function calls, and sub function
		// calls to see they are pure too.
	}
}

unittest
{
	assertAnalyzerWarnings(q{
		pure int addPure(int a, int b)
		{
			return a + b;
		}

		int addNonPure(int a, int b)
		{
			return a + b;
		}

		// Can be pure because addPure is pure
		int aaa(int a, int b) // FIXME: [warn]: Function 'aaa' can be pure.
		{
			return addPure(a, b);
		}

		// Can't be pure because addNonPure is not pure
		int bbb(int a, int b)
		{
			return addNonPure(a, b);
		}
	}c, analysis.run.AnalyzerCheck.missing_purity);

	stderr.writeln("Unittest for MissingPurityCheck passed.");
}

