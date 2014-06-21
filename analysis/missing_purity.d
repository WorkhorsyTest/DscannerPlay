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
import dlang_helper;


/**
 * Checks for functions that can be pure.
 */
// FIXME: Make it work with globals
// FIXME: Make it work with methods
class MissingPurityCheck : ScopeAnalyzer
{
	alias visit = ScopeAnalyzer.visit;
	private bool[string] calls;
	private bool[string][string] dependencies;

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

	override void visit(const FunctionCallExpression node)
	{
		string name = getFunctionCallName(scopeManager.scope_, node);
		auto funcData = scopeManager.scope_.getFunction(name);

		// If the function is unknown, assume that it is not pure
		bool isPure = false;
		if (funcData is FunctionData.init)
			stderr.writefln("!!! failed to find function '%s'", name);
		else
			isPure = funcData.isPure;

		calls[name] = isPure;
		node.accept(this);
	}

	override void visit(const FunctionDeclaration node)
	{
		// Get the name of this function
		string name = node.name.text;
		auto funcData = scopeManager.scope_.getFunction(name);
		if (funcData is FunctionData.init)
		{
			stderr.writefln("!!! failed to find function '%s'", name);
			node.accept(this);
			return;
		}

		// Get the functions called by this function
		calls.clear();
		node.accept(this);

		// Check all the called functions to see if they are pure
		dependencies[name] = calls;
		bool areDependenciesPure = areDependenciesPure(name);

		// Warn if all the dependent functions are pure and this is not pure
		if (areDependenciesPure && !funcData.isPure)
		{
			string message = "The function '%s' can be pure.".format(name);
			size_t line = node.name.line;
			size_t column = node.name.column;
			addErrorMessage(line, column, message);
		}
	}

	private bool areDependenciesPure(string name)
	{
		foreach (string callName, bool callPure; dependencies[name])
		{
			// Retrun false if this function is not pure
			if (!callPure)
				return false;

			// Return false if this function calls any that are not pure
			if (!areDependenciesPure(callName))
				return false;
		}

		return true;
	}
}

unittest
{
	assertAnalyzerWarnings(q{
		void testFunctions()
		{
			pure int addPure(int a, int b)
			{
				return a + b;
			}

			int addNonPure(int a, int b) // [warn]: The function 'addNonPure' can be pure.
			{
				return a + b;
			}

			// Can be pure because addPure is pure
			int aaa(int a, int b) // [warn]: The function 'aaa' can be pure.
			{
				return addPure(a, b);
			}

			// Can't be pure because addNonPure is not pure
			int bbb(int a, int b)
			{
				return addNonPure(a, b);
			}
		}
	}c, analysis.run.AnalyzerCheck.missing_purity);

	stderr.writeln("Unittest for MissingPurityCheck passed.");
}

