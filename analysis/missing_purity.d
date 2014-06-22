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
// FIXME: Make it work with methods
class MissingPurityCheck : ScopeAnalyzer
{
	alias visit = ScopeAnalyzer.visit;
	private bool[string] functionCalls;
	private size_t[string] variableCalls;
	private bool[string][string] functionDependencies;
	private size_t[string][string] variableDependencies;

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

	// Finds all the variables used inside a function
	override void visit(const IdentifierOrTemplateInstance node)
	{
		if (node.identifier !is Token.init)
		{
			string name = node.identifier.text;
			auto varData = scopeManager.scope_.getVariable(name);
			if (varData !is VariableData.init)
			{
				size_t onFrame = scopeManager.scope_.frames.length;
				variableCalls[name] = onFrame;
			}
		}

		node.accept(this);
	}

	// Finds all the functions used inside a function
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

		functionCalls[name] = isPure;
		node.accept(this);
	}

	// Finds 
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

		// Get the functions & variables used by this function
		functionCalls.clear();
		variableCalls.clear();
		node.accept(this);

		// Check all the called functions to see if they are pure,
		// and used variables to see if they are outside the stack frame.
		functionDependencies[name] = functionCalls;
		variableDependencies[name] = variableCalls;
		bool areDependenciesPure = areDependenciesPure(funcData);

		// Warn if the function can be pure, but is not
		if (areDependenciesPure && !funcData.isPure)
		{
			string message = "The function '%s' can be pure.".format(name);
			size_t line = node.name.line;
			size_t column = node.name.column;
			addErrorMessage(line, column, message);
		}
	}

	private bool areDependenciesPure(const FunctionData funcData)
	{
		// Check functions
		foreach (string callName, bool callPure; functionDependencies[funcData.name])
		{
			// Retrun false if this function is not pure
			if (!callPure)
				return false;

			// Return false if this function calls any that are not pure
			auto subFuncData = scopeManager.scope_.getFunction(callName);
			if (subFuncData !is FunctionData.init
				&& !areDependenciesPure(subFuncData))
				return false;
		}

		// Check variables
		foreach (string varName, size_t varFrame; variableDependencies[funcData.name])
		{
			auto varData = scopeManager.scope_.getVariable(varName);
			if (varData !is VariableData.init)
			{
				// Return false if the variable is outside the function's scope frame
				if (varData.frame >= funcData.frame)
					return false;
			}
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

		void testGlobas()
		{
			immutable int gOne = 1;

			pure int addOnePure(int a)
			{
				return a + 1;
			}

			// Can't be pure because it uses a global
			int addOneNonPure(int a)
			{
				return a + gOne;
			}

			// Can be pure because addOnePure is pure
			int aaa(int a) // [warn]: The function 'aaa' can be pure.
			{
				return addOnePure(a);
			}

			// Can't be pure because addOneNonPure is not pure
			int bbb(int a)
			{
				return addOneNonPure(a);
			}
		}
	}c, analysis.run.AnalyzerCheck.missing_purity);

	stderr.writeln("Unittest for MissingPurityCheck passed.");
}

