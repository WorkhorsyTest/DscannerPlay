// Copyright (c) 2014, Matthew Brennan Jones <matthew.brennan.jones@gmail.com>
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.check_unused;

import std.stdio;
import std.array;
import std.string;
import std.stdint;

import std.d.ast;
import std.d.inspect;
import std.d.lexer;
import analysis.base;
import analysis.helpers;
import analysis.scope_frame;
import analysis.scope_analyzer;

/**
 * Checks for unused variables and function parameters.
 */
class UnusedCheck : ScopeAnalyzer
{
	alias visit = ScopeAnalyzer.visit;

	this(string fileName)
	{
		super(fileName);
	}

	override void visit(const BlockStatement blockSta)
	{
		blockSta.accept(this);
		checkVariablesUnused();
	}

	override void visit(const FunctionDeclaration funcDec)
	{
		funcDec.accept(this);
		checkFunctionParamsUnused(funcDec);
	}

	void checkFunctionParamsUnused(const FunctionDeclaration funcDec)
	{
		// Just return if any args are null
		if (!funcDec)
			return;

		// FIXME: Rename all args to params
		string funcName = funcDec.name.text;
		string[] funArgNames = getFunctionArgNames(funcDec);
		if (!funArgNames)
			return;

		foreach (argName; funArgNames)
		{
			auto data = gScope.getVariable(argName);
			if (data != VariableData.init && !data.isUsed)
			{
				string message = "Parameter '%s' of function '%s' is not used.".format(argName, funcName);
				addErrorMessage(funcDec.name.line, funcDec.name.column, message);
			}
		}
	}

	void checkVariablesUnused()
	{
		// Before the variables in the current scope frame are destroyed, look to see if they were used.
		foreach (name, data; gScope.getCurrentFrameVariables())
		{
			if (data != VariableData.init && !data.isUsed)
			{
				string message = "Variable '%s' is not used.".format(name);
				addErrorMessage(data.line, data.column, message);
			}
		}
	}
}

unittest
{
	assertAnalyzerWarnings(q{
		void ignore(T)(T thing)
		{
			writefln("thing: %s", thing);
		}

		void testUnused()
		{
			// Unused variables
			int a; // [warn]: Variable 'a' is not used.
			int b = 8; // [warn]: Variable 'b' is not used.

			// Unused in one declaration
			size_t x, y, z; // [warn]: Variable 'x' is not used.

			// Unused array
			int[5] unusedData = [1, 2, 3, 4, 5]; // [warn]: Variable 'unusedData' is not used.

			// Unused loop variables
			int[5] data = [1, 2, 3, 4, 5];
			for (size_t i=0; i<data.length; ++i)
			{
				int zebra; // [warn]: Variable 'zebra' is not used.
			}

			foreach (i; 0 .. data.length) // i is unused
			{
				int rhino; // [warn]: Variable 'rhino' is not used.
			}

			foreach (i, d; data) // FIXME: i and d are unused
			{
				int velociraptor; // [warn]: Variable 'velociraptor' is not used.
			}

			while (false)
			{
				int platypus; // [warn]: Variable 'platypus' is not used.
			}

			do
			{
				int human; // [warn]: Variable 'human' is not used.
			} while (false);

			// Unused condition variables
			if (true)
			{
				int puma; // [warn]: Variable 'puma' is not used.
			}

			if (auto ape = 3) // unused FIXME
			{
			}

			switch (false)
			{
				case true:
					int wolf = 1; // [warn]: Variable 'wolf' is not used.
					break;
				default:
					break;
			}

			// Unused try/catch/finally variables
			try
			{
				int la; // [warn]: Variable 'la' is not used.
			}
			catch (Exception ex)
			{
				int le; // [warn]: Variable 'le' is not used.
			}
			catch (Error err)
			{
				int lu; // [warn]: Variable 'lu' is not used.
			}
			catch (Throwable thr)
			{
				int le; // [warn]: Variable 'le' is not used.
			}
			finally
			{
				int lo; // [warn]: Variable 'lo' is not used.
			}

			// ok
			int add(int a, int b)
			{
				return a + b;
			}

			// Unused function parameters
			void yoWorld(int unusedParam, string name) // [warn]: Parameter 'unusedParam' of function 'yoWorld' is not used.
			{
				writefln("Howdy %s!", name);
			}
			yoWorld(3, "Y'all");

			// Unused function return value
			int getBlah()
			{
				return 9;
			}
			getBlah(); // return is unused FIXME

			// Unused class method variable
			class Dog
			{
				string dogName; // ok
				void bark()
				{
					int volume = 11; // [warn]: Variable 'volume' is not used.
				}
			}

			// Unused struct method variable
			struct Cat
			{
				string catName; // ok
				void meow()
				{
					int volume = 11; // [warn]: Variable 'volume' is not used.
				}
			}

			// Unused class method parameter
			class Pig
			{
				void attack(string name) // [warn]: Parameter 'name' of function 'attack' is not used.
				{
				}
			}

			// Used builtin properties
			{
				int A, B, C, D, E, F, G; // ok
				ignore(A.alignof);
				ignore(B.init);
				ignore(C.mangleof);
				ignore(D.stringof);
				ignore(E.sizeof);
				ignore(typeid(F));
				ignore(typeof(G));
			}

			// Used array properties
			{
				int[] A, B; // ok
				ignore(A.length);
				ignore(B.dup);
			}
		}
	}c, analysis.run.AnalyzerCheck.unused_check);

	stderr.writeln("Unittest for UnusedCheck passed.");
}

