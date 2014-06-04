// Copyright (c) 2014, Matthew Brennan Jones <matthew.brennan.jones@gmail.com>
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.check_size_t;

import std.stdio;
import std.array;
import std.string;
import std.stdint;

import std.d.ast;
import std.d.inspect;
import std.d.lexer;
import analysis.base;
import analysis.helpers;
import analysis.expressions;
import analysis.scope_frame;
import analysis.scope_analyzer;

/**
 * Checks for errors with using size_t:
 * size_t = long fails on 32bit but not on 64bit
 * int = size_t fails on 64bit but not on 32bit
 */
class SizeTCheck : ScopeAnalyzer
{
	alias visit = ScopeAnalyzer.visit;

	this(string fileName)
	{
		super(fileName);
	}

	override void visit(const VariableDeclaration varDec)
	{
		checkIinitializer(varDec);
		varDec.accept(this);
	}

	override void visit(const AssignExpression assExp)
	{
		assExp.accept(this);
		checkExpression(assExp);
	}

	override void visit(const FunctionCallExpression fncExp)
	{
		fncExp.accept(this);
		checkFunctionCalls(fncExp);
	}

	void checkIinitializer(const VariableDeclaration varDec)
	{
		// Just return if any args are null
		if (!varDec
			|| !varDec.type
			|| !varDec.declarators
			|| !varDec.declarators.length)
		{
			return;
		}

		// Get the types
		auto varDeclarator = varDec.declarators[0];
		size_t line, column;
		TypeData toType = getTypeData(varDec.type);
		TypeData fromType = getExpressionReturnType(varDeclarator, line, column);
		if (line == 0 || column == 0)
		{
			getVariableLineColumn(varDec, varDeclarator.name.text, line, column);
		}

		actualCheck(toType, fromType, line, column);
	}

	// checks:
	// function arguments
	// function parameters
	void checkFunctionCalls(const FunctionCallExpression funcExp)
	{
		// Just return if any args are null
		if (!funcExp
			|| !funcExp.arguments
			|| !funcExp.arguments.argumentList
			|| !funcExp.arguments.argumentList.items)
		{
			return;
		}

		// Get the name and args of the function to call
		string name = getFunctionCallName(funcExp);

		// Just return if it failed to get the function name
		if (!name)
			return;

		auto funcData = gScope.getFunction(name);
		TypeData[] argTypes = funcData.argTypes;

		// Just return if the args length does not match
		if (!argTypes || argTypes.length != funcExp.arguments.argumentList.items.length)
			return;

		foreach (i, assExp; funcExp.arguments.argumentList.items)
		{
			// Get the expression return types
			size_t line, column;
			TypeData toType = argTypes[i];
			TypeData fromType = getExpressionReturnType(assExp, line, column);

			string message = "For function argument %d, ".format(i);
			actualCheck(toType, fromType, line, column, message);
		}
	}

	/*
	FIXME: Make it work with:
	. unit tests
	. class static variables
	. templates
	*/
	// checks:
	// size_t = ulong on 32bit
	// uint = ulong on 64bit
	void checkExpression(const AssignExpression assExp)
	{
		// Just return if any args are null
		if (!assExp
			|| !assExp.ternaryExpression
			|| !assExp.assignExpression)
		{
			return;
		}

		// Just return if the expression is wrong
		if (assExp.operator != tok!"="
			&& assExp.operator != tok!"+="
			&& assExp.operator != tok!"-="
			&& assExp.operator != tok!"*="
			&& assExp.operator != tok!"/="
			&& assExp.operator != tok!"%="
			&& assExp.operator != tok!"&="
			&& assExp.operator != tok!"|="
			&& assExp.operator != tok!"^="
			&& assExp.operator != tok!"<<="
			&& assExp.operator != tok!">>="
			&& assExp.operator != tok!">>>="
			&& assExp.operator != tok!"^^=")
		{
			return;
		}

		// Get the expression return types
		size_t line, column;
		TypeData toType = getExpressionReturnType(assExp.ternaryExpression, line, column);
		TypeData fromType = getExpressionReturnType(assExp.assignExpression, line, column);

		actualCheck(toType, fromType, line, column);
	}

	void actualCheck(TypeData toType, TypeData fromType, size_t line, size_t column, string message=null)
	{
		//assert (line > 0, "Line needs to be greater than zero.");
		//assert (column > 0, "Column needs to be greater than zero.");

		if (!message)
			message = "";

		// size_t = long fails on 32bit
		if (toType.isSizeT() && fromType.isAnInt64())
		{
			message ~= std.string.format("%s will overflow %s on 32bit.", fromType, toType);
			addErrorMessage(line, column, message);
		}
		// int = size_t fails on 64bit
		else if (fromType.isSizeT() && toType.isAnInt32())
		{
			message ~= std.string.format("%s will overflow %s on 64bit.", fromType, toType);
			addErrorMessage(line, column, message);
		}
	}
}

unittest
{
	assertAnalyzerWarnings(q{
		size_t gSizeT = cast(ulong) 8; // [warn]: ulong will overflow size_t on 32bit.

		void testSizeT()
		{
			size_t a = 0;

			// standard types
			int zInt = 9;
			long zLong = 9;
			uint zUint = 9;
			ulong zUlong = 9;

			// stdint types
			int32_t zInt32 = 9;
			int64_t zInt64 = 9;
			uint32_t zUint32 = 9;
			uint64_t zUint64 = 9;

			// standard types implicit conversions
			a = zInt;
			a = zLong; // [warn]: long will overflow size_t on 32bit.
			a = zUint;
			a = zUlong; // [warn]: ulong will overflow size_t on 32bit.

			// stdint types implicit conversions
			a = zInt32;
			a = zInt64; // [warn]: int64_t will overflow size_t on 32bit.
			a = zUint32;
			a = zUint64; // [warn]: uint64_t will overflow size_t on 32bit.

			// standard types implicit conversions
			zInt = a; // [warn]: size_t will overflow int on 64bit.
			zLong = a;
			zUint = a; // [warn]: size_t will overflow uint on 64bit.
			zUlong = a;

			// stdint types implicit conversions
			zInt32 = a; // [warn]: size_t will overflow int32_t on 64bit.
			zInt64 = a;
			zUint32 = a; // [warn]: size_t will overflow uint32_t on 64bit.
			zUint64 = a;

			// assignment expressions
			a = zUint64; // [warn]: uint64_t will overflow size_t on 32bit.
			a += zUint64; // [warn]: uint64_t will overflow size_t on 32bit.
			a -= zUint64; // [warn]: uint64_t will overflow size_t on 32bit.
			a *= zUint64; // [warn]: uint64_t will overflow size_t on 32bit.
			a /= zUint64; // [warn]: uint64_t will overflow size_t on 32bit.
			a %= zUint64; // [warn]: uint64_t will overflow size_t on 32bit.
			a &= zUint64; // [warn]: uint64_t will overflow size_t on 32bit.
			a |= zUint64; // [warn]: uint64_t will overflow size_t on 32bit.
			a ^= zUint64; // [warn]: uint64_t will overflow size_t on 32bit.
			a <<= zUint64; // [warn]: uint64_t will overflow size_t on 32bit.
			a >>= zUint64; // [warn]: uint64_t will overflow size_t on 32bit.
			a >>>= zUint64; // [warn]: uint64_t will overflow size_t on 32bit.
			a ^^= zUint64; // [warn]: uint64_t will overflow size_t on 32bit.

			// assignment from assignment expressions
			a = a + zUint64; // [warn]: ulong will overflow size_t on 32bit.
			a = a - zUint64; // [warn]: ulong will overflow size_t on 32bit.
			a = a * zUint64; // [warn]: ulong will overflow size_t on 32bit.
			a = a / zUint64; // [warn]: ulong will overflow size_t on 32bit.
			a = a % zUint64; // [warn]: ulong will overflow size_t on 32bit.
			a = a & zUint64; // [warn]: ulong will overflow size_t on 32bit.
			a = a | zUint64; // [warn]: ulong will overflow size_t on 32bit.
			a = a ^ zUint64; // [warn]: ulong will overflow size_t on 32bit.
			a = a << zUint64; // [warn]: ulong will overflow size_t on 32bit.
			a = a >> zUint64; // [warn]: ulong will overflow size_t on 32bit.
			a = a >>> zUint64; // [warn]: ulong will overflow size_t on 32bit.
			a = a ^^ zUint64; // [warn]: ulong will overflow size_t on 32bit.

			// Globals
			gSizeT = zUint64; // [warn]: uint64_t will overflow size_t on 32bit.

			// Casting
			size_t blah = 0;
			//blah = cast(ulong) 7; // FIXME: [warn]: ulong will overflow size_t on 32bit.

			// Initialization
			size_t herp1 = zUlong; // [warn]: ulong will overflow size_t on 32bit.
			size_t derp1 = cast(ulong) 8; // [warn]: ulong will overflow size_t on 32bit.
			uint herp2 = a; // [warn]: size_t will overflow uint on 64bit.
			uint derp2 = cast(size_t) 8; // [warn]: size_t will overflow uint on 64bit.

			// Auto
			auto autoSizeT = a;
			auto autoInt = 9;
			autoSizeT = zUlong; // [warn]: ulong will overflow size_t on 32bit.
			autoInt = a; // [warn]: size_t will overflow int on 64bit.

			// Function result
			ulong getBleh()
			{
				return 5;
			}
			size_t bleh = getBleh(); // [warn]: ulong will overflow size_t on 32bit.
			bleh = getBleh(); // [warn]: ulong will overflow size_t on 32bit.

			// Function result with auto
			auto getAutoBleh()
			{
				ulong ret = 9;
				return ret;
			}
			bleh = getAutoBleh(); // FIXME: [warn]: ulong will overflow size_t on 32bit.

			// Function argument
			void funcArgTest(size_t arg1SizeT, uint arg2Uint)
			{
				assert (arg1SizeT);
				assert (arg2Uint);
			}
			funcArgTest(
				zUlong, // [warn]: For function argument 0, ulong will overflow size_t on 32bit.
				herp1 // [warn]: For function argument 1, size_t will overflow uint on 64bit.
			);

			// Function parameters
			void funcParamTest(uint paramUint, size_t paramSizeT)
			{
				paramUint = a; // [warn]: size_t will overflow uint on 64bit.
				paramSizeT = zLong; // [warn]: long will overflow size_t on 32bit.
			}
version (none)
{
			// Calling library functions
			uint aAount = std.string.countchars("abaab", "a"); // FIXME: boom on 32bit
			uint padding = 4;
			string leftPadded = std.string.leftJustify("Hello", padding); // FIXME: boom on 64bit
			string leftPadded = std.string.leftJustify!(string)("Hello", padding); // FIXME: boom on 64bit
}
			// Calling template functions
			//bleh = to!ulong(27); // boom on 32bit

			// Class members
			class Dog
			{
				size_t weight;
				size_t height;

				void blah()
				{
					int w = this.weight; // [warn]: size_t will overflow int on 64bit.
					int h = this.height; // [warn]: size_t will overflow int on 64bit.

					w = weight; // [warn]: size_t will overflow int on 64bit.
					h = height; // [warn]: size_t will overflow int on 64bit.
				}
			}

			// Class methods
			class Cat
			{
				size_t getWeight()
				{
					return 4;
				}
			}
			auto cat = new Cat();
			int catWeight = cat.getWeight(); // [warn]: size_t will overflow int on 64bit.

			// Struct members
			struct Puma
			{
				size_t weight;
				size_t height;

				void blah()
				{
					int w = this.weight; // [warn]: size_t will overflow int on 64bit.
					int h = this.height; // [warn]: size_t will overflow int on 64bit.

					w = weight; // [warn]: size_t will overflow int on 64bit.
					h = height; // [warn]: size_t will overflow int on 64bit.
				}
			}

			// Struct methods
			struct Platypus
			{
				size_t getWeight()
				{
					return 8;
				}
			}
			auto platypus = Platypus();
			int platypusWeight = platypus.getWeight(); // [warn]: size_t will overflow int on 64bit.

			// Enum members
			enum Colors : size_t
			{
				red,
				green,
				blue
			}
			//int color = Colors.green; // FIXME: [warn]: size_t will overflow int on 64bit.
		}
	}c, analysis.run.AnalyzerCheck.size_t_check);
	stderr.writeln("Unittest for SizeTCheck passed.");
}

