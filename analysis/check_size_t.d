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
		super(fileName, false);
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
version (none)
{
		string[] toNames = getVariableNames(varDec);

		stderr.writefln("??? VariableDeclaration - toNames: %s, toType: %s, fromType: %s, line: %d, column: %d", 
			toNames, 
			toType, 
			fromType, 
			line, 
			column
		);
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
		auto funcData = getFunctionDataByName(name);
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
version (none)
{
			stderr.writefln("??? FunctionCallExpression - toType: %s, fromType: %s, line: %d, column: %d", 
				toType, 
				fromType, 
				line, 
				column
			);
}
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
version (none)
{
		TokenData toName = getExpressionReturnTokenData(assExp.ternaryExpression);
		TokenData fromName = getExpressionReturnTokenData(assExp.assignExpression);

		stderr.writefln("??? AssignExpression - toName: %s, fromName: %s, toType: %s, fromType: %s, op: %s, line: %d, column: %d", 
			toName, 
			fromName, 
			toType, 
			fromType, 
			assExp.operator.str, 
			line, 
			column
		);
}
		actualCheck(toType, fromType, line, column);
	}

	void actualCheck(TypeData toType, TypeData fromType, size_t line, size_t column, string message=null)
	{
		//assert (line > 0, "Line needs to be greater than zero.");
		//assert (column > 0, "Column needs to be greater than zero.");

		writefln("!!! actualCheck to:%s, from:%s", toType, fromType);

		if (!message)
			message = "";

		// size_t = long fails on 32bit
		if (toType.toString() == "size_t")
		{
			switch (fromType.toString())
			{
				case "long":
				case "ulong":
				case "int64_t":
				case "uint64_t":
					message ~= std.string.format("%s will overflow %s on 32bit.", fromType, toType);
					addErrorMessage(line, column, message);
					break;
				default:
					break;
			}
		}
		// int = size_t fails on 64bit
		else if (fromType.toString() == "size_t")
		{
			switch (toType.toString())
			{
				case "int":
				case "uint":
				case "int32_t":
				case "uint32_t":
					message ~= std.string.format("%s will overflow %s on 64bit.", fromType, toType);
					addErrorMessage(line, column, message);
					break;
				default:
					break;
			}
		}
	}
}

unittest
{
	assertAnalyzerWarnings(q{
		void testSizeT()
		{
			size_t a = 0;

			// stdint types
			uint64_t zUint64 = 9;

			// assignment from assignment expressions
			a = a + zUint64; // [warn]: ulong will overflow size_t on 32bit.
		}
	}c, analysis.run.AnalyzerCheck.size_t_check);
	stderr.writeln("Unittest for SizeTCheck passed.");
}

