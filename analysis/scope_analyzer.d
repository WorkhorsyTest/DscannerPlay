// Copyright (c) 2014, Matthew Brennan Jones <matthew.brennan.jones@gmail.com>
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.scope_analyzer;

import std.stdio;
import std.string;
import std.stdint;

import std.d.ast;
import std.d.codegen;
import std.d.lexer;
import analysis.base;
import analysis.helpers;
import analysis.scope_frame;
import dlang_helper;

/*
This works like the BaseAnalyzer but tracks all the variables, classes,
functions, et cetera, and know what is in scope and hold meta data 
about everything.
*/
class ScopeAnalyzer : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	this(string fileName)
	{
		super(fileName);
		gScope = new Scope();
	}

	override void visitStart(const AddExpression node)
	{
		markUsedVariables(node);
	}

	override void visitStart(const AndAndExpression node)
	{
		markUsedVariables(node);
	}

	override void visitStart(const AndExpression node)
	{
		markUsedVariables(node);
	}

	override void visitStart(const AssertExpression node)
	{
		markUsedVariables(node);
	}

	override void visitStart(const AssignExpression node)
	{
		markUsedVariables(node);
	}

	override void visitStart(const BlockStatement node)
	{
		gScope.pushFrame();
	}

	override void visitEnd(const BlockStatement node)
	{
		gScope.popFrame();
	}

	override void visitStart(const ClassDeclaration node)
	{
		gScope.parentsPush(IdentifierType.class_);
		gScope.thisPointersPush(node.name.text);

		declareClass(node);
	}

	override void visitEnd(const ClassDeclaration node)
	{
		gScope.thisPointersPop();
		gScope.parentsPop();
	}

	override void visitStart(const CmpExpression node)
	{
		markUsedVariables(node);
	}

	override void visitStart(const Declaration node)
	{
		// Add decorations such as properties, auto, ref, et cetera
		Decoration decoration = getDeclarationDecorations(node);
		gScope.decorationsPush(decoration);
	}

	override void visitEnd(const Declaration node)
	{
		gScope.decorationsPop();
	}

	override void visitStart(const EnumDeclaration node)
	{
		gScope.parentsPush(IdentifierType.enum_);
		declareEnum(node);
	}

	override void visitEnd(const EnumDeclaration node)
	{
		gScope.parentsPop();
	}

	override void visitStart(const EqualExpression node)
	{
		markUsedVariables(node);
	}

	override void visitStart(const FunctionDeclaration node)
	{
		// Only declare if NOT a struct/class method
		IdentifierType identifierType;
		if (gScope.parentsPeak() != IdentifierType.struct_ && gScope.parentsPeak() != IdentifierType.class_)
		{
			declareFunction(node);
			identifierType = IdentifierType.function_;
		}
		else
		{
			identifierType = IdentifierType.method_;
		}

		gScope.parentsPush(identifierType);
		gScope.pushFrame();
	}

	override void visitEnd(const FunctionDeclaration node)
	{
		gScope.popFrame();
		gScope.parentsPop();
	}

	override void visitStart(const IdentityExpression node)
	{
		markUsedVariables(node);
	}

	override void visitStart(const InExpression node)
	{
		markUsedVariables(node);
	}

	override void visitStart(const Module node)
	{
		gScope.pushFrame();

		gScope.parentsPush(IdentifierType.module_);
		declareModule(node);

		// Declare the global functions and variables
		// This is a special case, because global functions do NOT have to 
		// be declared in order.
		foreach (decl; node.declarations)
		{
			if (!decl) continue;

			// Add decorations such as properties, auto, ref, et cetera
			Decoration decoration = getDeclarationDecorations(decl);
			gScope.decorationsPush(decoration);

			// Add the import
			if (decl.importDeclaration && decl.importDeclaration.singleImports)
			{
				foreach (singleImport; decl.importDeclaration.singleImports)
				{
					declareImport(singleImport);
				}
			}
			// Declare the function
			else if (decl.functionDeclaration)
			{
				declareFunction(decl.functionDeclaration);
			}
			// Declare the variable
			else if (decl.variableDeclaration)
			{
				declareVariable(decl.variableDeclaration);
			}

			// Remove decorations
			gScope.decorationsPop();
		}
	}

	override void visitEnd(const Module node)
	{
		gScope.parentsPop();
		gScope.popFrame();
	}

	override void visitStart(const MulExpression node)
	{
		markUsedVariables(node);
	}

	override void visitStart(const OrOrExpression node)
	{
		markUsedVariables(node);
	}

	override void visitStart(const OrExpression node)
	{
		markUsedVariables(node);
	}

	override void visitStart(const Parameter node)
	{
		declareParameter(node);
	}

	override void visitStart(const PowExpression node)
	{
		markUsedVariables(node);
	}

	override void visitStart(const RelExpression node)
	{
		markUsedVariables(node);
	}

	override void visitStart(const ShiftExpression node)
	{
		markUsedVariables(node);
	}

	override void visitStart(const SingleImport node)
	{
		declareImport(node);
	}

	override void visitStart(const StructDeclaration node)
	{
		gScope.parentsPush(IdentifierType.struct_);
		gScope.thisPointersPush(node.name.text);

		declareStruct(node);
	}

	override void visitEnd(const StructDeclaration node)
	{
		gScope.thisPointersPop();
		gScope.parentsPop();
	}

	override void visitStart(const TemplateParameters node)
	{
		declareTemplates(node);
	}

	override void visitStart(const VariableDeclaration node)
	{
		// Only declare if NOT a struct/class field
		if (gScope.parentsPeak() != IdentifierType.struct_ && gScope.parentsPeak() != IdentifierType.class_)
		{
			declareVariable(node);
		}
	}

	override void visitStart(const XorExpression node)
	{
		markUsedVariables(node);
	}
}

