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
import analysis.ast_helpers;
import analysis.manager;
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
	ScopeManager scopeManager;

	this(string fileName)
	{
		super(fileName);
		scopeManager = new ScopeManager();
	}

	override void visitStart(const AddExpression node)
	{
		scopeManager.markUsedVariables(node);
	}

	override void visitStart(const AndAndExpression node)
	{
		scopeManager.markUsedVariables(node);
	}

	override void visitStart(const AndExpression node)
	{
		scopeManager.markUsedVariables(node);
	}

	override void visitStart(const AssertExpression node)
	{
		scopeManager.markUsedVariables(node);
	}

	override void visitStart(const AssignExpression node)
	{
		scopeManager.markUsedVariables(node);
	}

	override void visitStart(const BlockStatement node)
	{
		scopeManager.pushFrame();
	}

	override void visitEnd(const BlockStatement node)
	{
		scopeManager.popFrame();
	}

	override void visitStart(const ClassDeclaration node)
	{
		scopeManager.parentsPush(IdentifierType.class_);
		scopeManager.thisPointersPush(node.name.text);

		scopeManager.declareClass(node);
	}

	override void visitEnd(const ClassDeclaration node)
	{
		scopeManager.thisPointersPop();
		scopeManager.parentsPop();
	}

	override void visitStart(const CmpExpression node)
	{
		scopeManager.markUsedVariables(node);
	}

	override void visitStart(const Declaration node)
	{
		// Add decorations such as properties, auto, ref, et cetera
		Decoration decoration = getDeclarationDecorations(scopeManager.scope_, node);
		scopeManager.decorationsPush(decoration);
	}

	override void visitEnd(const Declaration node)
	{
		scopeManager.decorationsPop();
	}

	override void visitStart(const EnumDeclaration node)
	{
		scopeManager.parentsPush(IdentifierType.enum_);
		scopeManager.declareEnum(node);
	}

	override void visitEnd(const EnumDeclaration node)
	{
		scopeManager.parentsPop();
	}

	override void visitStart(const EqualExpression node)
	{
		scopeManager.markUsedVariables(node);
	}

	override void visitStart(const FunctionDeclaration node)
	{
		// Only declare if NOT a struct/class method
		IdentifierType identifierType;
		if (scopeManager.parentsPeak() != IdentifierType.struct_ && scopeManager.parentsPeak() != IdentifierType.class_)
		{
			scopeManager.declareFunction(node);
			identifierType = IdentifierType.function_;
		}
		else
		{
			identifierType = IdentifierType.method_;
		}

		scopeManager.parentsPush(identifierType);
		scopeManager.pushFrame();
	}

	override void visitEnd(const FunctionDeclaration node)
	{
		scopeManager.popFrame();
		scopeManager.parentsPop();
	}

	override void visitStart(const IdentityExpression node)
	{
		scopeManager.markUsedVariables(node);
	}

	override void visitStart(const InExpression node)
	{
		scopeManager.markUsedVariables(node);
	}

	override void visitStart(const Module node)
	{
		scopeManager.pushFrame();

		scopeManager.parentsPush(IdentifierType.module_);
		scopeManager.declareModule(node);

		// Declare the global functions and variables
		// This is a special case, because global functions do NOT have to 
		// be declared in order.
		foreach (decl; node.declarations)
		{
			if (!decl) continue;

			// Add decorations such as properties, auto, ref, et cetera
			Decoration decoration = getDeclarationDecorations(scopeManager.scope_, decl);
			scopeManager.decorationsPush(decoration);

			// Add the import
			if (decl.importDeclaration && decl.importDeclaration.singleImports)
			{
				foreach (singleImport; decl.importDeclaration.singleImports)
				{
					scopeManager.declareImport(singleImport);
				}
			}
			// Declare the function
			else if (decl.functionDeclaration)
			{
				scopeManager.declareFunction(decl.functionDeclaration);
			}
			// Declare the variable
			else if (decl.variableDeclaration)
			{
				scopeManager.declareVariable(decl.variableDeclaration);
			}

			// Remove decorations
			scopeManager.decorationsPop();
		}
	}

	override void visitEnd(const Module node)
	{
		scopeManager.parentsPop();
		scopeManager.popFrame();
	}

	override void visitStart(const MulExpression node)
	{
		scopeManager.markUsedVariables(node);
	}

	override void visitStart(const OrOrExpression node)
	{
		scopeManager.markUsedVariables(node);
	}

	override void visitStart(const OrExpression node)
	{
		scopeManager.markUsedVariables(node);
	}

	override void visitStart(const Parameter node)
	{
		scopeManager.declareParameter(node);
	}

	override void visitStart(const PowExpression node)
	{
		scopeManager.markUsedVariables(node);
	}

	override void visitStart(const RelExpression node)
	{
		scopeManager.markUsedVariables(node);
	}

	override void visitStart(const ShiftExpression node)
	{
		scopeManager.markUsedVariables(node);
	}

	override void visitStart(const SingleImport node)
	{
		scopeManager.declareImport(node);
	}

	override void visitStart(const StructDeclaration node)
	{
		scopeManager.parentsPush(IdentifierType.struct_);
		scopeManager.thisPointersPush(node.name.text);

		scopeManager.declareStruct(node);
	}

	override void visitEnd(const StructDeclaration node)
	{
		scopeManager.thisPointersPop();
		scopeManager.parentsPop();
	}

	override void visitStart(const TemplateParameters node)
	{
		scopeManager.declareTemplates(node);
	}

	override void visitStart(const VariableDeclaration node)
	{
		// Only declare if NOT a struct/class field
		if (scopeManager.parentsPeak() != IdentifierType.struct_ && scopeManager.parentsPeak() != IdentifierType.class_)
		{
			scopeManager.declareVariable(node);
		}
	}

	override void visitStart(const XorExpression node)
	{
		scopeManager.markUsedVariables(node);
	}
}

