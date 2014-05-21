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
class ScopeAnalyzer : BaseAnalyzer {
	alias visit = BaseAnalyzer.visit;

	private bool _log_info = false;
	private bool prev_log = false;

	this(string fileName, bool log_info) {
		super(fileName);
		_log_info = log_info;
	}

	override void visitStart(const AddExpression node) {
		markUsedVariables(node);
	}

	override void visitStart(const AndAndExpression node) {
		markUsedVariables(node);
	}

	override void visitStart(const AndExpression node) {
		markUsedVariables(node);
	}

	override void visitStart(const AssertExpression node) {
		markUsedVariables(node);
	}

	override void visitStart(const AssignExpression node) {
		markUsedVariables(node);
	}

	override void visitStart(const BlockStatement node) {
		info("start frame by %s", typeid(node));
		scopeFrameStart();
	}

	override void visitEnd(const BlockStatement node) {
		info("exit frame by %s", typeid(node));
		scopeFrameExit();
	}

	override void visitStart(const ClassDeclaration node) {
		parents.push(IdentifierType.class_);
		thisPointers.push(node.name.text);

		declareClass(node);
	}

	override void visitEnd(const ClassDeclaration node) {
		thisPointers.pop();
		parents.pop();
	}

	override void visitStart(const CmpExpression node) {
		markUsedVariables(node);
	}

	override void visitStart(const Declaration node) {
		// Add decorations such as properties, auto, ref, et cetera
		Decoration decoration = getDeclarationDecorations(node);
		decorations.push(decoration);
	}

	override void visitEnd(const Declaration node) {
		decorations.pop();
	}

	override void visitStart(const EnumDeclaration node) {
		parents.push(IdentifierType.enum_);
		declareEnum(node);
	}

	override void visitEnd(const EnumDeclaration node) {
		parents.pop();
	}

	override void visitStart(const EqualExpression node) {
		markUsedVariables(node);
	}

	override void visitStart(const FunctionDeclaration node) {
		// Only declare if NOT a struct/class method
		if (parents.peak != IdentifierType.struct_ && parents.peak != IdentifierType.class_) {
			declareFunction(node);
		}

		parents.push(IdentifierType.function_);
		scopeFrameStart();
	}

	override void visitEnd(const FunctionDeclaration node) {
		scopeFrameExit();
		parents.pop();
	}

	override void visitStart(const IdentityExpression node) {
		markUsedVariables(node);
	}

	override void visitStart(const InExpression node) {
		markUsedVariables(node);
	}

	override void visitStart(const Module node) {
		// Turn logging on if desired
		prev_log = g_log_info;
		g_log_info = _log_info;

		info("start frame by %s", typeid(node));
		scopeFrameStart();

		parents.push(IdentifierType.module_);
		declareModule(node);

		// Declare the global functions and variables
		// This is a special case, because global functions do NOT have to 
		// be declared in order.
		foreach (decl; node.declarations) {
			if (!decl) continue;

			// Add decorations such as properties, auto, ref, et cetera
			Decoration decoration = getDeclarationDecorations(decl);
			decorations.push(decoration);

			// Add the import
			if (decl.importDeclaration && decl.importDeclaration.singleImports) {
				foreach (singleImport; decl.importDeclaration.singleImports) {
					declareImport(singleImport);
				}
			// Declare the function
			} else if (decl.functionDeclaration) {
				declareFunction(decl.functionDeclaration);
			// Declare the variable
			} else if (decl.variableDeclaration) {
				declareVariable(decl.variableDeclaration);
			}

			// Remove decorations
			decorations.pop();
		}
	}

	override void visitEnd(const Module node) {
		parents.pop();

		info("exit frame by %s", typeid(node));
		scopeFrameExit();

		// Return to previous logging mode
		g_log_info = prev_log;
	}

	override void visitStart(const MulExpression node) {
		markUsedVariables(node);
	}

	override void visitStart(const OrOrExpression node) {
		markUsedVariables(node);
	}

	override void visitStart(const OrExpression node) {
		markUsedVariables(node);
	}

	override void visitStart(const Parameter node) {
		declareParameter(node);
	}

	override void visitStart(const PowExpression node) {
		markUsedVariables(node);
	}

	override void visitStart(const RelExpression node) {
		markUsedVariables(node);
	}

	override void visitStart(const ShiftExpression node) {
		markUsedVariables(node);
	}

	override void visitStart(const SingleImport node) {
		declareImport(node);
	}

	override void visitStart(const StructDeclaration node) {
		parents.push(IdentifierType.struct_);
		thisPointers.push(node.name.text);

		declareStruct(node);
	}

	override void visitEnd(const StructDeclaration node) {
		thisPointers.pop();
		parents.pop();
	}

	override void visitStart(const TemplateParameters node) {
		declareTemplates(node);
	}

	override void visitStart(const VariableDeclaration node) {
		// Only declare if NOT a struct/class field
		if (parents.peak != IdentifierType.struct_ && parents.peak != IdentifierType.class_) {
			declareVariable(node);
		}
	}

	override void visitStart(const XorExpression node) {
		markUsedVariables(node);
	}
}

