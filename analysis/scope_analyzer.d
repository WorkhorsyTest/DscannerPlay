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
		mark_used_variables(node);
	}

	override void visitStart(const AndAndExpression node) {
		mark_used_variables(node);
	}

	override void visitStart(const AndExpression node) {
		mark_used_variables(node);
	}

	override void visitStart(const AssertExpression node) {
		mark_used_variables(node);
	}

	override void visitStart(const AssignExpression node) {
		mark_used_variables(node);
	}

	override void visitStart(const BlockStatement node) {
		info("start frame by %s", typeid(node));
		scope_frame_start();
	}

	override void visitEnd(const BlockStatement node) {
		info("exit frame by %s", typeid(node));
		scope_frame_exit();
	}

	override void visitStart(const ClassDeclaration node) {
		parents.push(IdentifierType.class_);
		this_pointers.push(node.name.text);

		declare_class(node);
	}

	override void visitEnd(const ClassDeclaration node) {
		this_pointers.pop();
		parents.pop();
	}

	override void visitStart(const CmpExpression node) {
		mark_used_variables(node);
	}

	override void visitStart(const Declaration node) {
		// Add decorations such as properties, auto, ref, et cetera
		Decoration decoration = get_declaration_decorations(node);
		decorations.push(decoration);
	}

	override void visitEnd(const Declaration node) {
		decorations.pop();
	}

	override void visitStart(const EnumDeclaration node) {
		parents.push(IdentifierType.enum_);
		declare_enum(node);
	}

	override void visitEnd(const EnumDeclaration node) {
		parents.pop();
	}

	override void visitStart(const EqualExpression node) {
		mark_used_variables(node);
	}

	override void visitStart(const FunctionDeclaration node) {
		// Only declare if NOT a struct/class method
		if(parents.peak != IdentifierType.struct_ && parents.peak != IdentifierType.class_) {
			declare_function(node);
		}

		parents.push(IdentifierType.function_);
		scope_frame_start();
	}

	override void visitEnd(const FunctionDeclaration node) {
		scope_frame_exit();
		parents.pop();
	}

	override void visitStart(const IdentityExpression node) {
		mark_used_variables(node);
	}

	override void visitStart(const InExpression node) {
		mark_used_variables(node);
	}

	override void visitStart(const Module node) {
		// Turn logging on if desired
		prev_log = g_log_info;
		g_log_info = _log_info;

		info("start frame by %s", typeid(node));
		scope_frame_start();

		parents.push(IdentifierType.module_);
		declare_module(node);

		// Declare the global functions and variables
		// This is a special case, because global functions do NOT have to 
		// be declared in order.
		foreach(decl; node.declarations) {
			if(!decl) continue;

			// Add decorations such as properties, auto, ref, et cetera
			Decoration decoration = get_declaration_decorations(decl);
			decorations.push(decoration);

			// Add the import
			if(decl.importDeclaration && decl.importDeclaration.singleImports) {
				foreach(singleImport; decl.importDeclaration.singleImports) {
					declare_import(singleImport);
				}
			// Declare the function
			} else if(decl.functionDeclaration) {
				declare_function(decl.functionDeclaration);
			// Declare the variable
			} else if(decl.variableDeclaration) {
				declare_variable(decl.variableDeclaration);
			}

			// Remove decorations
			decorations.pop();
		}
	}

	override void visitEnd(const Module node) {
		parents.pop();

		info("exit frame by %s", typeid(node));
		scope_frame_exit();

		// Return to previous logging mode
		g_log_info = prev_log;
	}

	override void visitStart(const MulExpression node) {
		mark_used_variables(node);
	}

	override void visitStart(const OrOrExpression node) {
		mark_used_variables(node);
	}

	override void visitStart(const OrExpression node) {
		mark_used_variables(node);
	}

	override void visitStart(const Parameter node) {
		declare_parameter(node);
	}

	override void visitStart(const PowExpression node) {
		mark_used_variables(node);
	}

	override void visitStart(const RelExpression node) {
		mark_used_variables(node);
	}

	override void visitStart(const ShiftExpression node) {
		mark_used_variables(node);
	}

	override void visitStart(const SingleImport node) {
		declare_import(node);
	}

	override void visitStart(const StructDeclaration node) {
		parents.push(IdentifierType.struct_);
		this_pointers.push(node.name.text);

		declare_struct(node);
	}

	override void visitEnd(const StructDeclaration node) {
		this_pointers.pop();
		parents.pop();
	}

	override void visitStart(const TemplateParameters node) {
		declare_templates(node);
	}

	override void visitStart(const VariableDeclaration node) {
		// Only declare if NOT a struct/class field
		if(parents.peak != IdentifierType.struct_ && parents.peak != IdentifierType.class_) {
			declare_variable(node);
		}
	}

	override void visitStart(const XorExpression node) {
		mark_used_variables(node);
	}
}

