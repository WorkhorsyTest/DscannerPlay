// Copyright (c) 2014, Matthew Brennan Jones <matthew.brennan.jones@gmail.com>
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.walking_analyzer;

import std.stdio;
import std.string;
import std.stdint;

import std.d.ast;
import std.d.codegen;
import std.d.lexer;
import analysis.base;
import analysis.helpers;
import analysis.stack_frame;
import dlang_helper;


// FIXME: We need a way to have separate visit events so that child classes 
// don't override these visit events.
/*
This works like the BaseAnalyzer but tracks all the variables, classes, 
functions, et cetera, and know what is in scope and hold meta data 
about everything.
*/
class BaseWalkingAnalyzer : BaseAnalyzer {
	alias visit = BaseAnalyzer.visit;

	private bool _log_info = false;

	this(string fileName, bool log_info) {
		super(fileName);
		_log_info = log_info;
	}

	override void visit(const AddExpression node) {
		mark_used_variables(node);
		node.accept(this);
	}

	override void visit(const AndAndExpression node) {
		mark_used_variables(node);
		node.accept(this);
	}

	override void visit(const AndExpression node) {
		mark_used_variables(node);
		node.accept(this);
	}

	override void visit(const AssertExpression node) {
		mark_used_variables(node);
		node.accept(this);
	}

	override void visit(const AssignExpression node) {
		mark_used_variables(node);
		node.accept(this);
	}

	override void visit(const BlockStatement node) {
		info("start frame by %s", typeid(node));
		stack_frame_start();

		node.accept(this);

		info("exit frame by %s", typeid(node));
		stack_frame_exit();
	}

	override void visit(const ClassDeclaration node) {
		parents.push(IdentifierType.class_);
		this_pointers.push(node.name.text);

		declare_class(node);

		node.accept(this);

		this_pointers.pop();
		parents.pop();
	}

	override void visit(const CmpExpression node) {
		mark_used_variables(node);
		node.accept(this);
	}

	override void visit(const Declaration node) {
		// Add decorations such as properties, auto, ref, et cetera
		Decoration decoration = get_declaration_decorations(node);
		decorations.push(decoration);

		node.accept(this);

		decorations.pop();
	}

	override void visit(const EnumDeclaration node) {
		parents.push(IdentifierType.enum_);

		declare_enum(node);

		node.accept(this);

		parents.pop();
	}

	override void visit(const EqualExpression node) {
		mark_used_variables(node);
		node.accept(this);
	}

	override void visit(const FunctionDeclaration node) {
		// Only declare if NOT a struct/class method
		if(parents.peak != IdentifierType.struct_ && parents.peak != IdentifierType.class_) {
			declare_function(node);
		}

		parents.push(IdentifierType.function_);
		stack_frame_start();

		node.accept(this);

		stack_frame_exit();
		parents.pop();
	}

	override void visit(const IdentityExpression node) {
		mark_used_variables(node);
		node.accept(this);
	}

	override void visit(const InExpression node) {
		mark_used_variables(node);
		node.accept(this);
	}

	override void visit(const Module node) {
		// Turn logging on if desired
		bool prev_log = g_log_info;
		g_log_info = _log_info;

		info("start frame by %s", typeid(node));
		stack_frame_start();

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

		// Walk through the declarations normally
		node.accept(this);

		parents.pop();

		info("exit frame by %s", typeid(node));
		stack_frame_exit();

		// Return to previous logging mode
		g_log_info = prev_log;
	}

	override void visit(const MulExpression node) {
		mark_used_variables(node);
		node.accept(this);
	}

	override void visit(const OrOrExpression node) {
		mark_used_variables(node);
		node.accept(this);
	}

	override void visit(const OrExpression node) {
		mark_used_variables(node);
		node.accept(this);
	}

	override void visit(const Parameter node) {
		declare_parameter(node);

		node.accept(this);
	}

	override void visit(const PowExpression node) {
		mark_used_variables(node);
		node.accept(this);
	}

	override void visit(const RelExpression node) {
		mark_used_variables(node);
		node.accept(this);
	}

	override void visit(const ShiftExpression node) {
		mark_used_variables(node);
		node.accept(this);
	}

	override void visit(const SingleImport node) {
		declare_import(node);
		node.accept(this);
	}

	override void visit(const StructDeclaration node) {
		parents.push(IdentifierType.struct_);
		this_pointers.push(node.name.text);

		declare_struct(node);

		node.accept(this);

		this_pointers.pop();
		parents.pop();
	}

	override void visit(const TemplateParameters node) {
		declare_templates(node);
		node.accept(this);
	}

	override void visit(const VariableDeclaration node) {
		// Only declare if NOT a struct/class field
		if(parents.peak != IdentifierType.struct_ && parents.peak != IdentifierType.class_) {
			declare_variable(node);
		}

		node.accept(this);
	}

	override void visit(const XorExpression node) {
		mark_used_variables(node);
		node.accept(this);
	}
}

