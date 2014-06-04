// Copyright (c) 2014, Matthew Brennan Jones <matthew.brennan.jones@gmail.com>
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.check_name_clash;

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
 * Checks for name clashes in classes, structs, variables, parameters, enums, and members.
 */
class NameClashCheck : ScopeAnalyzer
{
	alias visit = ScopeAnalyzer.visit;

	this(string fileName)
	{
		super(fileName);
	}

	override void visit(const VariableDeclaration node)
	{
		// Variable or field?
		IdentifierType identifierType = IdentifierType.variable_;
		if (this.scopeManager.parentsPeak() == IdentifierType.class_ || this.scopeManager.parentsPeak() == IdentifierType.struct_)
			identifierType = IdentifierType.field_;

		// Get the data
		foreach (varData; this.scopeManager.getVariableDatas(node))
		{
			// Check to see if the name is already used
			if (varData !is VariableData.init)
				checkClashes(varData.name, varData.line, varData.column, identifierType);
		}

		node.accept(this);
	}

	override void visit(const Parameter node)
	{
		// Just return if can't get the name
		if (node.name is Token.init || node.name.text is null || node.name.text == "")
		{
			node.accept(this);
			return;
		}

		// Get the data
		VariableData paramData;
		paramData.name = node.name.text;
		paramData.type = this.scopeManager.getTypeData(node.type);
		paramData.isParameter = true;
		paramData.line = node.name.line;
		paramData.column = node.name.column;

		// Check to see if the name is already used
		checkClashes(paramData.name, paramData.line, paramData.column, IdentifierType.parameter_);

		node.accept(this);
	}

	override void visit(const ClassDeclaration node)
	{
		// Get the data
		auto classData = this.scopeManager.getClassData(node);

		// Check to see if the name is already used
		if (classData !is ClassData.init)
			checkClashes(classData.name, classData.line, classData.column, IdentifierType.class_);

		node.accept(this);
	}

	override void visit(const StructDeclaration node)
	{
		// Get the data
		auto structData = this.scopeManager.getStructData(node);

		// Check to see if the name is already used
		if (structData !is StructData.init)
			checkClashes(structData.name, structData.line, structData.column, IdentifierType.struct_);

		node.accept(this);
	}

	override void visit(const FunctionDeclaration node)
	{
		// Get the data
		auto funcData = this.scopeManager.getFunctionData(node);

		// Function or method?
		// FIXME: The problem is that it is already marked that the parent is the function or 
		// method we are in. So the parent is actually this, rather than a struct/class.
		IdentifierType identifierType = this.scopeManager.parentsPeak();

		// Check to see if the name is already used
		if (funcData !is FunctionData.init)
			checkClashes(funcData.name, funcData.line, funcData.column, identifierType);

		node.accept(this);
	}

	override void visit(const TemplateParameters node)
	{
		// Get the data
		foreach (tempData; this.scopeManager.getTemplateDatas(node))
		{
			// Check to see if the name is already used
			if (tempData !is TemplateData.init)
				checkClashes(tempData.name, tempData.line, tempData.column, IdentifierType.template_);
		}

		node.accept(this);
	}

	override void visit(const EnumDeclaration node)
	{
		// Get the data
		auto enumData = this.scopeManager.getEnumData(node);

		// Check to see if the name is already used
		if (enumData !is EnumData.init)
			checkClashes(enumData.name, enumData.line, enumData.column, IdentifierType.enum_);

		node.accept(this);
	}

	override void visit(const EnumMember node)
	{
		// Get the data
		string name;
		auto fieldData = this.scopeManager.getEnumFieldData(node, name);

		// Check to see if the name is already used
		if (fieldData !is FieldData.init)
			checkClashes(name, fieldData.line, fieldData.column, IdentifierType.field_);

		node.accept(this);
	}

	void checkClashes(string name, size_t line, size_t column, IdentifierType type)
	{
//		writefln("!!! checkClashes() name:%s, line:%s, column:%s, type:%s", name, line, column, type);

		size_t oldLine;
		size_t oldColumn;
		IdentifierType oldType = IdentifierType.invalid_;

		foreach (frame; std.range.retro(this.scopeManager.scope_.frames))
		{
			checkInstanceClashes(frame.variables, IdentifierType.variable_, oldLine, oldColumn, oldType, name, line, column, type);
			checkInstanceClashes(frame.functions, IdentifierType.function_, oldLine, oldColumn, oldType, name, line, column, type);
			checkInstanceClashes(frame.structs, IdentifierType.struct_, oldLine, oldColumn, oldType, name, line, column, type);
			checkInstanceClashes(frame.classes, IdentifierType.class_, oldLine, oldColumn, oldType, name, line, column, type);
			checkInstanceClashes(frame.enums, IdentifierType.enum_, oldLine, oldColumn, oldType, name, line, column, type);

			checkFieldClashes(frame.enums, oldLine, oldColumn, oldType, name, line, column, type);
			// FIXME: Make these work too
			version (none)
			{
			checkFieldClashes(frame.structs, oldLine, oldColumn, oldType, name, line, column, type);
			checkFieldClashes(frame.classes, oldLine, oldColumn, oldType, name, line, column, type);
			checkMethodClashes(frame.structs, oldLine, oldColumn, oldType, name, line, column, type);
			checkMethodClashes(frame.classes, oldLine, oldColumn, oldType, name, line, column, type);
			}
		}

		// Match exact module variable name in other module
		foreach (mod; this.scopeManager.scope_.modules)
		{
			if (name.startsWith(mod.name) && name.length > mod.name.length)
			{
				auto offset = mod.name.length + 1;
				string offsetName = name[offset .. $];

				checkInstanceClashes(mod.variables, IdentifierType.variable_, oldLine, oldColumn, oldType, offsetName, line, column, type);
				checkInstanceClashes(mod.functions, IdentifierType.function_, oldLine, oldColumn, oldType, offsetName, line, column, type);
				checkInstanceClashes(mod.structs, IdentifierType.struct_, oldLine, oldColumn, oldType, offsetName, line, column, type);
				checkInstanceClashes(mod.classes, IdentifierType.class_, oldLine, oldColumn, oldType, offsetName, line, column, type);
				checkInstanceClashes(mod.enums, IdentifierType.enum_, oldLine, oldColumn, oldType, offsetName, line, column, type);

				checkFieldClashes(mod.enums, oldLine, oldColumn, oldType, offsetName, line, column, type);
				// FIXME: Make these work too
				version (none)
				{
				checkFieldClashes(mod.structs, oldLine, oldColumn, oldType, offsetName, line, column, type);
				checkFieldClashes(mod.classes, oldLine, oldColumn, oldType, offsetName, line, column, type);
				checkMethodClashes(mod.structs, oldLine, oldColumn, oldType, offsetName, line, column, type);
				checkMethodClashes(mod.classes, oldLine, oldColumn, oldType, offsetName, line, column, type);
				}
			}
		}

		// Match partial module variable name using imports
		foreach (frame; std.range.retro(this.scopeManager.scope_.frames))
		{
			foreach (importName; frame.imports)
			{
				if (importName in this.scopeManager.scope_.modules)
				{
					auto mod = this.scopeManager.scope_.modules[importName];

					checkInstanceClashes(mod.variables, IdentifierType.variable_, oldLine, oldColumn, oldType, name, line, column, type);
					checkInstanceClashes(mod.functions, IdentifierType.function_, oldLine, oldColumn, oldType, name, line, column, type);
					checkInstanceClashes(mod.structs, IdentifierType.struct_, oldLine, oldColumn, oldType, name, line, column, type);
					checkInstanceClashes(mod.classes, IdentifierType.class_, oldLine, oldColumn, oldType, name, line, column, type);
					checkInstanceClashes(mod.enums, IdentifierType.enum_, oldLine, oldColumn, oldType, name, line, column, type);

					checkFieldClashes(mod.enums, oldLine, oldColumn, oldType, name, line, column, type);
					// FIXME: Make these work too
					version (none)
					{
					checkFieldClashes(mod.structs, oldLine, oldColumn, oldType, name, line, column, type);
					checkFieldClashes(mod.classes, oldLine, oldColumn, oldType, name, line, column, type);
					checkMethodClashes(mod.structs, oldLine, oldColumn, oldType, name, line, column, type);
					checkMethodClashes(mod.classes, oldLine, oldColumn, oldType, name, line, column, type);
					}
				}
			}
		}

		if (oldType == IdentifierType.invalid_)
			return;

		// Print the warning
		string message = "The %s '%s' clashes with the %s at (%s:%s).".format(
			cast(string) type,
			name,
			cast(string) oldType,
			oldLine,
			oldColumn
		);
		addErrorMessage(line, column, message);
	}

	private void checkInstanceClashes(T)(T datas, IdentifierType matchType,
		ref size_t oldLine, ref size_t oldColumn, ref IdentifierType oldType,
		string name, size_t line, size_t column, IdentifierType type)
	{
		if (name in datas)
		{
			auto data = datas[name];
			if (line > data.line || (line == data.line && column > data.column))
			{
				oldLine = data.line;
				oldColumn = data.column;
				oldType = matchType;
			}
		}
	}

	private void checkMethodClashes(T)(T datas,
		ref size_t oldLine, ref size_t oldColumn, ref IdentifierType oldType,
		string name, size_t line, size_t column, IdentifierType type)
	{
		foreach (thingName, thingData; datas)
		{
			foreach (methodName, data; thingData.methods)
			{
				if (methodName == name && line > data.line || (line == data.line && column > data.column))
				{
					oldLine = data.line;
					oldColumn = data.column;
					oldType = IdentifierType.method_;
				}
			}
		}
	}

	private void checkFieldClashes(T)(T datas,
		ref size_t oldLine, ref size_t oldColumn, ref IdentifierType oldType,
		string name, size_t line, size_t column, IdentifierType type)
	{
		foreach (thingName, thingData; datas)
		{
			foreach (fieldName, data; thingData.fields)
			{
				if (fieldName == name && line > data.line || (line == data.line && column > data.column))
				{
					oldLine = data.line;
					oldColumn = data.column;
					oldType = IdentifierType.field_;
				}
			}
		}
	}
}

// FIXME: Make this so it does not need to have the line & column number in the comment
unittest
{
	// FIXME: Make it work with class/struct static fields and methods.
	// FIXME: Make it work with functions, and delegates too.
	assertAnalyzerWarnings(q{
		// Variable vs nested variable
		string sound = "quack";
		void talk() {
			string sound = "oink"; // [warn]: The variable 'sound' clashes with the variable at (3:10).
		}

		// Function vs nested variable
		void name() {
			string name; // [warn]: The variable 'name' clashes with the function at (9:8).
		}

		// Variable vs function parameter
		int doughnut;
		void eat(int doughnut) { // [warn]: The parameter 'doughnut' clashes with the variable at (14:7).
		}

		// Function vs function parameter
		void cake(int cake) { // [warn]: The parameter 'cake' clashes with the function at (19:8).
		}

		// Function vs nested function
		void pie() {
			void pie() { // [warn]: The function 'pie' clashes with the function at (23:8).
			}
		}
		// Function vs nested struct
		void monkey() {
			struct monkey { // [warn]: The struct 'monkey' clashes with the function at (28:8).
			}
		}

		// Struct vs member variable vs nested variable
		struct dog {
			int dog; // [warn]: The field 'dog' clashes with the struct at (34:10).
			void bark() {
				int dog; // [warn]: The variable 'dog' clashes with the struct at (34:10).
			}
		}

		// Struct vs member method vs nested variable
		struct cat {
			void cat() { // [warn]: The method 'cat' clashes with the struct at (42:10).
				int cat; // [warn]: The variable 'cat' clashes with the struct at (42:10).
			}
		}

		// Struct member variable vs variable vs nested variable
		int weight;
		struct bird {
			int weight; // [warn]: The field 'weight' clashes with the variable at (49:7).
			void fly() {
				int weight; // [warn]: The variable 'weight' clashes with the variable at (49:7).
			}
		}

		// Class member variable vs variable
		string flavor;
		class pig {
			string flavor; // [warn]: The field 'flavor' clashes with the variable at (58:10).
			void oink() {
				string pig = "honey baked ham"; // [warn]: The variable 'pig' clashes with the class at (59:9).
				this.flavor = "canadian bacon";
			}
		}

		// Enum vs variable
		int coffee;
		void drinks() {
			enum coffee { // [warn]: The enum 'coffee' clashes with the variable at (68:7).
				coffee // [warn]: The field 'coffee' clashes with the variable at (68:7).
			}
		}

		// Variable vs enum
		enum teas {
			chai
		}
		int chai; // [warn]: The variable 'chai' clashes with the field at (77:4).
	}c, analysis.run.AnalyzerCheck.name_clash_check);

	stderr.writeln("Unittest for NameClashCheck passed.");
}

