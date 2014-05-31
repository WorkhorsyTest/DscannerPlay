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

struct Position
{
	size_t line;
	size_t column;
	IdentifierType type;

	string typeName()
	{
		return cast(string) this.type;
	}
}

/**
 * Checks for name clashes in classes, structs, variables, parameters, enums, and members.
 */
class NameClashCheck : ScopeAnalyzer
{
	alias visit = ScopeAnalyzer.visit;

	Position[][string] nameClashes;

	this(string fileName)
	{
		super(fileName, false);
	}

	override void visit(const VariableDeclaration node)
	{
		// Variable or field?
		IdentifierType identifierType = IdentifierType.variable_;
		if (gScope.parentsPeak() == IdentifierType.class_ || gScope.parentsPeak() == IdentifierType.struct_)
		{
			identifierType = IdentifierType.field_;
		}

		foreach (varData; getVariableDatas(node))
		{
			if (varData is VariableData.init)
				continue;

			// Check to see if the name is already used
			checkNameClashes(varData.name, varData.line, varData.column, identifierType);
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

		VariableData paramData;
		paramData.name = node.name.text;
		paramData.type = getTypeData(node.type);
		paramData.isParameter = true;
		paramData.line = node.name.line;
		paramData.column = node.name.column;

		// Check to see if the name is already used
		checkNameClashes(paramData.name, paramData.line, paramData.column, IdentifierType.parameter_);

		node.accept(this);
	}

	override void visit(const ClassDeclaration node)
	{
		auto classData = getClassData(node);
		if (classData is ClassData.init)
			return;

		// Check to see if the name is already used
		checkNameClashes(classData.name, classData.line, classData.column, IdentifierType.class_);

		node.accept(this);
	}

	override void visit(const StructDeclaration node)
	{
		auto structData = getStructData(node);
		if (structData is StructData.init)
			return;

		// Check to see if the name is already used
		checkNameClashes(structData.name, structData.line, structData.column, IdentifierType.struct_);

		node.accept(this);
	}

	override void visit(const FunctionDeclaration node)
	{
		auto funcData = getFunctionData(node);
		if (funcData is FunctionData.init)
			return;

		// Function or method?
		// FIXME: The problem is that it is already marked that the parent is the function or 
		// method we are in. So the parent is actually this, rather than a struct/class.
		IdentifierType identifierType = gScope.parentsPeak();

		// Check to see if the name is already used
		checkNameClashes(funcData.name, funcData.line, funcData.column, identifierType);

		node.accept(this);
	}

	override void visit(const TemplateParameters node)
	{
		foreach (tempData; getTemplateDatas(node))
		{
			if (tempData is TemplateData.init)
				continue;

			// Check to see if the name is already used
			auto idenType = IdentifierType.template_;
			checkNameClashes(tempData.name, tempData.line, tempData.column, idenType);
		}

		node.accept(this);
	}

	override void visit(const EnumDeclaration node)
	{
		auto enumData = getEnumData(node);
		if (enumData is EnumData.init)
			return;

		// Check to see if the name is already used
		checkNameClashes(enumData.name, enumData.line, enumData.column, IdentifierType.enum_);

		// Check to see if the field names are already used
		foreach (fieldName, field; enumData.fields)
		{
			checkNameClashes(fieldName, field.line, field.column, IdentifierType.field_);
		}

		node.accept(this);
	}

	// FIXME: Make it so it instead of storing the errors and printing them here
	// It just prints them when they happen.
	override void visit(const Module node)
	{
		node.accept(this);

		foreach (name, positions; this.nameClashes)
		{
			// Skip if there are less than two
			if (positions.length < 2)
				continue;

			// Get the original position
			auto orig = positions[0];

			// Add an error for each clashing name
			foreach (pos; positions[1 .. $])
			{
				string message = "The %s '%s' clashes with the %s at (%s:%s).".format(
					pos.typeName(),
					name,
					orig.typeName(),
					orig.line,
					orig.column
				);
				addErrorMessage(pos.line, pos.column, message);
			}
		}
	}

	void checkNameClashes(string name, size_t line, size_t column, IdentifierType type)
	{
		VariableData varData;
		FunctionData funcData;
		StructData structData;
		ClassData classData;
		EnumData enumData;
		TemplateData templateData;
		gScope.getIdentifier(name, varData, funcData, structData, classData, enumData, templateData);

		size_t oldLine, oldColumn;
		IdentifierType oldType;

		// A variable has that name
		if (varData !is VariableData.init)
		{
			oldLine = varData.line;
			oldColumn = varData.column;
			oldType = IdentifierType.variable_;
		}
		// A function has that name
		else if (funcData !is FunctionData.init)
		{
			oldLine = funcData.line;
			oldColumn = funcData.column;
			oldType = IdentifierType.function_;
		}
		// A struct has that name
		else if (structData !is StructData.init)
		{
			oldLine = structData.line;
			oldColumn = structData.column;
			oldType = IdentifierType.struct_;
		}
		// A class has that name
		else if (classData !is ClassData.init)
		{
			oldLine = classData.line;
			oldColumn = classData.column;
			oldType = IdentifierType.class_;
		}
		// An enum has that name
		else if (enumData !is EnumData.init)
		{
			oldLine = enumData.line;
			oldColumn = enumData.column;
			oldType = IdentifierType.enum_;
		}

		// Check struct fields and methods
		if (oldType == IdentifierType.invalid_)
		{
			// Each scope frame
			foreach (frame; gScope.frames)
			{
				// Each struct
				foreach (structName, structData; frame.structs)
				{
					// Each field
					foreach (fieldName, fieldData; structData.fields)
					{
						if (fieldName == name)
						{
							oldLine = fieldData.line;
							oldColumn = fieldData.column;
							oldType = IdentifierType.field_;
						}
					}
					// Each method
					foreach (methodName, methodData; structData.methods)
					{
						if (methodName == name)
						{
							oldLine = methodData.line;
							oldColumn = methodData.column;
							oldType = IdentifierType.method_;
						}
					}
				}
			}
		}

		// Check class fields and methods
		if (oldType == IdentifierType.invalid_)
		{
			// Each scope frame
			foreach (frame; gScope.frames)
			{
				// Each class
				foreach (className, classData; frame.classes)
				{
					// Each field
					foreach (fieldName, fieldData; classData.fields)
					{
						if (fieldName == name)
						{
							oldLine = fieldData.line;
							oldColumn = fieldData.column;
							oldType = IdentifierType.field_;
						}
					}
					// Each method
					foreach (methodName, methodInfo; classData.methods)
					{
						if (methodName == name)
						{
							oldLine = methodInfo.line;
							oldColumn = methodInfo.column;
							oldType = IdentifierType.method_;
						}
					}
				}
			}
		}

		// Check enum fields
		if (oldType == IdentifierType.invalid_)
		{
			// Each scope frame
			foreach (frame; gScope.frames)
			{
				// Each enum
				foreach (enumName, enumData; frame.enums)
				{
					// Each field
					foreach (fieldName, fieldData; enumData.fields)
					{
						if (fieldName == name)
						{
							oldLine = fieldData.line;
							oldColumn = fieldData.column;
							oldType = IdentifierType.field_;
						}
					}
				}
			}
		}

		// Just return if there is nothing declared with that name
		if (oldType == IdentifierType.invalid_)
			return;

		// Save the line and column of the original declaration
		if (name !in this.nameClashes)
		{
			this.nameClashes[name] = [];
			this.nameClashes[name] ~= Position(oldLine, oldColumn, oldType);
		}

		// It is a redeclaration if the line and column are already used
		bool isRedeclaration = false;
		foreach (pos; this.nameClashes[name])
		{
			if (pos.line == line && pos.column == column && pos.type == type)
				isRedeclaration = true;
		}

		// Save it if it is not a redeclaration
		if (!isRedeclaration)
		{
			this.nameClashes[name] ~= Position(line, column, type);
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
			enum coffee {// [warn]: The enum 'coffee' clashes with the variable at (68:7).
				coffee // [warn]: The field 'coffee' clashes with the variable at (68:7).
			}
		}
	}c, analysis.run.AnalyzerCheck.name_clash_check);

	stderr.writeln("Unittest for NameClashCheck passed.");
}

