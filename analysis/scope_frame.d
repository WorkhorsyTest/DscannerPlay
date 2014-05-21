// Copyright (c) 2014, Matthew Brennan Jones <matthew.brennan.jones@gmail.com>
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

// FIXME: Restructure everything is in module so we can access it by going:
// Scope.blah
module analysis.scope_frame;
/*
This module has functions for managing what is in scope. This includes:
  1. What the this pointer is
  2. What the parent container object is(EG: module, function, class, et cetera)
  3. What variables and parameters are in scope
  4. What classes, structs, functions, delegates, enums, methods, and fields are
     available to the current scope.
  5. The names, types, return types, parameters, line, and columns of everything

A good example of how to use it would be the ScopeAnalyzer.
*/

import std.stdio;
import std.string;
import std.stdint;
import dlang_helper;

struct TypeData
{
	string name;
	bool isArray;
	bool isTemplate; // FIXME: Not actually used yet

	this(string name)
	{
		if (name.ends_with("[]"))
		{
			this.isArray = true;
		}
		this.name = name.before("[]");
	}

	const string toString()
	{
		if (this.isArray)
			return "%s[]".format(this.name);
		else
			return this.name;
	}
}

struct FunctionData
{
	string name;
	TemplateData[] templates;
	TypeData returnType;
	TypeData[] argTypes;
	size_t line;
	size_t column;
}

struct VariableData
{
	string name;
	TypeData type;
	bool isUsed;
	bool isParameter;
	size_t line;
	size_t column;
}

struct TemplateData
{
	string name;
	size_t line;
	size_t column;
}

struct StructData
{
	string name;
	VariableData[string] fields;
	FunctionData[string] methods;
	size_t line;
	size_t column;
}

struct ClassData
{
	string name;
	VariableData[string] fields;
	FunctionData[string] methods;
	size_t line;
	size_t column;
}

struct FieldData
{
	size_t line;
	size_t column;
}

struct EnumData
{
	string name;
	TypeData type;
	FieldData[string] fields;
	size_t line;
	size_t column;
}

struct ModuleData
{
	string name;
	VariableData[string] variables;
	FunctionData[string] functions;
	StructData[string] structs;
	ClassData[string] classes;
	EnumData[string] enums;
}

struct ScopeFrame
{
	string[] imports;
	TemplateData[string] templates;
	VariableData[string] variables;
	FunctionData[string] functions;
	StructData[string] structs;
	ClassData[string] classes;
	EnumData[string] enums;
}

enum IdentifierType : string
{
	invalid_ = "invalid",
	module_ = "module",
	class_ = "class",
	struct_ = "struct",
	function_ = "function",
	method_ = "method",
	delegate_ = "delegate",
	variable_ = "variable",
	parameter_ = "parameter",
	template_ = "template",
	field_ = "field",
	enum_ = "enum"
}

struct Decoration
{
	bool isProperty;
	bool isRef;
	bool isAuto;
}

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

// FIXME: All the name clash checks should be extracted and moved into the name clash checker.
/*private*/ Position[][string] nameClashes;
/*private*/ ScopeFrame[] frames;
/*private*/ ModuleData[string] modules;
Queue!string thisPointers;
Queue!Decoration decorations;
Queue!IdentifierType parents;

void scopeFrameClearEverything()
{
	nameClashes.clear();
	frames.clear();
	modules.clear();
	thisPointers.clear();
	decorations.clear();
	parents.clear();
}

void scopeFrameStart()
{
	// Add a new scope frame
	ScopeFrame frame;
	frames ~= frame;

	info("scopeFrameStart #%s", frames.length-1);
}

void scopeFrameExit()
{
	info("scopeFrameExit");

	// Print all the variables & functions in the scope frames
	info("    Dumping frame #%s", frames.length-1);
	auto frame = frames[$-1];
	foreach (name, varData; frame.variables)
		info("            var:%s:%s, isUsed:%d", name, varData, varData.isUsed);
	foreach (name, funcData; frame.functions)
		info("            func:%s:%s", name, funcData);
	foreach (name, structData; frame.structs)
		info("            struct:%s:%s", name, structData);
	foreach (name, classData; frame.classes)
		info("            class:%s:%s", name, classData);
	foreach (name, enumData; frame.enums)
		info("            enum:%s:%s", name, enumData);

	// Remove the current scope frame
	frames = frames[0 .. $-1];
}

VariableData[string] getCurrentScopeFrameVariables()
{
	if (frames.length == 0)
		return null;

	return frames[$-1].variables;
}

FunctionData getFunctionDataByName(string name)
{
	// Match functions in scope frames
	foreach (frame; std.range.retro(frames))
	{
		if (name in frame.functions)
		{
			return frame.functions[name];
		}
	}

	// Match exact module function name in other module
	foreach (mod; modules)
	{
		if (name.startsWith(mod.name) && name.length > mod.name.length)
		{
			auto offset = mod.name.length + 1;
			string offsetName = name[offset .. $];
			if (offsetName in mod.functions)
			{
				return mod.functions[offsetName];
			}
		}
	}

	// Match partial module function name using imports
	foreach (frame; std.range.retro(frames))
	{
		foreach (importName; frame.imports)
		{
			if (importName in modules)
			{
				auto mod = modules[importName];
				if (name in mod.functions) {
					return mod.functions[name];
				}
			}
		}
	}

	return FunctionData.init;
}

VariableData getVariableDataByName(string name) {
	// Match variables in scope frames
	foreach (frame; std.range.retro(frames))
	{
		if (name in frame.variables)
		{
			return frame.variables[name];
		}
	}

	// Match exact module variable name in other module
	foreach (mod; modules)
	{
		if (name.startsWith(mod.name) && name.length > mod.name.length)
		{
			auto offset = mod.name.length + 1;
			string offsetName = name[offset .. $];
			if (offsetName in mod.variables)
			{
				return mod.variables[offsetName];
			}
		}
	}

	// Match partial module variable name using imports
	foreach (frame; std.range.retro(frames))
	{
		foreach (importName; frame.imports)
		{
			if (importName in modules)
			{
				auto mod = modules[importName];
				if (name in mod.variables)
				{
					return mod.variables[name];
				}
			}
		}
	}

	return VariableData.init;
}

TemplateData getTemplateDataByName(string name) {
	// Match template in scope frames
	foreach (frame; std.range.retro(frames))
	{
		if (name in frame.templates)
		{
			return frame.templates[name];
		}
	}

	return TemplateData.init;
}

StructData getStructDataByName(string name)
{
	// Match structs in scope frames
	foreach (frame; std.range.retro(frames))
	{
		if (name in frame.structs)
		{
			return frame.structs[name];
		}
	}

	// Match exact module struct name in other module
	foreach (mod; modules)
	{
		if (name.startsWith(mod.name) && name.length > mod.name.length)
		{
			auto offset = mod.name.length + 1;
			string offsetName = name[offset .. $];
			if (offsetName in mod.structs)
			{
				return mod.structs[offsetName];
			}
		}
	}

	// Match partial module struct name using imports
	foreach (frame; std.range.retro(frames))
	{
		foreach (importName; frame.imports)
		{
			if (importName in modules)
			{
				auto mod = modules[importName];
				if (name in mod.structs)
				{
					return mod.structs[name];
				}
			}
		}
	}

	return StructData.init;
}

ClassData getClassDataByName(string name)
{
	// Match classes in scope frames
	foreach (frame; std.range.retro(frames))
	{
		if (name in frame.classes)
		{
			return frame.classes[name];
		}
	}

	// Match exact module class name in other module
	foreach (mod; modules)
	{
		if (name.startsWith(mod.name) && name.length > mod.name.length)
		{
			auto offset = mod.name.length + 1;
			string offsetName = name[offset .. $];
			if (offsetName in mod.classes)
			{
				return mod.classes[offsetName];
			}
		}
	}

	// Match partial module class name using imports
	foreach (frame; std.range.retro(frames))
	{
		foreach (importName; frame.imports)
		{
			if (importName in modules)
			{
				auto mod = modules[importName];
				if (name in mod.classes)
				{
					return mod.classes[name];
				}
			}
		}
	}

	return ClassData.init;
}

EnumData getEnumDataByName(string name)
{
	// Match enums in scope frames
	foreach (frame; std.range.retro(frames))
	{
		if (name in frame.enums)
		{
			return frame.enums[name];
		}
	}

	// Match exact module enum name in other module
	foreach (mod; modules)
	{
		if (name.startsWith(mod.name) && name.length > mod.name.length)
		{
			auto offset = mod.name.length + 1;
			string offsetName = name[offset .. $];
			if (offsetName in mod.enums)
			{
				return mod.enums[offsetName];
			}
		}
	}

	// Match partial module enum name using imports
	foreach (frame; std.range.retro(frames))
	{
		foreach (importName; frame.imports)
		{
			if (importName in modules)
			{
				auto mod = modules[importName];
				if (name in mod.enums)
				{
					return mod.enums[name];
				}
			}
		}
	}

	return EnumData.init;
}

ModuleData getModuleDataByName(string name)
{
	if (name in modules)
	{
		return modules[name];
	}

	return ModuleData.init;
}

void setVariableIsUsedByName(string name)
{
	foreach (frame; std.range.retro(frames))
	{
		if (name in frame.variables)
		{
			frame.variables[name].isUsed = true;
			return;
		}
	}

	throw new Exception("Could not find any declared variable named: %s".format(name));
}

bool isAlreadyImported(string importName)
{
	foreach (frame; frames)
	{
		foreach (imp; frame.imports)
		{
			if (imp == importName)
			{
				return true;
			}
		}
	}

	return false;
}

void addImport(string importName)
{
	info("addImport");

	// Make sure everything is sane
	assert (importName, "import name was null");
	assert (importName.length, "import name was blank");

	frames[$-1].imports ~= importName;
	info("    add import: %s", importName);
}

void addFunction(FunctionData funcData)
{
	info("addFunction");

	// Make sure everything is sane
	assert (funcData !is FunctionData.init, "function was null");
	assert (funcData.name, "function name was null");
	assert (funcData.name.length, "function name was blank");
	assert (funcData.returnType !is TypeData.init, "function return type was null");
	assert (funcData.line, "function line was 0");
	assert (funcData.column, "function column was 0");

	// Skip if declaring same function
	foreach (frame; frames)
	{
		if (funcData.name in frame.functions)
		{
			auto funcOther = frame.functions[funcData.name];
			if (funcOther == funcData) {
				info("Tried to redeclare function '%s'.", funcData.name);
				return;
			}
		}
	}

	// Declare the function
	info("    frame count: %d", frames.length);
	frames[$-1].functions[funcData.name] = funcData;
	info("    declare function: %s:%s", funcData.name, funcData.returnType);

	// Check to see if the name is already used
	checkNameClashes(funcData.name, funcData.line, funcData.column, IdentifierType.function_);
}

void addTemplateParameter(TemplateData tempData)
{
	info("addTemplateParameter");

	// Make sure everything is sane
	assert (tempData !is TemplateData.init, "template was null");
	assert (tempData.name, "template name was null");
	assert (tempData.name.length, "template name was blank");
	assert (tempData.line, "template line was 0");
	assert (tempData.column, "template column was 0");

	// Skip if declaring same template
	foreach (frame; frames)
	{
		if (tempData.name in frame.templates)
		{
			auto tempOther = frame.templates[tempData.name];
			if (tempOther == tempData)
			{
				info("Tried to redeclare template param '%s'.", tempData.name);
				return;
			}
		}
	}

	// Declare the template
	info("    frame count: %d", frames.length);
	frames[$-1].templates[tempData.name] = tempData;
	info("    declare template: %s:%s", tempData.name);

	// Check to see if the name is already used
	auto idenType = IdentifierType.template_;
	checkNameClashes(tempData.name, tempData.line, tempData.column, idenType);
}

void addVariable(VariableData varData)
{
	info("addVariable");

	// Make sure everything is sane
	assert (varData !is VariableData.init, "variable was null");
	assert (varData.name, "variable name was null");
	assert (varData.name.length, "variable name was blank");
	assert (varData.type !is TypeData.init, "variable type was null");
	assert (varData.line, "variable line was 0");
	assert (varData.column, "variable column was 0");

	// Skip if declaring same variable
	foreach (frame; frames)
	{
		if (varData.name in frame.variables)
		{
			auto varOther = frame.variables[varData.name];
			if (varOther == varData)
			{
				info("Tried to redeclare variable '%s'.", varData.name);
				return;
			}
		}
	}

	// Declare the variable
	info("    frame count: %d", frames.length);
	frames[$-1].variables[varData.name] = varData;
	info("    declare variable: %s:%s", varData.name, varData.type);

	// Check to see if the name is already used
	IdentifierType identifier;
	if (varData.isParameter)
		identifier = IdentifierType.parameter_;
	else
		identifier = IdentifierType.variable_;

	checkNameClashes(varData.name, varData.line, varData.column, identifier);
}

void addClass(ClassData classData)
{
	info("addClass");

	// Make sure everything is sane
	assert (classData !is ClassData.init, "class was null");
	assert (classData.name, "class name was null");
	assert (classData.name.length, "class name was blank");
	assert (classData.line, "class line was 0");
	assert (classData.column, "class column was 0");

	// Skip if declaring same class
	foreach (frame; frames)
	{
		if (classData.name in frame.classes)
		{
			auto classOther = frame.classes[classData.name];
			if (classOther == classData)
			{
				info("Tried to redeclare class '%s'.", classData.name);
				return;
			}
		}
	}

	// Declare the class
	info("    frame count: %d", frames.length);
	frames[$-1].classes[classData.name] = classData;
	info("    declare class: %s", classData.name);

	// Check to see if the name is already used
	checkNameClashes(classData.name, classData.line, classData.column, IdentifierType.class_);
	// Check to see if the field names are already used
	foreach (fieldName, field; classData.fields)
	{
		checkNameClashes(fieldName, field.line, field.column, IdentifierType.field_);
	}
	// Check to see if the method names are already used
	foreach (methodName, method; classData.methods)
	{
		checkNameClashes(methodName, method.line, method.column, IdentifierType.method_);
	}
}

void addStruct(StructData structData)
{
	info("addStruct");

	// Make sure everything is sane
	assert (structData !is StructData.init, "Struct was null");
	assert (structData.name, "struct name was null");
	assert (structData.name.length, "struct name was blank");
	assert (structData.line, "struct line was 0");
	assert (structData.column, "struct column was 0");

	// Skip if declaring same struct
	foreach (frame; frames)
	{
		if (structData.name in frame.structs)
		{
			auto structOther = frame.structs[structData.name];
			if (structOther == structData)
			{
				info("Tried to redeclare struct '%s'.", structData.name);
				return;
			}
		}
	}

	// Declare the struct
	info("    frame count: %d", frames.length);
	frames[$-1].structs[structData.name] = structData;
	info("    declare struct: %s", structData.name);

	// Check to see if the name is already used
	checkNameClashes(structData.name, structData.line, structData.column, IdentifierType.struct_);
	// Check to see if the field names are already used
	foreach (fieldName, field; structData.fields)
	{
		checkNameClashes(fieldName, field.line, field.column, IdentifierType.field_);
	}
	// Check to see if the method names are already used
	foreach (methodName, method; structData.methods)
	{
		checkNameClashes(methodName, method.line, method.column, IdentifierType.method_);
	}
}

void addEnum(EnumData enumData) {
	info("addEnum");

	// Make sure everything is sane
	assert (enumData !is EnumData.init, "enum was null");
	assert (enumData.name, "enum name was null");
	assert (enumData.name.length, "enum name was blank");
	assert (enumData.line, "enum line was 0");
	assert (enumData.column, "enum column was 0");

	// Skip if declaring same enum
	foreach (frame; frames)
	{
		if (enumData.name in frame.enums)
		{
			auto enumOther = frame.enums[enumData.name];
			if (enumOther == enumData)
			{
				info("Tried to redeclare enum '%s'.", enumData.name);
				return;
			}
		}
	}

	// Declare the enum
	info("    frame count: %d", frames.length);
	frames[$-1].enums[enumData.name] = enumData;
	info("    declare enum: %s", enumData.name);

	// Check to see if the name is already used
	checkNameClashes(enumData.name, enumData.line, enumData.column, IdentifierType.enum_);
	// Check to see if the field names are already used
	foreach (fieldName, field; enumData.fields)
	{
		checkNameClashes(fieldName, field.line, field.column, IdentifierType.field_);
	}
}

void addModule(ModuleData moduleData)
{
	info("addModule");

	// Make sure everything is sane
	assert (moduleData !is ModuleData.init, "Module was null");
	assert (moduleData.name, "module name was null");
	assert (moduleData.name.length, "module name was blank");

	// Skip if declaring same module
	if (moduleData.name in modules)
	{
		auto moduleOther = modules[moduleData.name];
		if (moduleOther == moduleData)
		{
			info("Tried to redeclare module '%s'.", moduleData.name);
			return;
		}
	}

	// Declare the module
	modules[moduleData.name] = moduleData;
	info("    declare module: %s", moduleData.name);
}

Position[][string] getNameClashes()
{
	return nameClashes;
}

void checkNameClashes(string name, size_t line, size_t column, IdentifierType type)
{
	auto varData = getVariableDataByName(name);
	auto funcData = getFunctionDataByName(name);
	auto structData = getStructDataByName(name);
	auto classData = getClassDataByName(name);
	auto enumData = getEnumDataByName(name);
	size_t oldLine, oldColumn;
	IdentifierType oldType;

	// A variable has that name
	if (varData !is VariableData.init)
	{
		oldLine = varData.line;
		oldColumn = varData.column;
		oldType = IdentifierType.variable_;
	// A function has that name
	}
	else if (funcData !is FunctionData.init)
	{
		oldLine = funcData.line;
		oldColumn = funcData.column;
		oldType = IdentifierType.function_;
	// A struct has that name
	}
	else if (structData !is StructData.init)
	{
		oldLine = structData.line;
		oldColumn = structData.column;
		oldType = IdentifierType.struct_;
	// A class has that name
	}
	else if (classData !is ClassData.init)
	{
		oldLine = classData.line;
		oldColumn = classData.column;
		oldType = IdentifierType.class_;
	// An enum has that name
	}
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
		foreach (frame; frames)
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
		foreach (frame; frames)
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
		foreach (frame; frames)
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
	if (name !in nameClashes)
	{
		nameClashes[name] = [];
		nameClashes[name] ~= Position(oldLine, oldColumn, oldType);
	}

	// It is a redeclaration if the line and column are already used
	bool isRedeclaration = false;
	foreach (pos; nameClashes[name])
	{
		if (pos.line == line && pos.column == column && pos.type == type)
			isRedeclaration = true;
	}

	// Save it if it is not a redeclaration
	if (!isRedeclaration)
	{
		nameClashes[name] ~= Position(line, column, type);
	}
}

