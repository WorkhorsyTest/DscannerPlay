// Copyright (c) 2014, Matthew Brennan Jones <matthew.brennan.jones@gmail.com>
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.scope_frame;
/*
This module has functions for managing what is in scope. This includes:
  1. What the this pointer is
  2. What the parent container object is(EG: module, function, class, et cetera)
  3. What variables and parameters are in scope
  4. What classes, structs, functions, delegates, enums, methods, and fields are
     available to the current scope.
  5. The names, types, return types, parameters, line, and columns of everything

A good example of how to use it would be the BaseScopeAnalyzer.
*/

import std.stdio;
import std.string;
import std.stdint;
import dlang_helper;


Scope gScope = null;

struct TypeData
{
	string name;
	bool isArray;

	this(string name)
	{
		if (name.endsWith("[]"))
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

pure bool isSizeT(const TypeData typeData) { return typeData.name == "size_t"; }

pure bool isInt(const TypeData typeData) { return typeData.name == "int"; }
pure bool isUint(const TypeData typeData) { return typeData.name == "uint"; }
pure bool isLong(const TypeData typeData) { return typeData.name == "long"; }
pure bool isUlong(const TypeData typeData) { return typeData.name == "ulong"; }

pure bool isInt8T(const TypeData typeData) { return typeData.name == "int8_t"; }
pure bool isInt16T(const TypeData typeData) { return typeData.name == "int16_t"; }
pure bool isInt32T(const TypeData typeData) { return typeData.name == "int32_t"; }
pure bool isInt64T(const TypeData typeData) { return typeData.name == "int64_t"; }

pure bool isUint8T(const TypeData typeData) { return typeData.name == "uint8_t"; }
pure bool isUint16T(const TypeData typeData) { return typeData.name == "uint16_t"; }
pure bool isUint32T(const TypeData typeData) { return typeData.name == "uint32_t"; }
pure bool isUint64T(const TypeData typeData) { return typeData.name == "uint64_t"; }

pure bool isAnInt32(const TypeData typeData)
{
	return isInt(typeData)
		|| isUint(typeData)
		|| isInt32T(typeData)
		|| isUint32T(typeData);
}

pure bool isAnInt64(const TypeData typeData)
{
	return isLong(typeData)
		|| isUlong(typeData)
		|| isInt64T(typeData)
		|| isUint64T(typeData);
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

class Scope
{
	ScopeFrame[] frames;
	ModuleData[string] modules;
	private Queue!IdentifierType parents;
	private Queue!string thisPointers;
	private Queue!Decoration decorations;

	void pushFrame()
	{
		// Add a new scope frame
		ScopeFrame frame;
		frames ~= frame;
	}

	void popFrame()
	{
		// Remove the current scope frame
		frames = frames[0 .. $-1];
	}

	void clear()
	{
		frames.clear();
		modules.clear();
		parents.clear();
		thisPointers.clear();
		decorations.clear();
	}

	void parentsPush(IdentifierType parent)
	{
		parents.push(parent);
	}

	void parentsPop()
	{
		parents.pop();
	}

	IdentifierType parentsPeak()
	{
		return parents.peak();
	}

	void thisPointersPush(string thisPointer)
	{
		thisPointers.push(thisPointer);
	}

	void thisPointersPop()
	{
		thisPointers.pop();
	}

	string thisPointersPeak()
	{
		return thisPointers.peak();
	}

	void decorationsPush(Decoration decoration)
	{
		decorations.push(decoration);
	}

	void decorationsPop()
	{
		decorations.pop();
	}

	Decoration decorationsPeak()
	{
		return decorations.peak();
	}

	VariableData[string] getCurrentFrameVariables()
	{
		if (frames.length == 0)
			return null;

		return frames[$-1].variables;
	}

	FunctionData getFunction(string name)
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
					if (name in mod.functions)
					{
						return mod.functions[name];
					}
				}
			}
		}

		return FunctionData.init;
	}

	VariableData getVariable(string name)
	{
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

	TemplateData getTemplate(string name)
	{
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

	StructData getStruct(string name)
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

	ClassData getClass(string name)
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

	EnumData getEnum(string name)
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

	ModuleData getModule(string name)
	{
		if (name in modules)
			return modules[name];

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

		stderr.writefln("??? setVariableIsUsedByName() failed unknown variable '%s'.".format(name));
	}

	void addImport(string importName)
	{
		// Make sure everything is sane
		if (isNullOrBlank(importName))
		{
			stderr.writeln("!!! addImport() failed because importName was null or blank.");
			return;
		}

		frames[$-1].imports ~= importName;
	}

	bool isImported(string importName)
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

	void addFunction(FunctionData funcData)
	{
		// Make sure everything is sane
		string failMessage = null;
		if (funcData is FunctionData.init)
			failMessage = "function was init";
		else if (isNullOrBlank(funcData.name))
			failMessage = "function name was null or blank";
		else if (funcData.returnType is TypeData.init)
			failMessage = "function return type was init";
		else if (funcData.line == 0)
			failMessage = "function line was 0";
		else if (funcData.column == 0)
			failMessage = "function column was 0";

		if (failMessage)
		{
			stderr.writefln("!!! addFunction() failed because %s", failMessage);
			return;
		}

		// Skip if declaring same function
		auto funcOther = gScope.getFunction(funcData.name);
		if (funcOther !is FunctionData.init && funcOther == funcData)
		{
			stderr.writefln("??? addVariable() failed because function is already declared '%s'.", funcData.name);
			return;
		}

		// Declare the function
		frames[$-1].functions[funcData.name] = funcData;
	}

	void addTemplateParameter(TemplateData tempData)
	{
		// Make sure everything is sane
		string failMessage = null;
		if (tempData is TemplateData.init)
			failMessage = "template was init";
		else if (isNullOrBlank(tempData.name))
			failMessage = "template name was init";
		else if (tempData.line == 0)
			failMessage = "template line was 0";
		else if (tempData.column == 0)
			failMessage = "template column was 0";

		if (failMessage)
		{
			stderr.writefln("!!! addTemplateParameter() failed because %s", failMessage);
			return;
		}

		// Skip if declaring same template
		auto tempOther = gScope.getTemplate(tempData.name);
		if (tempOther !is TemplateData.init && tempOther == tempData)
		{
			stderr.writefln("??? addTemplateParameter() failed because template is already declared '%s'.", tempData.name);
			return;
		}

		// Declare the template
		frames[$-1].templates[tempData.name] = tempData;
	}

	void addVariable(VariableData varData)
	{
		// Make sure everything is sane
		string failMessage = null;
		if (varData is VariableData.init)
			failMessage = "variable was init";
		else if (isNullOrBlank(varData.name))
			failMessage = "variable name was null or blank";
		else if (varData.type is TypeData.init)
			failMessage = "variable type was init";
		else if (varData.line == 0)
			failMessage = "variable line was 0";
		else if (varData.column == 0)
			failMessage = "variable column was 0";

		if (failMessage)
		{
			stderr.writefln("!!! addVariable() failed because %s", failMessage);
			return;
		}

		// Skip if declaring same variable
		auto varOther = gScope.getVariable(varData.name);
		if (varOther !is VariableData.init && varOther == varData)
		{
			stderr.writefln("??? addVariable() failed because variable is already declared '%s'.", varData.name);
			return;
		}

		// Declare the variable
		frames[$-1].variables[varData.name] = varData;
	}

	void addClass(ClassData classData)
	{
		// Make sure everything is sane
		string failMessage = null;
		if (classData is ClassData.init)
			failMessage = "class was init";
		else if (isNullOrBlank(classData.name))
			failMessage = "class name was null or blank";
		else if (classData.line == 0)
			failMessage = "class line was 0";
		else if (classData.column == 0)
			failMessage = "class column was 0";

		if (failMessage)
		{
			stderr.writefln("!!! addClass() failed because %s", failMessage);
			return;
		}

		// Skip if declaring same class
		auto classOther = gScope.getClass(classData.name);
		if (classOther !is ClassData.init && classOther == classData)
		{
			stderr.writefln("??? addClass() failed because class is already declared '%s'.", classData.name);
			return;
		}

		// Declare the class
		frames[$-1].classes[classData.name] = classData;
	}

	void addStruct(StructData structData)
	{
		// Make sure everything is sane
		string failMessage = null;
		if (structData is StructData.init)
			failMessage = "Struct was init";
		else if (isNullOrBlank(structData.name))
			failMessage = "struct name was null or blank";
		else if (structData.line == 0)
			failMessage = "struct line was 0";
		else if (structData.column == 0)
			failMessage = "struct column was 0";

		if (failMessage)
		{
			stderr.writefln("!!! addStruct() failed because %s", failMessage);
			return;
		}

		// Skip if declaring same struct
		auto structOther = gScope.getStruct(structData.name);
		if (structOther !is structData.init && structOther == structData)
		{
			stderr.writefln("??? addStruct() failed because struct is already declared '%s'.", structData.name);
			return;
		}

		// Declare the struct
		frames[$-1].structs[structData.name] = structData;
	}

	void addEnum(EnumData enumData)
	{
		// Make sure everything is sane
		string failMessage = null;
		if (enumData is EnumData.init)
			failMessage = "enum was init";
		else if (isNullOrBlank(enumData.name))
			failMessage = "enum name was null or blank";
		else if (enumData.line == 0)
			failMessage = "enum line was 0";
		else if (enumData.column == 0)
			failMessage = "enum column was 0";

		if (failMessage)
		{
			stderr.writefln("!!! addEnum() failed because %s", failMessage);
			return;
		}

		// Skip if declaring same enum
		auto enumOther = gScope.getEnum(enumData.name);
		if (enumOther !is enumData.init && enumOther == enumData)
		{
			stderr.writefln("??? addEnum() failed because enum is already declared '%s'.", enumData.name);
			return;
		}

		// Declare the enum
		frames[$-1].enums[enumData.name] = enumData;
	}

	void addModule(ModuleData moduleData)
	{
		// Make sure everything is sane
		string failMessage = null;
		if (moduleData is ModuleData.init)
			failMessage = "Module was init";
		else if (isNullOrBlank(moduleData.name))
			failMessage = "module name was null or blank";

		if (failMessage)
		{
			stderr.writefln("!!! addModule() failed because %s", failMessage);
			return;
		}

		// Skip if declaring same module
		if (moduleData.name in modules)
		{
			auto moduleOther = modules[moduleData.name];
			if (moduleOther == moduleData)
			{
				stderr.writefln("??? addModule() failed because module is already declared '%s'.", moduleData.name);
				return;
			}
		}

		// Declare the module
		modules[moduleData.name] = moduleData;
	}
}

