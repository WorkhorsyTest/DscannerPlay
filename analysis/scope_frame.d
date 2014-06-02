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

		info("scopeFrameStart #%s", frames.length-1);
	}

	void popFrame()
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

		stderr.writefln("!!! setVariableIsUsedByName() failed on variable '%s'.".format(name));
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
				if (funcOther == funcData)
				{
					info("Tried to redeclare function '%s'.", funcData.name);
					return;
				}
			}
		}

		// Declare the function
		info("    frame count: %d", frames.length);
		frames[$-1].functions[funcData.name] = funcData;
		info("    declare function: %s:%s", funcData.name, funcData.returnType);
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
	}

	void addVariable(VariableData varData)
	{
		info("addVariable");

		// Make sure everything is sane
		assert (varData !is VariableData.init, "variable was null");
		assert (varData.name, "variable name was null");
		assert (varData.name.length, "variable name was blank");
		assert (varData.type !is TypeData.init, "variable type was null");
		//assert (varData.line, "variable line was 0");
		//assert (varData.column, "variable column was 0");

		// Warn if the variable is on line or column zero
		if (varData.line == 0 || varData.column == 0)
		{
			stderr.writefln(
				"!!! addVariable() warning variable is on line or column zero: name:%s, type:%s, isUsed:%s, isParameter:%s, line:%s, column:%s", 
				varData.name,
				varData.type,
				varData.isUsed,
				varData.isParameter,
				varData.line,
				varData.column
			);
		}

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
	}

	void addEnum(EnumData enumData)
	{
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
}

