// Copyright (c) 2014, Matthew Brennan Jones <matthew.brennan.jones@gmail.com>
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.stack_frame;
/*
This module has functions for managing what is in scope. This includes:
  1. What the this pointer is
  2. What the parent container object is(EG: module, function, class, et cetera)
  3. What variables and parameters are in scope
  4. What classes, structs, functions, delegates, enums, methods, and fields are
     available to the current scope.
  5. The names, types, return types, parameters, line, and columns of everything

A good example of how to use it would be the BaseWalkingAnalyzer.
*/

import std.stdio;
import std.string;
import std.stdint;
import dlang_helper;

struct TypeData {
	string name;
	bool is_array;
	bool is_template; // FIXME: Not actually used yet

	this(string name) {
		if(name.ends_with("[]")) {
			this.is_array = true;
		}
		this.name = name.before("[]");
	}

	const string toString() {
		if(this.is_array) {
			return "%s[]".format(this.name);
		} else {
			return this.name;
		}
	}
}

struct FunctionData {
	string name;
	TemplateData[] templates;
	TypeData return_type;
	TypeData[] arg_types;
	size_t line;
	size_t column;
}

struct VariableData {
	string name;
	TypeData type;
	bool is_used;
	bool is_parameter;
	size_t line;
	size_t column;
}

struct TemplateData {
	string name;
	size_t line;
	size_t column;
}

struct StructData {
	string name;
	VariableData[string] fields;
	FunctionData[string] methods;
	size_t line;
	size_t column;
}

struct ClassData {
	string name;
	VariableData[string] fields;
	FunctionData[string] methods;
	size_t line;
	size_t column;
}

struct FieldData {
	size_t line;
	size_t column;
}

struct EnumData {
	string name;
	TypeData type;
	FieldData[string] fields;
	size_t line;
	size_t column;
}

struct ModuleData {
	string name;
	VariableData[string] variables;
	FunctionData[string] functions;
	StructData[string] structs;
	ClassData[string] classes;
	EnumData[string] enums;
}

struct StackFrame {
	string[] imports;
	TemplateData[string] templates;
	VariableData[string] variables;
	FunctionData[string] functions;
	StructData[string] structs;
	ClassData[string] classes;
	EnumData[string] enums;
}

enum IdentifierType : string {
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

struct Decoration {
	bool is_property;
	bool is_ref;
	bool is_auto;
}

struct Position {
	size_t line;
	size_t column;
	IdentifierType type;

	string type_name() {
		return cast(string) this.type;
	}
}

/*private*/ Position[][string] name_clashes;
/*private*/ StackFrame[] frames;
/*private*/ ModuleData[string] modules;
Queue!string this_pointers;
Queue!Decoration decorations;
Queue!IdentifierType parents;

void stack_frame_print_all() {
	stderr.writefln("!!! modules");
	foreach(mod; modules) {
		stderr.writefln("module: %s", mod.name);
		foreach(name, var_data; mod.variables) {
			stderr.writefln("            var:%s:%s, is_used:%d", name, var_data, var_data.is_used);
		}
		foreach(name, func_data; mod.functions) {
			stderr.writefln("            func:%s:%s", name, func_data);
		}
		foreach(name, struct_data; mod.structs) {
			stderr.writefln("            struct:%s:%s", name, struct_data);
		}
		foreach(name, class_data; mod.classes) {
			stderr.writefln("            class:%s:%s", name, class_data);
		}
		foreach(name, enum_data; mod.enums) {
			stderr.writefln("            enum:%s:%s", name, enum_data);
		}
	}
	stderr.writefln("!!! stack_frame_print_all()");
	foreach(frame; frames) {
		foreach(name, var_data; frame.variables) {
			stderr.writefln("            var:%s:%s, is_used:%d", name, var_data, var_data.is_used);
		}
		foreach(name, func_data; frame.functions) {
			stderr.writefln("            func:%s:%s", name, func_data);
		}
		foreach(name, struct_data; frame.structs) {
			stderr.writefln("            struct:%s:%s", name, struct_data);
		}
		foreach(name, class_data; frame.classes) {
			stderr.writefln("            class:%s:%s", name, class_data);
		}
		foreach(name, enum_data; frame.enums) {
			stderr.writefln("            enum:%s:%s", name, enum_data);
		}
	}
}

void stack_frame_clear_everything() {
	name_clashes.clear();
	frames.clear();
	modules.clear();
	this_pointers.clear();
	decorations.clear();
	parents.clear();
}

void stack_frame_start() {
	// Add a new stack frame
	StackFrame frame;
	frames ~= frame;

	info("stack_frame_start #%s", frames.length-1);
}

void stack_frame_exit() {
	info("stack_frame_exit");

	// Print all the variables & functions in the stack frames
	info("    Dumping frame #%s", frames.length-1);
	auto frame = frames[$-1];
	foreach(name, var_data; frame.variables) {
		info("            var:%s:%s, is_used:%d", name, var_data, var_data.is_used);
	}
	foreach(name, func_data; frame.functions) {
		info("            func:%s:%s", name, func_data);
	}
	foreach(name, struct_data; frame.structs) {
		info("            struct:%s:%s", name, struct_data);
	}
	foreach(name, class_data; frame.classes) {
		info("            class:%s:%s", name, class_data);
	}
	foreach(name, enum_data; frame.enums) {
		info("            enum:%s:%s", name, enum_data);
	}

	// Remove the current stack frame
	frames = frames[0 .. $-1];
}

VariableData[string] get_current_stack_frame_variables() {
	if(frames.length == 0)
		return null;

	return frames[$-1].variables;
}

FunctionData get_function_data_by_name(string name) {
	// Match functions in stack frames
	foreach(frame; std.range.retro(frames)) {
		if(name in frame.functions) {
			return frame.functions[name];
		}
	}

	// Match exact module function name in other module
	foreach(mod; modules) {
		if(name.startsWith(mod.name) && name.length > mod.name.length) {
			auto offset = mod.name.length + 1;
			string offset_name = name[offset .. $];
			if(offset_name in mod.functions) {
				return mod.functions[offset_name];
			}
		}
	}

	// Match partial module function name using imports
	foreach(frame; std.range.retro(frames)) {
		foreach(import_name; frame.imports) {
			if(import_name in modules) {
				auto mod = modules[import_name];
				if(name in mod.functions) {
					return mod.functions[name];
				}
			}
		}
	}

	return FunctionData.init;
}

VariableData get_variable_data_by_name(string name) {
	// Match variables in stack frames
	foreach(frame; std.range.retro(frames)) {
		if(name in frame.variables) {
			return frame.variables[name];
		}
	}

	// Match exact module variable name in other module
	foreach(mod; modules) {
		if(name.startsWith(mod.name) && name.length > mod.name.length) {
			auto offset = mod.name.length + 1;
			string offset_name = name[offset .. $];
			if(offset_name in mod.variables) {
				return mod.variables[offset_name];
			}
		}
	}

	// Match partial module variable name using imports
	foreach(frame; std.range.retro(frames)) {
		foreach(import_name; frame.imports) {
			if(import_name in modules) {
				auto mod = modules[import_name];
				if(name in mod.variables) {
					return mod.variables[name];
				}
			}
		}
	}

	return VariableData.init;
}

TemplateData get_template_data_by_name(string name) {
	// Match template in stack frames
	foreach(frame; std.range.retro(frames)) {
		if(name in frame.templates) {
			return frame.templates[name];
		}
	}

	return TemplateData.init;
}

StructData get_struct_data_by_name(string name) {
	// Match structs in stack frames
	foreach(frame; std.range.retro(frames)) {
		if(name in frame.structs) {
			return frame.structs[name];
		}
	}

	// Match exact module struct name in other module
	foreach(mod; modules) {
		if(name.startsWith(mod.name) && name.length > mod.name.length) {
			auto offset = mod.name.length + 1;
			string offset_name = name[offset .. $];
			if(offset_name in mod.structs) {
				return mod.structs[offset_name];
			}
		}
	}

	// Match partial module struct name using imports
	foreach(frame; std.range.retro(frames)) {
		foreach(import_name; frame.imports) {
			if(import_name in modules) {
				auto mod = modules[import_name];
				if(name in mod.structs) {
					return mod.structs[name];
				}
			}
		}
	}

	return StructData.init;
}

ClassData get_class_data_by_name(string name) {
	// Match classes in stack frames
	foreach(frame; std.range.retro(frames)) {
		if(name in frame.classes) {
			return frame.classes[name];
		}
	}

	// Match exact module class name in other module
	foreach(mod; modules) {
		if(name.startsWith(mod.name) && name.length > mod.name.length) {
			auto offset = mod.name.length + 1;
			string offset_name = name[offset .. $];
			if(offset_name in mod.classes) {
				return mod.classes[offset_name];
			}
		}
	}

	// Match partial module class name using imports
	foreach(frame; std.range.retro(frames)) {
		foreach(import_name; frame.imports) {
			if(import_name in modules) {
				auto mod = modules[import_name];
				if(name in mod.classes) {
					return mod.classes[name];
				}
			}
		}
	}

	return ClassData.init;
}

EnumData get_enum_data_by_name(string name) {
	// Match enums in stack frames
	foreach(frame; std.range.retro(frames)) {
		if(name in frame.enums) {
			return frame.enums[name];
		}
	}

	// Match exact module enum name in other module
	foreach(mod; modules) {
		if(name.startsWith(mod.name) && name.length > mod.name.length) {
			auto offset = mod.name.length + 1;
			string offset_name = name[offset .. $];
			if(offset_name in mod.enums) {
				return mod.enums[offset_name];
			}
		}
	}

	// Match partial module enum name using imports
	foreach(frame; std.range.retro(frames)) {
		foreach(import_name; frame.imports) {
			if(import_name in modules) {
				auto mod = modules[import_name];
				if(name in mod.enums) {
					return mod.enums[name];
				}
			}
		}
	}

	return EnumData.init;
}

ModuleData get_module_data_by_name(string name) {
	if(name in modules) {
		return modules[name];
	}

	return ModuleData.init;
}

void set_variable_is_used_by_name(string name) {
	foreach(frame; std.range.retro(frames)) {
		if(name in frame.variables) {
			frame.variables[name].is_used = true;
			return;
		}
	}

	throw new Exception("Could not find any declared variable named: %s".format(name));
}

bool is_already_imported(string import_name) {
	foreach(frame; frames) {
		foreach(imp; frame.imports) {
			if(imp == import_name) {
				return true;
			}
		}
	}

	return false;
}

void add_import(string import_name) {
	info("add_import");

	// Make sure everything is sane
	assert(import_name, "import name was null");
	assert(import_name.length, "import name was blank");

	frames[$-1].imports ~= import_name;
	info("    add import: %s", import_name);
}

void add_function(FunctionData func_data) {
	info("add_function");

	// Make sure everything is sane
	assert(func_data !is FunctionData.init, "function was null");
	assert(func_data.name, "function name was null");
	assert(func_data.name.length, "function name was blank");
	assert(func_data.return_type !is TypeData.init, "function return type was null");
	assert(func_data.line, "function line was 0");
	assert(func_data.column, "function column was 0");

	// Skip if declaring same function
	foreach(frame; frames) {
		if(func_data.name in frame.functions) {
			auto func_other = frame.functions[func_data.name];
			if(func_other == func_data) {
				info("Tried to redeclare function '%s'.", func_data.name);
				return;
			}
		}
	}

	// Declare the function
	info("    frame count: %d", frames.length);
	frames[$-1].functions[func_data.name] = func_data;
	info("    declare function: %s:%s", func_data.name, func_data.return_type);

	// Check to see if the name is already used
	check_name_clashes(func_data.name, func_data.line, func_data.column, IdentifierType.function_);
}

void add_template_parameter(TemplateData temp_data) {
	info("add_template_parameter");

	// Make sure everything is sane
	assert(temp_data !is TemplateData.init, "template was null");
	assert(temp_data.name, "template name was null");
	assert(temp_data.name.length, "template name was blank");
	assert(temp_data.line, "template line was 0");
	assert(temp_data.column, "template column was 0");

	// Skip if declaring same template
	foreach(frame; frames) {
		if(temp_data.name in frame.templates) {
			auto temp_other = frame.templates[temp_data.name];
			if(temp_other == temp_data) {
				info("Tried to redeclare template param '%s'.", temp_data.name);
				return;
			}
		}
	}

	// Declare the template
	info("    frame count: %d", frames.length);
	frames[$-1].templates[temp_data.name] = temp_data;
	info("    declare template: %s:%s", temp_data.name);

	// Check to see if the name is already used
	auto iden_type = IdentifierType.template_;
	check_name_clashes(temp_data.name, temp_data.line, temp_data.column, iden_type);
}

void add_variable(VariableData var_data) {
	info("add_variable");

	// Make sure everything is sane
	assert(var_data !is VariableData.init, "variable was null");
	assert(var_data.name, "variable name was null");
	assert(var_data.name.length, "variable name was blank");
	assert(var_data.type !is TypeData.init, "variable type was null");
	assert(var_data.line, "variable line was 0");
	assert(var_data.column, "variable column was 0");

	// Skip if declaring same variable
	foreach(frame; frames) {
		if(var_data.name in frame.variables) {
			auto var_other = frame.variables[var_data.name];
			if(var_other == var_data) {
				info("Tried to redeclare variable '%s'.", var_data.name);
				return;
			}
		}
	}

	// Declare the variable
	info("    frame count: %d", frames.length);
	frames[$-1].variables[var_data.name] = var_data;
	info("    declare variable: %s:%s", var_data.name, var_data.type);

	// Check to see if the name is already used
	IdentifierType identifier;
	if(var_data.is_parameter)
		identifier = IdentifierType.parameter_;
	else
		identifier = IdentifierType.variable_;

	check_name_clashes(var_data.name, var_data.line, var_data.column, identifier);
}

void add_class(ClassData class_data) {
	info("add_class");

	// Make sure everything is sane
	assert(class_data !is ClassData.init, "class was null");
	assert(class_data.name, "class name was null");
	assert(class_data.name.length, "class name was blank");
	assert(class_data.line, "class line was 0");
	assert(class_data.column, "class column was 0");

	// Skip if declaring same class
	foreach(frame; frames) {
		if(class_data.name in frame.classes) {
			auto class_other = frame.classes[class_data.name];
			if(class_other == class_data) {
				info("Tried to redeclare class '%s'.", class_data.name);
				return;
			}
		}
	}

	// Declare the class
	info("    frame count: %d", frames.length);
	frames[$-1].classes[class_data.name] = class_data;
	info("    declare class: %s", class_data.name);

	// Check to see if the name is already used
	check_name_clashes(class_data.name, class_data.line, class_data.column, IdentifierType.class_);
	// Check to see if the field names are already used
	foreach(field_name, field; class_data.fields) {
		check_name_clashes(field_name, field.line, field.column, IdentifierType.field_);
	}
	// Check to see if the method names are already used
	foreach(method_name, method; class_data.methods) {
		check_name_clashes(method_name, method.line, method.column, IdentifierType.method_);
	}
}

void add_struct(StructData struct_data) {
	info("add_struct");

	// Make sure everything is sane
	assert(struct_data !is StructData.init, "Struct was null");
	assert(struct_data.name, "struct name was null");
	assert(struct_data.name.length, "struct name was blank");
	assert(struct_data.line, "struct line was 0");
	assert(struct_data.column, "struct column was 0");

	// Skip if declaring same struct
	foreach(frame; frames) {
		if(struct_data.name in frame.structs) {
			auto struct_other = frame.structs[struct_data.name];
			if(struct_other == struct_data) {
				info("Tried to redeclare struct '%s'.", struct_data.name);
				return;
			}
		}
	}

	// Declare the struct
	info("    frame count: %d", frames.length);
	frames[$-1].structs[struct_data.name] = struct_data;
	info("    declare struct: %s", struct_data.name);

	// Check to see if the name is already used
	check_name_clashes(struct_data.name, struct_data.line, struct_data.column, IdentifierType.struct_);
	// Check to see if the field names are already used
	foreach(field_name, field; struct_data.fields) {
		check_name_clashes(field_name, field.line, field.column, IdentifierType.field_);
	}
	// Check to see if the method names are already used
	foreach(method_name, method; struct_data.methods) {
		check_name_clashes(method_name, method.line, method.column, IdentifierType.method_);
	}
}

void add_enum(EnumData enum_data) {
	info("add_enum");

	// Make sure everything is sane
	assert(enum_data !is EnumData.init, "enum was null");
	assert(enum_data.name, "enum name was null");
	assert(enum_data.name.length, "enum name was blank");
	assert(enum_data.line, "enum line was 0");
	assert(enum_data.column, "enum column was 0");

	// Skip if declaring same enum
	foreach(frame; frames) {
		if(enum_data.name in frame.enums) {
			auto enum_other = frame.enums[enum_data.name];
			if(enum_other == enum_data) {
				info("Tried to redeclare enum '%s'.", enum_data.name);
				return;
			}
		}
	}

	// Declare the enum
	info("    frame count: %d", frames.length);
	frames[$-1].enums[enum_data.name] = enum_data;
	info("    declare enum: %s", enum_data.name);

	// Check to see if the name is already used
	check_name_clashes(enum_data.name, enum_data.line, enum_data.column, IdentifierType.enum_);
	// Check to see if the field names are already used
	foreach(field_name, field; enum_data.fields) {
		check_name_clashes(field_name, field.line, field.column, IdentifierType.field_);
	}
}

void add_module(ModuleData module_data) {
	info("add_module");

	// Make sure everything is sane
	assert(module_data !is ModuleData.init, "Module was null");
	assert(module_data.name, "module name was null");
	assert(module_data.name.length, "module name was blank");

	// Skip if declaring same module
	if(module_data.name in modules) {
		auto module_other = modules[module_data.name];
		if(module_other == module_data) {
			info("Tried to redeclare module '%s'.", module_data.name);
			return;
		}
	}

	// Declare the module
	modules[module_data.name] = module_data;
	info("    declare module: %s", module_data.name);
}

Position[][string] get_name_clashes() {
	return name_clashes;
}

void check_name_clashes(string name, size_t line, size_t column, IdentifierType type) {
	auto var_data = get_variable_data_by_name(name);
	auto func_data = get_function_data_by_name(name);
	auto struct_data = get_struct_data_by_name(name);
	auto class_data = get_class_data_by_name(name);
	auto enum_data = get_enum_data_by_name(name);
	size_t old_line, old_column;
	IdentifierType old_type;

	// A variable has that name
	if(var_data !is VariableData.init) {
		old_line = var_data.line;
		old_column = var_data.column;
		old_type = IdentifierType.variable_;
	// A function has that name
	} else if(func_data !is FunctionData.init) {
		old_line = func_data.line;
		old_column = func_data.column;
		old_type = IdentifierType.function_;
	// A struct has that name
	} else if(struct_data !is StructData.init) {
		old_line = struct_data.line;
		old_column = struct_data.column;
		old_type = IdentifierType.struct_;
	// A class has that name
	} else if(class_data !is ClassData.init) {
		old_line = class_data.line;
		old_column = class_data.column;
		old_type = IdentifierType.class_;
	// An enum has that name
	} else if(enum_data !is EnumData.init) {
		old_line = enum_data.line;
		old_column = enum_data.column;
		old_type = IdentifierType.enum_;
	}

	// Check struct fields and methods
	if(old_type == IdentifierType.invalid_) {
		// Each stack frame
		foreach(frame; frames) {
			// Each struct
			foreach(struct_name, struct_data; frame.structs) {
				// Each field
				foreach(field_name, field_data; struct_data.fields) {
					if(field_name == name) {
						old_line = field_data.line;
						old_column = field_data.column;
						old_type = IdentifierType.field_;
					}
				}
				// Each method
				foreach(method_name, method_data; struct_data.methods) {
					if(method_name == name) {
						old_line = method_data.line;
						old_column = method_data.column;
						old_type = IdentifierType.method_;
					}
				}
			}
		}
	}

	// Check class fields and methods
	if(old_type == IdentifierType.invalid_) {
		// Each stack frame
		foreach(frame; frames) {
			// Each class
			foreach(class_name, class_data; frame.classes) {
				// Each field
				foreach(field_name, field_data; class_data.fields) {
					if(field_name == name) {
						old_line = field_data.line;
						old_column = field_data.column;
						old_type = IdentifierType.field_;
					}
				}
				// Each method
				foreach(method_name, method_info; class_data.methods) {
					if(method_name == name) {
						old_line = method_info.line;
						old_column = method_info.column;
						old_type = IdentifierType.method_;
					}
				}
			}
		}
	}

	// Check enum fields
	if(old_type == IdentifierType.invalid_) {
		// Each stack frame
		foreach(frame; frames) {
			// Each enum
			foreach(enum_name, enum_data; frame.enums) {
				// Each field
				foreach(field_name, field_data; enum_data.fields) {
					if(field_name == name) {
						old_line = field_data.line;
						old_column = field_data.column;
						old_type = IdentifierType.field_;
					}
				}
			}
		}
	}

	// Just return if there is nothing declared with that name
	if(old_type == IdentifierType.invalid_)
		return;

	// Save the line and column of the original declaration
	if(name !in name_clashes) {
		name_clashes[name] = [];
		name_clashes[name] ~= Position(old_line, old_column, old_type);
	}

	// It is a redeclaration if the line and column are already used
	bool is_redeclaration = false;
	foreach(pos; name_clashes[name]) {
		if(pos.line == line && pos.column == column && pos.type == type)
			is_redeclaration = true;
	}

	// Save it if it is not a redeclaration
	if(!is_redeclaration) {
		name_clashes[name] ~= Position(line, column, type);
	}
}

