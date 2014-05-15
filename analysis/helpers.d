// Copyright (c) 2014, Matthew Brennan Jones <matthew.brennan.jones@gmail.com>
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.helpers;

import std.stdio;
import std.array;
import std.string;
import std.stdint;
import std.conv;

import std.d.ast;
import std.d.lexer;
import analysis.scope_frame;
import dlang_helper;

const string[] BASIC_TYPES = [
	"byte", "short", "int", "long", 
	"ubyte", "ushort", "uint", "ulong", 
	"size_t", 
	"intptr_t", "uintptr_t", 
	"intmax_t", "uintmax_t"
	"int8_t", "int16_t", "int32_t", "int64_t", 
	"uint8_t", "uint16_t", "uint32_t", "uint64_t", 
	"int_least8_t", "int_least16_t", "int_least32_t", "int_least64_t", 
	"uint_least8_t", "uint_least16_t", "uint_least32_t", "uint_least64_t", 
	"int_fast8_t", "int_fast16_t", "int_fast32_t", "int_fast64_t", 
	"uint_fast8_t", "uint_fast16_t", "uint_fast32_t", "uint_fast64_t", 
	"bool",
	"float", "double", "real", 
	"ifloat", "idouble", "ireal", 
	"char", "wchar", "dchar", 
	"string", "wstring", "dstring"
];

const string[] INTEGER_TYPES = [
	"byte", "short", "int", "long", 
	"ubyte", "ushort", "uint", "ulong", 
	"size_t", 
	"intptr_t", "uintptr_t", 
	"intmax_t", "uintmax_t"
	"int8_t", "int16_t", "int32_t", "int64_t", 
	"uint8_t", "uint16_t", "uint32_t", "uint64_t", 
	"int_least8_t", "int_least16_t", "int_least32_t", "int_least64_t", 
	"uint_least8_t", "uint_least16_t", "uint_least32_t", "uint_least64_t", 
	"int_fast8_t", "int_fast16_t", "int_fast32_t", "int_fast64_t", 
	"uint_fast8_t", "uint_fast16_t", "uint_fast32_t", "uint_fast64_t"
];

const string[] FLOAT_TYPES = [
	"float", "double", "real", 
	"ifloat", "idouble", "ireal"
];

const string[] BOOL_TYPES = [
	"bool"
];

const string[] STRING_TYPES = [
	"string", "wstring", "dstring"
];

const string[] CHAR_TYPES = [
	"char", "wchar", "dchar"
];

	enum TokenType : string {
		invalid = "invalid", 
		null_ = "null", 
		variable = "variable", 
		field = "field", 
		method = "method", 
		this_ = "this", 
		this_field = "this.field", 
		this_method = "this.method", 
		enum_ = "enum", 
		class_ = "class", 
		struct_ = "struct", 
		function_ = "function", 
		basic_type = "basic type", 
		literal = "literal", 
		auto_declaration = "auto", 
		ref_declaration = "ref", 
		property_attribute = "property", 
		__file__ = "__FILE__", 
		__line__ = "__LINE__", 
		super_ = "super", 
		template_ = "template",
	}

	struct TokenData {
		size_t line;
		size_t column;
		TokenType token_type;
		TypeData type_data;
		string name;
		string value;
	}

struct ModuleFunctionSet {
	string import_name;
	string[] functions;
}

// Returns true if a ModuleFunctionSet has a function.
bool has_function(const ModuleFunctionSet func_set, string func_name) {
	bool is_imported = analysis.scope_frame.is_already_imported(func_set.import_name);

	foreach(func; func_set.functions) {
		// If the module is imported the short function name may be used
		if(is_imported && func_name == func) {
			return true;
		}

		// If the module is not imported, the full function name must be used
		string full_func_name = "%s.%s".format(func_set.import_name, func);
		if(func_name == full_func_name) {
			return true;
		}
	}

	return false;
}

void should_warn(string code, analysis.run.AnalyzerCheck analyzers, string file=__FILE__, size_t line=__LINE__) {
	import analysis.run;

	// Reset everything
	scope_frame_clear_everything();

	// Run the code and get any warnings
	string[] raw_warnings = analyze("test", cast(ubyte[]) code, analyzers);
	string[] code_lines = code.split("\n");

	// Get the warnings ordered by line
	string[size_t] warnings;
	for(size_t i=0; i<raw_warnings.length; ++i) {
		size_t warn_line = line - 1 + std.conv.to!size_t(raw_warnings[i].between("test(", ":"));
		warnings[warn_line] = raw_warnings[i].after(")");
//		stderr.writefln("!!! warnings[%d] = \"%s\"", warn_line, warnings[warn_line]);
	}

	// Get all the messages from the comments in the code
	string[size_t] messages;
	foreach(i, code_line; code_lines) {
		// Skip if no [warn] comment
		if(code_line.indexOf("// [warn]:") == -1)
			continue;

		// Skip if there is no comment or code
		string code_part = code_line.before("// ");
		string comment_part = code_line.after("// ");
		if(!code_part.length || !comment_part.length)
			continue;

		// Get the line of this code line
		size_t line_no = i + line;

		// Get the message
//		stderr.writefln("!!! message[%d] = \"%s\"", line_no, comment_part);
		messages[line_no] = comment_part;
	}

	// Throw an assert error if any messages are not listed in the warnings
	foreach(line_no, message; messages) {
//		stderr.writefln("!!!!!! messages[%d] : %s", line_no, messages[line_no]);
		// No warning
		if(line_no !in warnings) {
			string errors = "Expected warning:\n%s\nFrom source code at (%s:?):\n%s".format(
				messages[line_no], 
				line_no, 
				code_lines[line_no - line]
			);
			throw new core.exception.AssertError(errors, file, line_no);
		// Different warning
		} else if(warnings[line_no] != messages[line_no]) {
			string errors = "Expected warning:\n%s\nBut was:\n%s\nFrom source code at (%s:?):\n%s".format(
				messages[line_no], 
				warnings[line_no], 
				line_no, 
				code_lines[line_no - line]
			);
			throw new core.exception.AssertError(errors, file, line_no);
		}
	}

	// Throw an assert error if there were any warnings that were not expected
	string[] unexpected_warnings;
	foreach(line_no, warning; warnings) {
//		stderr.writefln("!!!!!! warnings[%d] : %s", line_no, warning);
		// Unexpected warning
		if(line_no !in messages) {
			unexpected_warnings ~= "%s\nFrom source code at (%s:?):\n%s".format(
				warning, 
				line_no, 
				code_lines[line_no - line]
			);
		}
	}
	if(unexpected_warnings.length) {
		string message = "Unexpected warnings:\n" ~ unexpected_warnings.join("\n");
		throw new core.exception.AssertError(message, file, line);
	}
}

void declare_import(const SingleImport singImpo) {
	info("declare_import");

	// Just return if anything is null
	if(!singImpo && !singImpo.identifierChain) {
		return;
	}

	// Get the whole name from each identifier chunk
	string[] chunks;
	foreach(identifier; singImpo.identifierChain.identifiers) {
		chunks ~= identifier.text;
	}
	string import_name = chunks.join(".");

	// Just return if already imported
	if(is_already_imported(import_name))
		return;

	// Add the import
	stderr.writefln("!!! importing: %s", import_name);
	add_import(import_name);

	// Add the module
	string file_name = "%s.d".format(import_name);
	load_module(file_name);
}

void load_module(string file_name) {
	// Just return if the file does not exist
	if(!std.file.exists(file_name) || !std.file.isFile(file_name))
		return;

	// Read the code
	File f = File(file_name);
	auto code = uninitializedArray!(ubyte[])(to!size_t(f.size));
	f.rawRead(code);

	// Lex the code
	auto lexer = byToken(code);
	auto app = appender!(typeof(lexer.front)[])();
	while(!lexer.empty) {
		app.put(lexer.front);
		lexer.popFront();
	}

	// Parse the code
	auto p = new std.d.parser.ParseAllocator();
	Module mod = std.d.parser.parseModule(app.data, file_name, p, null);

	// Get data from the module
	ModuleData module_data = get_module_data(mod); // FIXME: This splodes
	add_module(module_data);
}

	void declare_function(const FunctionDeclaration funcDec) {
		info("declare_function");

		FunctionData data = get_function_data(funcDec);
		add_function(data);
	}

	void declare_variable(const VariableDeclaration varDec) {
		info("declare_variable");

		foreach(var_data; get_variable_datas(varDec)) {
			add_variable(var_data);
		}
	}

	void declare_parameter(const Parameter param) {
		info("declare_parameter");

		VariableData var_data;
		var_data.name = param.name.text;
		var_data.type = get_type_data(param.type);
		var_data.is_parameter = true;
		var_data.line = param.name.line;
		var_data.column = param.name.column;

		add_variable(var_data);
	}

	void declare_templates(const TemplateParameters tempParams) {
		info("declare_templates");

		TemplateData[] temp_datas = get_template_datas(tempParams);
		foreach(temp_data; temp_datas) {
			add_template_parameter(temp_data);
		}
	}

	void declare_struct(const StructDeclaration structDec) {
		info("declare_struct");

		StructData struct_data = get_struct_data(structDec);
		add_struct(struct_data);
	}

	void declare_class(const ClassDeclaration classDec) {
		info("declare_class");

		ClassData class_data = get_class_data(classDec);
		add_class(class_data);
	}

	void declare_enum(const EnumDeclaration enumDec) {
		info("declare_enum");

		EnumData enum_data = get_enum_data(enumDec);
		add_enum(enum_data);
	}

	void declare_module(const Module mod) {
		info("declare_module");

		ModuleData module_data = get_module_data(mod);
		add_module(module_data);
	}

	// FIXME: For some reason this never sets decoration.is_property to true
	Decoration get_declaration_decorations(const Declaration decl) {
		Decoration decoration;
		foreach(attr; decl.attributes) {
			// Skip if no storage class
			if(!attr.storageClass || attr.storageClass.token is Token.init)
				continue;

			// Reference
			if(attr.storageClass.token.type.str == "ref") {
				decoration.is_ref = true;
			// Auto
			} else if(attr.storageClass.token.type.str == "auto") {
				decoration.is_auto = true;
			}

			// Skip if no at attribute
			if(!attr.storageClass.atAttribute || attr.storageClass.atAttribute.identifier is Token.init)
				continue;

			// Property
			Token iden = attr.storageClass.atAttribute.identifier;
			if(iden.type.str == "identifier" && iden.text == "property") {
				decoration.is_property = true;
			}
		}

		return decoration;
	}

	VariableData[] get_variable_datas(const VariableDeclaration varDec) {
		VariableData[] datas;

		// Using auto
		if(varDec.autoDeclaration) {
			string[] names = get_variable_names(varDec);
			TokenData token_data = get_expression_return_token_data(varDec.autoDeclaration);

			if(token_data is TokenData.init) {
				throw new Exception("Failed to get valid token from auto variable declaration.");
			}

			foreach(name; names) {
				VariableData data;
				data.name = name;
				data.type = token_data.type_data;
				data.is_parameter = false;
				data.line = token_data.line;
				data.column = token_data.column;
				datas ~= data;
			}
		// Normal variable
		} else {
			string[] names = get_variable_names(varDec);
			TypeData type = get_type_data(varDec.type);
			foreach(name; names) {
				VariableData data;
				data.name = name;
				data.type = type;
				data.is_parameter = false;
				get_variable_line_column(varDec, name, data.line, data.column);
				datas ~= data;
			}
		}

		return datas;
	}

	TemplateData[] get_template_datas(const TemplateParameters templateParameters) {
		TemplateData[] datas;
		if(templateParameters && templateParameters.templateParameterList) {
			foreach(item; templateParameters.templateParameterList.items) {
				if(item && item.templateTypeParameter) {
					auto identifier = item.templateTypeParameter.identifier;
					if(identifier !is Token.init) {
						TemplateData data;
						data.name = identifier.text;
						data.line = identifier.line;
						data.column = identifier.column;
						datas ~= data;
					}
				}
			}
		}
		return datas;
	}

	FunctionData get_function_data(const FunctionDeclaration funcDec) {
		FunctionData data;
		data.name = funcDec.name.text;
		data.templates = get_template_datas(funcDec.templateParameters);
		data.return_type = get_function_return_type_data(funcDec);
		data.arg_types = get_function_arg_type_datas(funcDec);
		data.line = funcDec.name.line;
		data.column = funcDec.name.column;
		return data;
	}

	StructData get_struct_data(const StructDeclaration structDec) {
		StructData data;
		data.name = structDec.name.text;
		data.line = structDec.name.line;
		data.column = structDec.name.column;

		foreach(decl; structDec.structBody.declarations) {
			if(decl.variableDeclaration) {
				foreach(var_data; get_variable_datas(decl.variableDeclaration)) {
					data.fields[var_data.name] = var_data;
				}
			} else if(decl.functionDeclaration) {
				FunctionData func_data = get_function_data(decl.functionDeclaration);
				data.methods[func_data.name] = func_data;
			}
		}

		return data;
	}

	ClassData get_class_data(const ClassDeclaration classDec) {
		ClassData data;
		data.name = classDec.name.text;
		data.line = classDec.name.line;
		data.column = classDec.name.column;

		foreach(decl; classDec.structBody.declarations) {
			if(decl.variableDeclaration) {
				foreach(var_data; get_variable_datas(decl.variableDeclaration)) {
					data.fields[var_data.name] = var_data;
				}
			} else if(decl.functionDeclaration) {
				FunctionData func_data = get_function_data(decl.functionDeclaration);
				data.methods[func_data.name] = func_data;
			}
		}
		return data;
	}

	EnumData get_enum_data(const EnumDeclaration enumDec) {
		EnumData data;
		data.name = enumDec.name.text;
		data.line = enumDec.name.line;
		data.column = enumDec.name.column;
		data.type = TypeData("int");
		try {
			data.type = get_type_data(enumDec.type);
		} catch(Exception ex) {
			//
		}

		foreach(member; enumDec.enumBody.enumMembers) {
			if(member) {
				string name = member.name.text;
				FieldData field_data;
				field_data.line = member.name.line;
				field_data.column = member.name.column;
				data.fields[name] = field_data;
			}
		}

		return data;
	}

	ModuleData get_module_data(const Module mod) {
		ModuleData data;

		// FIXME: What do we do when a module has no name?
		// Get the module name
		if(mod && 
			mod.moduleDeclaration && 
			mod.moduleDeclaration.moduleName &&
			mod.moduleDeclaration.moduleName.identifiers.length) {
			string[] mod_name_chunks;
			foreach(identifier; mod.moduleDeclaration.moduleName.identifiers) {
				mod_name_chunks ~= identifier.text;
			}
			data.name = mod_name_chunks.join(".");
		} else {
			data.name = "unknown";
		}

		foreach(decl; mod.declarations) {

			// Add decorations such as properties, auto, ref, et cetera
			Decoration decoration = get_declaration_decorations(decl);
			decorations.push(decoration);

			if(decl.functionDeclaration) {
				FunctionData func_data = get_function_data(decl.functionDeclaration);
				data.functions[func_data.name] = func_data;
			} else if(decl.variableDeclaration) {
				foreach(var_data; get_variable_datas(decl.variableDeclaration)) {
					data.variables[var_data.name] = var_data;
				}
			} else if(decl.structDeclaration) {
				StructData struct_data = get_struct_data(decl.structDeclaration);
				data.structs[struct_data.name] = struct_data;
			} else if(decl.classDeclaration) {
				ClassData class_data = get_class_data(decl.classDeclaration);
				data.classes[class_data.name] = class_data;
			} else if(decl.enumDeclaration) {
				EnumData enum_data = get_enum_data(decl.enumDeclaration);
				data.enums[enum_data.name] = enum_data;
			}

			// Remove decorations
			decorations.pop();
		}

		return data;
	}

	TokenData[] get_function_call_arguments(const FunctionCallExpression funcCallExp) {
		TokenData[] args;

		// Just return if there are no args
		if(!funcCallExp || 
			!funcCallExp.arguments || 
			!funcCallExp.arguments.argumentList || 
			!funcCallExp.arguments.argumentList.items) {
			return args;
		}

		// Get the token data for each arg
		foreach(item; funcCallExp.arguments.argumentList.items) {
			args ~= get_expression_return_token_data(item);
		}

		return args;
	}

	string get_function_call_name(const FunctionCallExpression funcExp) {
		string[] chunks;
		auto unaryExp = cast(UnaryExpression) funcExp.unaryExpression;
		while(unaryExp) {
			// Part of the name
			if(unaryExp.identifierOrTemplateInstance) {
				// from a template
				if(unaryExp.identifierOrTemplateInstance.templateInstance) {
					chunks ~= unaryExp.identifierOrTemplateInstance.templateInstance.identifier.text;
				// from a function
				} else {
					chunks ~= unaryExp.identifierOrTemplateInstance.identifier.text;
				}
			}

			// The end of the name
			if(unaryExp.primaryExpression && 
				unaryExp.primaryExpression.identifierOrTemplateInstance) {
				chunks ~= unaryExp.primaryExpression.identifierOrTemplateInstance.identifier.text;
			}

			unaryExp = unaryExp.unaryExpression;
		}

		if(!chunks.length) {
			throw new Exception("Could not get name of function to call.");
		}

		return chunks.reverse.join(".");
	}

	TypeData get_function_return_type_data(const FunctionDeclaration funcDec) {
		// Normal return type
		if(funcDec.returnType) {
			return get_type_data(funcDec.returnType);
		}

		// Auto return type
		auto decoration = analysis.scope_frame.decorations.peak;
		if(decoration !is Decoration.init && decoration.is_auto) {
			return TypeData("auto");
		}

		return TypeData.init;
	}

	TypeData[] get_function_arg_type_datas(const FunctionDeclaration funcDec) {
		if(funcDec && 
			funcDec.parameters) {
			TypeData[] arg_types;
			foreach(param; funcDec.parameters.parameters) {
				arg_types ~= get_type_data(param.type);
			}
			return arg_types;
		}

		throw new Exception("Could not find function arg type names.");
	}

	string[] get_function_arg_names(const FunctionDeclaration funcDec) {
		if(funcDec && 
			funcDec.parameters) {
			string[] arg_names;
			foreach(param; funcDec.parameters.parameters) {
				arg_names ~= param.name.text;
			}
			return arg_names;
		}

		throw new Exception("Could not find function arg names.");
	}

	string[] get_variable_names(const VariableDeclaration varDec) {
		string[] retval;
		if(varDec) {
			if(varDec.autoDeclaration) {
				foreach(iden; varDec.autoDeclaration.identifiers) {
					retval ~= iden.text;
				}
			} else {
				foreach(d; varDec.declarators) {
					retval ~= d.name.text;
				}
			}
		}

		if(retval.length)
			return retval;

		throw new Exception("Could not find variable names.");
	}

	void get_variable_line_column(const VariableDeclaration varDec, string name, ref size_t line, ref size_t column) {
		if(varDec) {
			if(varDec.autoDeclaration) {
				foreach(iden; varDec.autoDeclaration.identifiers) {
					if(iden.text == name) {
						line = iden.line;
						column = iden.column;
						return;
					}
				}
			}

			foreach(d; varDec.declarators) {
				if(d.name.text == name) {
					line = d.name.line;
					column = d.name.column;
					return;
				}
			}
		}

		throw new Exception("Could not find variable line and column.");
	}

	void mark_used_variables(const ASTNode node) {
		auto unaryExp = cast(const UnaryExpression) node;
		auto ternaryExp = cast(const TernaryExpression) node;
		if(unaryExp || ternaryExp) {
			TokenData data;
			if(unaryExp)
				data = get_expression_return_token_data(unaryExp);
			else
				data = get_expression_return_token_data(ternaryExp);
			//stderr.writefln("!!! token_data token_type:%s, data_type:%s, name:%s", data.token_type, data.type_data, data.name);
			if(data !is TokenData.init && data.name && 
				(data.token_type == TokenType.variable || data.token_type == TokenType.field || data.token_type == TokenType.method)) {
				string name = data.name.before(".");
				set_variable_is_used_by_name(name);
			}
		} else if(auto exp = cast(const AssignExpression) node) {
			mark_used_variables(exp.ternaryExpression);
		} else if(auto exp = cast(const AssertExpression) node) {
			mark_used_variables(exp.assertion);
		} else if(auto exp = cast(const CmpExpression) node) {
			mark_used_variables(exp.shiftExpression);
			mark_used_variables(exp.equalExpression);
			mark_used_variables(exp.identityExpression);
			mark_used_variables(exp.relExpression);
			mark_used_variables(exp.inExpression);
		} else if(auto exp = cast(AddExpression) node) {
			mark_used_variables(exp.left);
			mark_used_variables(exp.right);
		} else if(auto exp = cast(AndAndExpression) node) {
			mark_used_variables(exp.left);
			mark_used_variables(exp.right);
		} else if(auto exp = cast(AndExpression) node) {
			mark_used_variables(exp.left);
			mark_used_variables(exp.right);
		} else if(auto exp = cast(EqualExpression) node) {
			mark_used_variables(exp.left);
			mark_used_variables(exp.right);
		} else if(auto exp = cast(IdentityExpression) node) {
			mark_used_variables(exp.left);
			mark_used_variables(exp.right);
		} else if(auto exp = cast(InExpression) node) {
			mark_used_variables(exp.left);
			mark_used_variables(exp.right);
		} else if(auto exp = cast(MulExpression) node) {
			mark_used_variables(exp.left);
			mark_used_variables(exp.right);
		} else if(auto exp = cast(OrExpression) node) {
			mark_used_variables(exp.left);
			mark_used_variables(exp.right);
		} else if(auto exp = cast(OrOrExpression) node) {
			mark_used_variables(exp.left);
			mark_used_variables(exp.right);
		} else if(auto exp = cast(PowExpression) node) {
			mark_used_variables(exp.left);
			mark_used_variables(exp.right);
		} else if(auto exp = cast(RelExpression) node) {
			mark_used_variables(exp.left);
			mark_used_variables(exp.right);
		} else if(auto exp = cast(ShiftExpression) node) {
			mark_used_variables(exp.left);
			mark_used_variables(exp.right);
		} else if(auto exp = cast(XorExpression) node) {
			mark_used_variables(exp.left);
			mark_used_variables(exp.right);
		} else if(node) {
			throw new Exception("!!! mark_used_variables() override needed for: %s".format(typeid(node)));
		}
	}

	TypeData get_type_data(const Type type) {
		TypeData type_data;
		if(type && type.type2) {
			// Check for a nested type
			if(type.type2.type)
				return get_type_data(type.type2.type);

			// Check if it is an array
			foreach(suffix; type.typeSuffixes) {
				if(suffix.array)
					type_data.is_array = true;
			}

			// Get type from builtinType
			if(type.type2.builtinType) {
				type_data.name = type.type2.builtinType.str;
				return type_data;
			}

			// Get type from symbol
			if(type.type2.symbol && 
				type.type2.symbol.identifierOrTemplateChain) {

				auto chain = type.type2.symbol.identifierOrTemplateChain.identifiersOrTemplateInstances;
				string[] identifiers;
				foreach(idenOrTemp; chain) {
					identifiers ~= idenOrTemp.identifier.text;
				}
				type_data.name = identifiers.join(".");
				return type_data;
			}
		}
		//std.d.inspect.inspect(type, 0, "type");
		throw new Exception("Could not find type name.");
	}

	bool is_same_token_variable(const TokenData a, const TokenData b) {
		return 
			a !is TokenData.init && b !is TokenData.init && 
			(a.token_type == TokenType.variable && b.token_type == TokenType.variable || 
			a.token_type == TokenType.field && b.token_type == TokenType.field) && 
			a.name && b.name && 
			a.name == b.name;
	}

	TokenData get_expression_return_token_data(const ASTNode node) {
		Token token = get_expression_return_token(node, 0);
		TokenData data = get_token_data(token);
		//stderr.writefln("!!!!!! %s, %s, %s, %s, %s", data.token_type, data.type_data, data.name, data.line, data.column);
		return data;
	}

	TypeData get_expression_return_type(const ASTNode node, ref size_t line, ref size_t column) {
		TokenData data = get_expression_return_token_data(node);
		line = data.line;
		column = data.column;
		return data.type_data;
	}

	Token get_expression_return_token(const ASTNode node, size_t indent) {
		//stderr.writefln("%s??? get_expression_return_token: %s", pad(indent++), typeid(node));

		if(auto addExp = cast(const AddExpression) node) {
			auto l = get_expression_return_token(addExp.left, indent);
			auto r = get_expression_return_token(addExp.right, indent);
			return get_promoted_token(l, r);
		} else if(auto andAndExp = cast(const AndAndExpression) node) {
			auto l = get_expression_return_token(andAndExp.left, indent);
			auto r = get_expression_return_token(andAndExp.right, indent);
			return Token(tok!"identifier", "bool", l.line, l.column, l.index);
		} else if(auto andExp = cast(const AndExpression) node) {
			auto l = get_expression_return_token(andExp.left, indent);
			auto r = get_expression_return_token(andExp.right, indent);
			return get_promoted_token(l, r);
		} else if(auto arrayInit = cast(const ArrayInitializer) node) {
			foreach(memberInit; arrayInit.arrayMemberInitializations) {
				return get_expression_return_token(memberInit, indent);
			}
		} else if(auto arrayLit = cast(const ArrayLiteral) node) {
			return get_expression_return_token(arrayLit.argumentList, indent);
		} else if(auto arrayMemInit = cast(const ArrayMemberInitialization) node) {
			if(arrayMemInit.assignExpression)
				return get_expression_return_token(arrayMemInit.assignExpression, indent);
			else if(arrayMemInit.nonVoidInitializer)
				return get_expression_return_token(arrayMemInit.nonVoidInitializer, indent);
		} else if(auto argList = cast(const ArgumentList) node) {
			foreach(item; argList.items) {
				return get_expression_return_token(item, indent);
			}
		} else if(auto asserExp = cast(const AssertExpression) node) {
			return get_expression_return_token(asserExp.assertion, indent);
		} else if(auto assExp = cast(const AssignExpression) node) {
			if(assExp.ternaryExpression) {
				return get_expression_return_token(assExp.ternaryExpression, indent);
			} else if(assExp.assignExpression) {
				return get_expression_return_token(assExp.assignExpression, indent);
			}
		} else if(auto autoDec = cast(const AutoDeclaration) node) {
//			foreach(iden; autoDec.identifiers)
//				if(iden)
//					return get_expression_return_token(iden, indent);

			foreach(init; autoDec.initializers)
				if(init)
					return get_expression_return_token(init, indent);
		} else if(auto castExp = cast(const CastExpression) node) {
			if(castExp.type)
				return get_expression_return_token(castExp.type, indent);
			if(castExp.castQualifier)
				return get_expression_return_token(castExp.castQualifier, indent);
			if(castExp.unaryExpression)
				return get_expression_return_token(castExp.unaryExpression, indent);
		} else if(auto cmpExp = cast(const CmpExpression) node) {
			if(cmpExp.shiftExpression)
				return get_expression_return_token(cmpExp.shiftExpression, indent);
			else if(cmpExp.equalExpression)
				return get_expression_return_token(cmpExp.equalExpression, indent);
			else if(cmpExp.identityExpression)
				return get_expression_return_token(cmpExp.identityExpression, indent);
			else if(cmpExp.relExpression)
				return get_expression_return_token(cmpExp.relExpression, indent);
			else if(cmpExp.inExpression)
				return get_expression_return_token(cmpExp.inExpression, indent);
		} else if(auto decl = cast(const Declarator) node) {
			if(decl.initializer) {
				return get_expression_return_token(decl.initializer, indent);
			} else {
				return decl.name;
			}
		} else if(auto delExp = cast(const DeleteExpression) node) {
			if(delExp.unaryExpression)
				return get_expression_return_token(delExp.unaryExpression, indent);
		} else if(auto eqlExp = cast(const EqualExpression) node) {
			auto l = get_expression_return_token(eqlExp.left, indent);
			auto r = get_expression_return_token(eqlExp.right, indent);
			return Token(tok!"identifier", "bool", l.line, l.column, l.index);
		} else if(auto expExp = cast(const Expression) node) {
			foreach(item; expExp.items)
				if(item)
					return get_expression_return_token(item, indent);
		} else if(auto funCallExp = cast(const FunctionCallExpression) node) {
			// FIXME: This breaks with UFC
			if(funCallExp.unaryExpression)
				return get_expression_return_token(funCallExp.unaryExpression, indent);
		} else if(auto idenOrTemp = cast(const IdentifierOrTemplateChain) node) {
			if(idenOrTemp)
				foreach(inst; idenOrTemp.identifiersOrTemplateInstances)
					return get_expression_return_token(inst, indent);
		} else if(auto idenOrTemp = cast(const IdentifierOrTemplateInstance) node) {
			if(idenOrTemp.templateInstance) {
				return get_expression_return_token(idenOrTemp.templateInstance, indent);
			} else {
				return idenOrTemp.identifier;
			}
		} else if(auto idenExp = cast(const IdentityExpression) node) {
			auto l = get_expression_return_token(idenExp.left, indent);
			auto r = get_expression_return_token(idenExp.right, indent);
			return Token(tok!"identifier", "bool", l.line, l.column, l.index);
		} else if(auto impExp = cast(const ImportExpression) node) {
			if(impExp.assignExpression)
				return get_expression_return_token(impExp.assignExpression, indent);
		} else if(auto indexExp = cast(const IndexExpression) node) {
			if(indexExp.unaryExpression)
				return get_expression_return_token(indexExp.unaryExpression, indent);
		} else if(auto inExp = cast(const InExpression) node) {
			auto l = get_expression_return_token(inExp.left, indent);
			auto r = get_expression_return_token(inExp.right, indent);
			return Token(tok!"identifier", "bool", l.line, l.column, l.index);
		} else if(auto intl = cast(const Initializer) node) {
			if(intl.nonVoidInitializer)
				return get_expression_return_token(intl.nonVoidInitializer, indent);
		} else if(auto mulExp = cast(const MulExpression) node) {
			auto l = get_expression_return_token(mulExp.left, indent);
			auto r = get_expression_return_token(mulExp.right, indent);
			return get_promoted_token(l, r);
		} else if(auto newExp = cast(const NewExpression) node) {
			if(newExp.type)
				return get_expression_return_token(newExp.type, indent);
			if(newExp.newAnonClassExpression)
				return get_expression_return_token(newExp.newAnonClassExpression, indent);
			//else if(newExp.arguments)
			//	return get_expression_return_token(newExp.arguments, indent);
			if(newExp.assignExpression)
				return get_expression_return_token(newExp.assignExpression, indent);
		} else if(auto nonVoidIntl = cast(const NonVoidInitializer) node) {
			if(nonVoidIntl.assignExpression)
				return get_expression_return_token(nonVoidIntl.assignExpression, indent);
			if(nonVoidIntl.arrayInitializer)
				return get_expression_return_token(nonVoidIntl.arrayInitializer, indent);
			if(nonVoidIntl.structInitializer)
				return get_expression_return_token(nonVoidIntl.structInitializer, indent);
		} else if(auto orExp = cast(const OrExpression) node) {
			auto l = get_expression_return_token(orExp.left, indent);
			auto r = get_expression_return_token(orExp.right, indent);
			return get_promoted_token(l, r);
		} else if(auto orOrExp = cast(const OrOrExpression) node) {
			auto l = get_expression_return_token(orOrExp.left, indent);
			auto r = get_expression_return_token(orOrExp.right, indent);
			return Token(tok!"identifier", "bool", l.line, l.column, l.index);
		} else if(auto postIncDecExp = cast(const PostIncDecExpression) node) {
			if(postIncDecExp.unaryExpression)
				return get_expression_return_token(postIncDecExp.unaryExpression, indent);
		} else if(auto powExp = cast(const PowExpression) node) {
			auto l = get_expression_return_token(powExp.left, indent);
			auto r = get_expression_return_token(powExp.right, indent);
			return get_promoted_token(l, r);
		} else if(auto preIncDecExp = cast(const PreIncDecExpression) node) {
			if(preIncDecExp.unaryExpression)
				return get_expression_return_token(preIncDecExp.unaryExpression, indent);
		} else if(auto primaryExp = cast(const PrimaryExpression) node) {
			if(primaryExp.identifierOrTemplateInstance)
				return get_expression_return_token(primaryExp.identifierOrTemplateInstance, indent);
			if(primaryExp.typeofExpression)
				return get_expression_return_token(primaryExp.typeofExpression, indent);
			if(primaryExp.typeidExpression)
				return get_expression_return_token(primaryExp.typeidExpression, indent);
			if(primaryExp.arrayLiteral)
				return get_expression_return_token(primaryExp.arrayLiteral, indent);
			if(primaryExp.assocArrayLiteral)
				return get_expression_return_token(primaryExp.assocArrayLiteral, indent);
			if(primaryExp.expression)
				return get_expression_return_token(primaryExp.expression, indent);
			if(primaryExp.isExpression)
				return get_expression_return_token(primaryExp.isExpression, indent);
			if(primaryExp.lambdaExpression)
				return get_expression_return_token(primaryExp.lambdaExpression, indent);
			if(primaryExp.functionLiteralExpression)
				return get_expression_return_token(primaryExp.functionLiteralExpression, indent);
			if(primaryExp.traitsExpression)
				return get_expression_return_token(primaryExp.traitsExpression, indent);
			if(primaryExp.mixinExpression)
				return get_expression_return_token(primaryExp.mixinExpression, indent);
			if(primaryExp.importExpression)
				return get_expression_return_token(primaryExp.importExpression, indent);
			if(primaryExp.vector)
				return get_expression_return_token(primaryExp.vector, indent);

			// return type
			if(get_token_data(primaryExp.dot) !is TokenData.init)
				return primaryExp.dot;
			if(get_token_data(primaryExp.primary) !is TokenData.init)
				return primaryExp.primary;
			if(get_token_data(primaryExp.basicType) !is TokenData.init)
				return primaryExp.basicType;

			string message = std.string.format(
				"!!! Unexpected token:\n"
				"    dot:\"%s\": \"%s\"\n"
				"    primary: \"%s\":\"%s\"\n"
				"    basicType: \"%s\":\"%s\"\n",
				primaryExp.dot.type.str, 
				primaryExp.dot.text, 
				primaryExp.primary.type.str, 
				primaryExp.primary.text, 
				primaryExp.basicType.type.str, 
				primaryExp.basicType.text, 
			);
			throw new Exception(message);
		} else if(auto relExp = cast(const RelExpression) node) {
			auto l = get_expression_return_token(relExp.left, indent);
			auto r = get_expression_return_token(relExp.right, indent);
			return Token(tok!"identifier", "bool", l.line, l.column, l.index);
		} else if(auto shiftExp = cast(const ShiftExpression) node) {
			auto l = get_expression_return_token(shiftExp.left, indent);
			auto r = get_expression_return_token(shiftExp.right, indent);
			return get_promoted_token(l, r);
		} else if(auto sliceExp = cast(const SliceExpression) node) {
			if(sliceExp.unaryExpression)
				return get_expression_return_token(sliceExp.unaryExpression, indent);
		} else if(auto symbol = cast(const Symbol) node) {
			if(symbol.identifierOrTemplateChain)
				return get_expression_return_token(symbol.identifierOrTemplateChain, indent);
		} else if(auto tempArgList = cast(const TemplateArgumentList) node) {
			foreach(item; tempArgList.items) {
				return get_expression_return_token(item, indent);
			}
		} else if(auto tempArg = cast(const TemplateArgument) node) {
			if(tempArg.type)
				return get_expression_return_token(tempArg.type, indent);
			if(tempArg.assignExpression)
				return get_expression_return_token(tempArg.assignExpression, indent);
		} else if(auto tempArgs = cast(const TemplateArguments) node) {
			if(tempArgs.templateArgumentList)
				return get_expression_return_token(tempArgs.templateArgumentList, indent);
			if(tempArgs.templateSingleArgument)
				return get_expression_return_token(tempArgs.templateSingleArgument, indent);
		} else if(auto tempIns = cast(const TemplateInstance) node) {
			if(tempIns.templateArguments)
				return get_expression_return_token(tempIns.templateArguments, indent);
			else
				return tempIns.identifier;
		} else if(auto ternaryExp = cast(const TernaryExpression) node) {
			if(ternaryExp.orOrExpression)
				return get_expression_return_token(ternaryExp.orOrExpression, indent);
			if(ternaryExp.expression)
				return get_expression_return_token(ternaryExp.expression, indent);
			if(ternaryExp.ternaryExpression)
				return get_expression_return_token(ternaryExp.ternaryExpression, indent);
		} else if(auto tempSingArg = cast(const TemplateSingleArgument) node) {
			if(get_token_data(tempSingArg.token) !is TokenData.init)
				return tempSingArg.token;
		} else if(auto type = cast(const Type) node) {
			if(type.type2)
				return get_expression_return_token(type.type2, indent);
		} else if(auto type2 = cast(const Type2) node) {
			if(type2.symbol)
				return get_expression_return_token(type2.symbol, indent);
			if(type2.typeofExpression)
				return get_expression_return_token(type2.typeofExpression, indent);
			if(type2.identifierOrTemplateChain)
				return get_expression_return_token(type2.identifierOrTemplateChain, indent);
			if(type2.type)
				return get_expression_return_token(type2.type, indent);

			if(type2.builtinType.str != "!ERROR!") {
				return Token(
					tok!"identifier", type2.builtinType.str, 
					// FIXME: Get the real line and column
					0, 0, 0);
			}
			//if(type2.typeConstructor.str != "!ERROR!")
			//	return type2.typeConstructor.str;
		} else if(auto typeidExp = cast(const TypeidExpression) node) {
			if(typeidExp.expression)
				return get_expression_return_token(typeidExp.expression, indent);
			if(typeidExp.type)
				return get_expression_return_token(typeidExp.type, indent);
		} else if(auto typeofExp = cast(const TypeofExpression) node) {
			if(typeofExp.expression)
				return get_expression_return_token(typeofExp.expression, indent);
			//if(typeofExp.return_)
			//	return get_expression_return_token(typeofExp.return_, indent);
		} else if(auto unaryExp = cast(const UnaryExpression) node) {
			// Get the prefix such as "this."
			Token firstToken, secondToken;
			if(unaryExp.unaryExpression) {
				firstToken = get_expression_return_token(unaryExp.unaryExpression, indent);
			}

			// Get the second token
			if(unaryExp.type)
				secondToken = get_expression_return_token(unaryExp.type, indent);
			if(unaryExp.primaryExpression)
				secondToken = get_expression_return_token(unaryExp.primaryExpression, indent);
			//if(unaryExp.prefix)
			//	secondToken = get_expression_return_token(unaryExp.prefix, indent);
			//if(unaryExp.suffix)
			//	secondToken = get_expression_return_token(unaryExp.suffix, indent);
			if(unaryExp.newExpression)
				secondToken = get_expression_return_token(unaryExp.newExpression, indent);
			if(unaryExp.deleteExpression)
				secondToken = get_expression_return_token(unaryExp.deleteExpression, indent);
			if(unaryExp.castExpression)
				secondToken = get_expression_return_token(unaryExp.castExpression, indent);
			if(unaryExp.functionCallExpression)
				secondToken = get_expression_return_token(unaryExp.functionCallExpression, indent);
			if(unaryExp.argumentList)
				secondToken = get_expression_return_token(unaryExp.argumentList, indent);
			if(unaryExp.identifierOrTemplateInstance)
				secondToken = get_expression_return_token(unaryExp.identifierOrTemplateInstance, indent);
			if(unaryExp.assertExpression)
				secondToken = get_expression_return_token(unaryExp.assertExpression, indent);
			if(unaryExp.sliceExpression)
				secondToken = get_expression_return_token(unaryExp.sliceExpression, indent);
			if(unaryExp.indexExpression)
				secondToken = get_expression_return_token(unaryExp.indexExpression, indent);

			// Combine the tokens
			//stderr.writefln("!!! get_expression_return_token firstToken type:%s, text:%s", firstToken.type.str, firstToken.text);
			//stderr.writefln("!!! get_expression_return_token right type:%s, text:%s", secondToken.type.str, secondToken.text);
			// FIXME: UFC Boom
			Token newToken = combine_tokens(firstToken, secondToken);
			//stderr.writefln("!!! t type:%s, text:%s", newToken.type.str, newToken.text);
			return newToken;
		} else if(auto xorExp = cast(const XorExpression) node) {
			auto l = get_expression_return_token(xorExp.left, indent);
			auto r = get_expression_return_token(xorExp.right, indent);
			return get_promoted_token(l, r);
		}

		if(node !is null) {
			stderr.writefln("!!! get_expression_return_token() failed on node: %s", typeid(node));
		}

		return Token.init;
	}

	const Token get_promoted_token(const Token left, const Token right) {
		const int[string] sizes = [
			"bool" : bool.sizeof, 
			"char" : char.sizeof, 
			"wchar" : wchar.sizeof, 
			"dchar" : dchar.sizeof, 
			"byte" : byte.sizeof, 
			"short" : short.sizeof, 
			"int" : int.sizeof, 
			"long" : long.sizeof, 
			"ubyte" : ubyte.sizeof, 
			"ushort" : ushort.sizeof, 
			"uint" : uint.sizeof, 
			"ulong" : ulong.sizeof, 
			"size_t" : size_t.sizeof, 
			"float" : float.sizeof, 
			"double" : double.sizeof, 

			"int8_t" : int8_t.sizeof, 
			"int16_t" : int16_t.sizeof, 
			"int32_t" : int32_t.sizeof, 
			"int64_t" : int64_t.sizeof, 
			"uint8_t" : uint8_t.sizeof, 
			"uint16_t" : uint16_t.sizeof, 
			"uint32_t" : uint32_t.sizeof, 
			"uint64_t" : uint64_t.sizeof, 

			"int_least8_t" : int_least8_t.sizeof, 
			"int_least16_t" : int_least16_t.sizeof, 
			"int_least32_t" : int_least32_t.sizeof, 
			"int_least64_t" : int_least64_t.sizeof, 
			"uint_least8_t" : uint_least8_t.sizeof, 
			"uint_least16_t" : uint_least16_t.sizeof, 
			"uint_least32_t" : uint_least32_t.sizeof, 
			"uint_least64_t" : uint_least64_t.sizeof, 

			"int_fast8_t" : int_fast8_t.sizeof, 
			"int_fast16_t" : int_fast16_t.sizeof, 
			"int_fast32_t" : int_fast32_t.sizeof, 
			"int_fast64_t" : int_fast64_t.sizeof, 
			"uint_fast8_t" : uint_fast8_t.sizeof, 
			"uint_fast16_t" : uint_fast16_t.sizeof, 
			"uint_fast32_t" : uint_fast32_t.sizeof, 
			"uint_fast64_t" : uint_fast64_t.sizeof, 
		];

		const string[string] promotions = [
			"bool" : "int", 
			"char" : "int", 
			"wchar" : "int", 
			"dchar" : "uint", 
			"byte" : "int", 
			"short" : "int", 
			"int" : "int", 
			"long" : "long", 
			"ubyte" : "int", 
			"ushort" : "int", 
			"uint" : "uint", 
			"ulong" : "ulong", 
			"size_t" : "size_t", 
			"float" : "float", 
			"double" : "double", 

			"int8_t" : "int", 
			"int16_t" : "int", 
			"int32_t" : "int", 
			"int64_t" : "long", 
			"uint8_t" : "int", 
			"uint16_t" : "int", 
			"uint32_t" : "uint", 
			"uint64_t" : "ulong", 

			"int_least8_t" : "int", 
			"int_least16_t" : "int", 
			"int_least32_t" : "int", 
			"int_least64_t" : "long", 
			"uint_least8_t" : "int", 
			"uint_least16_t" : "int", 
			"uint_least32_t" : "uint", 
			"uint_least64_t" : "ulong", 

			"int_fast8_t" : "int", 
			"int_fast16_t" : "int", 
			"int_fast32_t" : "int", 
			"int_fast64_t" : "long", 
			"uint_fast8_t" : "int", 
			"uint_fast16_t" : "int", 
			"uint_fast32_t" : "uint", 
			"uint_fast64_t" : "ulong", 
		];

		string a = get_token_data(left).type_data.name;
		string b = get_token_data(right).type_data.name;

		// throw an error if any type names are blank
		if(a is null || b is null || a == "" || b == "") {
			string message = "get_promoted_token() did not expect: \"%s\" or \"%s\".".format(
				get_token_data(left), get_token_data(right));
			throw new Exception(message);
		}

		// throw an error if any type names are unknown
		if(a !in promotions || a !in sizes ||
			b !in promotions || b !in sizes) {

			string message = "get_promoted_token() did not expect the type name: \"%s\" or \"%s\".".format(a, b);
			throw new Exception(message);
		}

		// Types are the same, so return the promotion of one
		if(a == b) {
			return Token(tok!"identifier", promotions[a], left.line, left.column, left.index);
		}

		// Types are different, so return the promotion of the larger
		if(sizes[a] > sizes[b]) {
			return Token(tok!"identifier", promotions[a], left.line, left.column, left.index);
		} else {
			return Token(tok!"identifier", promotions[b], right.line, right.column, right.index);
		}
	}

	// FIXME: Make this work with UFC for variables and literals
	TokenData get_token_data(const Token token) {
		TokenData data;
		//stderr.writefln("!!! get_token_data type:%s text:%s", token.type.str, token.text);

		// Line and column
		if(token.line && token.column) {
			data.line = token.line;
			data.column = token.column;
		}

		// Token is empty
		if(token is Token.init || token.type.str == "!ERROR!") {
			return TokenData.init;
		}

		// Token is null
		if(token.type == tok!"null") {
			data.token_type = TokenType.null_;
			data.name = null;
			data.type_data = TypeData.init;
			return data;
		}

		const string[ubyte] LITERAL_TYPE_TO_TYPE_MAP = [
			tok!"doubleLiteral" : "double", 
			tok!"idoubleLiteral" : "idouble", 
			tok!"floatLiteral" : "float", 
			tok!"ifloatLiteral" : "ifloat", 
			tok!"intLiteral" : "int", 
			tok!"uintLiteral" : "uint", 
			tok!"longLiteral" : "long", 
			tok!"ulongLiteral" : "ulong", 
			tok!"realLiteral" : "real", 
			tok!"irealLiteral" : "ireal", 
			tok!"characterLiteral" : "char", 
			tok!"stringLiteral" : "string", 
			tok!"dstringLiteral" : "dstring", 
			tok!"wstringLiteral" : "wstring"
		];

		// Token is a literal
		if(token.type in LITERAL_TYPE_TO_TYPE_MAP) {
			string type_name = LITERAL_TYPE_TO_TYPE_MAP[token.type];
			data.type_data = TypeData(type_name);
			data.token_type = TokenType.literal;
			data.name = null;
			data.value = token.text;
			return data;
		}

		// Token is a bool literal
		if(token.type == tok!"true" || token.type == tok!"false") {
			data.token_type = TokenType.literal;
			data.type_data = TypeData("bool");
			data.name = null;
			data.value = token.type.str;
			return data;
		}

		// Token is a basic type
		if(token.type == tok!"identifier") {
			if(!std.algorithm.find(BASIC_TYPES, token.text).empty) {
				data.token_type = TokenType.basic_type;
				data.type_data = TypeData(token.text);
				data.name = null;
				return data;
			}
		}

		// Token is an auto declaration
		if(token.type ==  tok!"auto") {
			data.token_type = TokenType.auto_declaration;
			data.type_data = TypeData.init;
			data.name = null;
			return data;
		}

		// Token is an ref declaration
		if(token.type ==  tok!"ref") {
			data.token_type = TokenType.ref_declaration;
			data.type_data = TypeData.init;
			data.name = null;
			return data;
		}

		// Token is __FILE__
		if(token.type == tok!"__FILE__") {
			data.token_type = TokenType.__file__;
			data.name = null;
			data.type_data = TypeData("string");
			return data;
		}

		// Token is __LINE__
		if(token.type == tok!"__LINE__") {
			data.token_type = TokenType.__line__;
			data.name = null;
			data.type_data = TypeData("int");
			return data;
		}

		// Token is super
		if(token.type == tok!"super") {
			data.token_type = TokenType.super_;
			data.name = null;
			data.type_data = TypeData.init;
			return data;
		}

		// Token is identifier with "this pointer" prefix
		// this.blah
		if(token.type == tok!"this" && token.text && token.text.length && this_pointers.peak) {
			string member = token.text;

			// Figure out what "this" is
			auto class_data = get_class_data_by_name(this_pointers.peak);
			auto struct_data = get_struct_data_by_name(this_pointers.peak);

			// Class
			if(class_data !is ClassData.init) {
				// Token is a field
				if(member in class_data.fields) {
					data.token_type = TokenType.this_field;
					data.name = token.text;
					data.type_data = class_data.fields[member].type;
					return data;
				// Token is a method
				} else if(member in class_data.methods) {
					data.token_type = TokenType.this_method;
					data.name = token.text;
					data.type_data = class_data.methods[member].return_type;
					return data;
				}
			// Struct
			} else if(struct_data !is StructData.init) {
				// Token is a field
				if(member in struct_data.fields) {
					data.token_type = TokenType.this_field;
					data.name = token.text;
					data.type_data = struct_data.fields[member].type;
					return data;
				// Token is a method
				} else if(member in struct_data.methods) {
					data.token_type = TokenType.this_method;
					data.name = token.text;
					data.type_data = struct_data.methods[member].return_type;
					return data;
				}
			}
		}

		// Token is just the "this pointer"
		// this
		if(token.type == tok!"this" && this_pointers.peak) {
			data.token_type = TokenType.this_;
			data.name = "this";
			data.type_data = TypeData(this_pointers.peak);
			return data;
		}

		// Token is an identifier
		if(token.type == tok!"identifier") {
			// If the identifier has a dot "blah.member", then it is an 
			// identifier and member
			string identifier = null;
			string member = null;
			if(token.text.indexOf(".") != -1) {
				identifier = token.text.before(".");
				member = token.text.after(".");
			} else {
				identifier = token.text;
			}

			auto var_data = get_variable_data_by_name(identifier);

			// Token is a struct/class instance
			if(var_data !is VariableData.init) {
				string type_name = var_data.type.name;
				auto class_data = get_class_data_by_name(type_name);
				auto struct_data = get_struct_data_by_name(type_name);

				// Class instance member
				if(member && class_data !is ClassData.init) {
					// Class instance field
					if(member in class_data.fields) {
						data.type_data = class_data.fields[member].type;
						data.name = token.text;
						data.token_type = TokenType.field;
					// Class instance method
					} else if(member in class_data.methods) {
						data.type_data = class_data.methods[member].return_type;
						data.name = token.text;
						data.token_type = TokenType.method;
					}
					return data;
				// Struct instance member
				} else if(member && struct_data !is StructData.init) {
					// Struct instance field
					if(member in struct_data.fields) {
						data.type_data = struct_data.fields[member].type;
						data.name = token.text;
						data.token_type = TokenType.field;
					// Struct instance method
					} else if(member in struct_data.methods) {
						data.type_data = struct_data.methods[member].return_type;
						data.name = token.text;
						data.token_type = TokenType.method;
					}
					return data;
				}
			}

			// Token is a variable with member
			if(var_data !is VariableData.init && member) {
				// Is a standard property
				switch(member) {
					case "alignof":
						data.type_data = TypeData("size_t");
						data.name = token.text;
						data.token_type = TokenType.field;
						return data;
					case "init":
						data.type_data = var_data.type;
						data.name = token.text;
						data.token_type = TokenType.field;
						return data;
					case "mangleof":
						data.type_data = TypeData("string");
						data.name = token.text;
						data.token_type = TokenType.field;
						return data;
					case "sizeof":
						data.type_data = TypeData("size_t");
						data.name = token.text;
						data.token_type = TokenType.field;
						return data;
					case "stringof":
						data.type_data = TypeData("string");
						data.name = token.text;
						data.token_type = TokenType.field;
						return data;
					default:
						break;
				}

				// Is an array
				if(var_data.type.is_array) {
					switch(member) {
						case "length":
							data.type_data = TypeData("size_t");
							data.name = token.text;
							data.token_type = TokenType.field;
							return data;
						// FIXME: case "ptr":
						case "dup":
						case "idup":
						case "reverse":
						case "sort":
							data.type_data = var_data.type;
							data.name = token.text;
							data.token_type = TokenType.field;
							return data;
						default:
							break;
					}
				// Is an integral type
				} else if(!std.algorithm.find(INTEGER_TYPES, identifier).empty) {
					switch(member) {
						case "max":
							data.type_data = TypeData("size_t");
							data.name = token.text;
							data.token_type = TokenType.field;
							return data;
						case "min":
							data.type_data = TypeData("size_t");
							data.name = token.text;
							data.token_type = TokenType.field;
							return data;
						default:
							break;
					}
				// Is a float type
				} else if(!std.algorithm.find(FLOAT_TYPES, identifier).empty) {
					switch(member) {
						case "infinity":
						case "nan":
						case "dig":
						case "epsilon":
						case "mant_dig":
						case "max_10_exp":
						case "max_exp":
						case "min_10_exp":
						case "min_exp":
						case "max":
						case "min_normal":
						case "re":
						case "im":
							data.type_data = var_data.type;
							data.name = token.text;
							data.token_type = TokenType.field;
							return data;
						default:
							break;
					}
				}
				throw new Exception("!!! identifier:%s, member:%s, var:%s, type:%s".format(identifier, member, var_data, var_data.type));
			}

			// Token is a variable
			if(var_data !is VariableData.init && identifier) {
				data.type_data = var_data.type;
				data.name = token.text;
				data.token_type = TokenType.variable;
				return data;
			}

			// Token is a template parameter
			auto temp_data = get_template_data_by_name(identifier);
			if(temp_data !is TemplateData.init) {
				data.token_type = TokenType.template_;
				data.type_data = TypeData(identifier);
				data.name = null;
				stderr.writefln("!!! template data: %s", data);
				return data;
			}

			// Token is a function name
			auto func_data = get_function_data_by_name(identifier);
			if(func_data !is FunctionData.init) {
				data.token_type = TokenType.function_;
				data.type_data = func_data.return_type;
				data.name = token.text;
				return data;
			}

			// Token is a class name
			auto class_data = get_class_data_by_name(identifier);
			if(class_data !is ClassData.init) {
				data.token_type = TokenType.class_;
				data.type_data = TypeData(token.text);
				data.name = null;
				return data;
			}

			// Token is a struct name
			auto struct_data = get_struct_data_by_name(identifier);
			if(struct_data !is StructData.init) {
				data.token_type = TokenType.struct_;
				data.type_data = TypeData(token.text);
				data.name = null;
				return data;
			}

			// Token is an enum
			auto enum_data = get_enum_data_by_name(identifier);
			if(enum_data !is EnumData.init) {
				data.name = token.text;
				// Enum field
				if(member in enum_data.fields) {
					data.token_type = TokenType.enum_;
					data.type_data = TypeData(identifier);
				// Enum
				} else {
					data.token_type = TokenType.enum_;
					data.type_data = enum_data.type;
				}
				return data;
			}
		}

		// Token is an identifier that should have used a this pointer
		// blah instead of this.blah
		if(token.type == tok!"identifier" && this_pointers.peak) {
			// Token may be a field/method without the this pointer
			// Figure out what "this" should be
			auto class_data = get_class_data_by_name(this_pointers.peak);
			auto struct_data = get_struct_data_by_name(this_pointers.peak);
			string identifier = token.text;

			// Class
			if(class_data !is ClassData.init) {
				// Token is a field
				if(identifier in class_data.fields) {
					data.token_type = TokenType.this_field;
					data.type_data = class_data.fields[identifier].type;
					data.name = token.text;
					return data;
				// Token is a method
				} else if(identifier in class_data.methods) {
					data.token_type = TokenType.this_method;
					data.type_data = class_data.methods[identifier].return_type;
					data.name = token.text;
					return data;
				}
			// Struct
			} else if(struct_data !is StructData.init) {
				// Token is a field
				if(identifier in struct_data.fields) {
					data.token_type = TokenType.this_field;
					data.type_data = struct_data.fields[identifier].type;
					data.name = token.text;
					return data;
				// Token is a method
				} else if(identifier in struct_data.methods) {
					data.token_type = TokenType.this_method;
					data.type_data = struct_data.methods[identifier].return_type;
					data.name = token.text;
					return data;
				}
			}
		}

		// Token uses a module
		if(token.type == tok!"identifier") {
			string full_identifier = token.text;
			string method = null;
			ModuleData module_data;

			// Match full module name
			foreach(mod; analysis.scope_frame.modules) {
				if(full_identifier.startsWith(mod.name) && full_identifier.length > mod.name.length) {
					module_data = mod;
					auto offset = mod.name.length + 1;
					method = full_identifier[offset .. $];
					goto got_module;
				}
			}

			// Match partial module name using imports
			foreach(frame; std.range.retro(analysis.scope_frame.frames)) {
				foreach(import_name; frame.imports) {
					if(import_name in analysis.scope_frame.modules) {
						auto candidate_module = analysis.scope_frame.modules[import_name];

						// FIXME: Make it work with enum fields, static methods, and properties
						if(full_identifier in candidate_module.variables || 
							full_identifier in candidate_module.functions || 
							full_identifier in candidate_module.classes || 
							full_identifier in candidate_module.structs || 
							full_identifier in candidate_module.enums) {
							method = full_identifier;
							module_data = candidate_module;
						}
					}
					if(module_data !is ModuleData.init)
						goto got_module;
				}
			}

			got_module:
			// FIXME: Make it work with enum fields, static methods, and properties

			// Variable match
			if(method in module_data.variables) {
				auto var_data = module_data.variables[method];
				data.type_data = var_data.type;
				data.name = method;
				data.token_type = TokenType.variable;
				return data;
			// Function match
			} else if(method in module_data.functions) {
				auto func_data = module_data.functions[method];
				data.token_type = TokenType.function_;
				data.type_data = func_data.return_type;
				data.name = method;
				return data;
			// Class match
			} else if(method in module_data.classes) {
				auto class_data = module_data.classes[method];
				data.token_type = TokenType.class_;
				data.type_data = TypeData(method);
				data.name = null;
				return data;
			// Struct match
			} else if(method in module_data.structs) {
				auto struct_data = module_data.structs[method];
				data.token_type = TokenType.struct_;
				data.type_data = TypeData(method);
				data.name = null;
				return data;
			// Enum match
			} else if(method in module_data.enums) {
				auto enum_data = module_data.enums[method];
				data.name = method;
				data.token_type = TokenType.enum_;
				data.type_data = enum_data.type;
				return data;
			}
		}

		// Token is an attribute
		if(token.type ==  tok!"identifier") {
			if(token.text == "property") {
				data.token_type = TokenType.property_attribute;
				data.type_data = TypeData(token.text);
				data.name = null;
				return data;
			}
		}

		string fail = "!!! get_token_data() failed on token: type:%s, text:%s, line:%s, column:%s".format(
			token.type.str, token.text, data.line, data.column);
		//throw new Exception(fail);
		stderr.writeln(fail);
		return TokenData.init;
	}

	Token combine_tokens(const Token a, const Token b) {
		Token token;

		if(a is Token.init)
			return b;
		else if(b is Token.init)
			return a;

		// This pointer
		if(a.type == tok!"this") {
			token.type = a.type;
			token.text = b.text;
			token.line = a.line;
			token.column = a.column;
		// class/struct member
		} else if(a.type == tok!"identifier") {
			token.type = a.type;
			token.text = a.text ~ "." ~ b.text;
			token.line = a.line;
			token.column = a.column;
		} else {
			stderr.writefln("!!! Unexpected tokens to combine: '%s:%s', '%s:%s'", 
			a.type.str, a.text, b.type.str, b.text);
			return Token.init;
		}

		return token;
	}

