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

enum TokenType : string
{
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

struct TokenData
{
	size_t line;
	size_t column;
	TokenType tokenType;
	TypeData typeData;
	string name;
	string value;
}

struct ModuleFunctionSet
{
	string importName;
	string[] functions;
}

// Returns true if a ModuleFunctionSet has a function.
bool hasFunction(const ModuleFunctionSet funcSet, string funcName)
{
	return getFunctionFullName(funcSet, funcName) !is null;
}

string getFunctionFullName(const ModuleFunctionSet funcSet, string funcName)
{
	bool isImported = analysis.scope_frame.isAlreadyImported(funcSet.importName);

	foreach (func; funcSet.functions)
	{
		string fullFuncName = "%s.%s".format(funcSet.importName, func);

		// If the module is imported the short function name may be used
		if (isImported && funcName == func)
		{
			return fullFuncName;
		}

		// If the module is not imported, the full function name must be used
		if (funcName == fullFuncName)
		{
			return fullFuncName;
		}
	}

	return null;
}

void assertAnalyzerWarnings(string code, analysis.run.AnalyzerCheck analyzers, string file=__FILE__, size_t line=__LINE__)
{
	import analysis.run;

	// Reset everything
	scopeFrameClearEverything();

	// Run the code and get any warnings
	string[] rawWarnings = analyze("test", cast(ubyte[]) code, analyzers);
	string[] codeLines = code.split("\n");

	// Get the warnings ordered by line
	string[size_t] warnings;
	for (size_t i=0; i<rawWarnings.length; ++i)
	{
		size_t warn_line = line - 1 + std.conv.to!size_t(rawWarnings[i].between("test(", ":"));
		warnings[warn_line] = rawWarnings[i].after(")");
//		stderr.writefln("!!! warnings[%d] = \"%s\"", warn_line, warnings[warn_line]);
	}

	// Get all the messages from the comments in the code
	string[size_t] messages;
	foreach (i, codeLine; codeLines)
	{
		// Skip if no [warn] comment
		if (codeLine.indexOf("// [warn]:") == -1)
			continue;

		// Skip if there is no comment or code
		string codePart = codeLine.before("// ");
		string commentPart = codeLine.after("// ");
		if (!codePart.length || !commentPart.length)
			continue;

		// Get the line of this code line
		size_t lineNo = i + line;

		// Get the message
//		stderr.writefln("!!! message[%d] = \"%s\"", lineNo, commentPart);
		messages[lineNo] = commentPart;
	}

	// Throw an assert error if any messages are not listed in the warnings
	foreach (lineNo, message; messages)
	{
//		stderr.writefln("!!!!!! messages[%d] : %s", lineNo, messages[lineNo]);
		// No warning
		if (lineNo !in warnings)
		{
			string errors = "Expected warning:\n%s\nFrom source code at (%s:?):\n%s".format(
				messages[lineNo], 
				lineNo, 
				codeLines[lineNo - line]
			);
			throw new core.exception.AssertError(errors, file, lineNo);
		// Different warning
		}
		else if (warnings[lineNo] != messages[lineNo])
		{
			string errors = "Expected warning:\n%s\nBut was:\n%s\nFrom source code at (%s:?):\n%s".format(
				messages[lineNo], 
				warnings[lineNo], 
				lineNo, 
				codeLines[lineNo - line]
			);
			throw new core.exception.AssertError(errors, file, lineNo);
		}
	}

	// Throw an assert error if there were any warnings that were not expected
	string[] unexpectedWarnings;
	foreach (lineNo, warning; warnings)
	{
//		stderr.writefln("!!!!!! warnings[%d] : %s", lineNo, warning);
		// Unexpected warning
		if (lineNo !in messages)
		{
			unexpectedWarnings ~= "%s\nFrom source code at (%s:?):\n%s".format(
				warning, 
				lineNo, 
				codeLines[lineNo - line]
			);
		}
	}
	if (unexpectedWarnings.length)
	{
		string message = "Unexpected warnings:\n" ~ unexpectedWarnings.join("\n");
		throw new core.exception.AssertError(message, file, line);
	}
}

void declareImport(const SingleImport singImpo)
{
	info("declareImport");

	// Just return if anything is null
	if (!singImpo && !singImpo.identifierChain)
	{
		return;
	}

	// Get the whole name from each identifier chunk
	string[] chunks;
	foreach (identifier; singImpo.identifierChain.identifiers)
	{
		chunks ~= identifier.text;
	}
	string importName = chunks.join(".");

	// Just return if already imported
	if (isAlreadyImported(importName))
		return;

	// Add the import
	stderr.writefln("!!! importing: %s", importName);
	addImport(importName);

	// Add the module
	string file_name = "%s.d".format(importName);
	loadModule(file_name);
}

void loadModule(string file_name)
{
	// Just return if the file does not exist
	if (!std.file.exists(file_name) || !std.file.isFile(file_name))
		return;

	// Read the code
	File f = File(file_name);
	auto code = uninitializedArray!(ubyte[])(to!size_t(f.size));
	f.rawRead(code);

	// Lex the code
	auto lexer = byToken(code);
	auto app = appender!(typeof(lexer.front)[])();
	while (!lexer.empty)
	{
		app.put(lexer.front);
		lexer.popFront();
	}

	// Parse the code
	auto p = new std.d.parser.ParseAllocator();
	Module mod = std.d.parser.parseModule(app.data, file_name, p, null);

	// Get data from the module
	ModuleData module_data = getModuleData(mod); // FIXME: This splodes
	addModule(module_data);
}

void declareFunction(const FunctionDeclaration funcDec)
{
	info("declareFunction");

	FunctionData data = getFunctionData(funcDec);
	addFunction(data);
}

void declareVariable(const VariableDeclaration varDec)
{
	info("declareVariable");

	foreach (var_data; getVariableDatas(varDec))
	{
		addVariable(var_data);
	}
}

void declareParameter(const Parameter param)
{
	info("declareParameter");

	VariableData var_data;
	var_data.name = param.name.text;
	var_data.type = getTypeData(param.type);
	var_data.isParameter = true;
	var_data.line = param.name.line;
	var_data.column = param.name.column;

	addVariable(var_data);
}

void declareTemplates(const TemplateParameters tempParams) {
	info("declareTemplates");

	TemplateData[] temp_datas = getTemplateDatas(tempParams);
	foreach (temp_data; temp_datas) {
		addTemplateParameter(temp_data);
	}
}

void declareStruct(const StructDeclaration structDec) {
	info("declareStruct");

	StructData struct_data = getStructData(structDec);
	addStruct(struct_data);
}

void declareClass(const ClassDeclaration classDec) {
	info("declareClass");

	ClassData class_data = getClassData(classDec);
	addClass(class_data);
}

void declareEnum(const EnumDeclaration enumDec) {
	info("declareEnum");

	EnumData enum_data = getEnumData(enumDec);
	addEnum(enum_data);
}

void declareModule(const Module mod) {
	info("declareModule");

	ModuleData module_data = getModuleData(mod);
	addModule(module_data);
}

// FIXME: For some reason this never sets decoration.is_property to true
Decoration getDeclarationDecorations(const Declaration decl) {
	Decoration decoration;
	foreach (attr; decl.attributes) {
		// Skip if no storage class
		if (!attr.storageClass || attr.storageClass.token is Token.init)
			continue;

		// Reference
		if (attr.storageClass.token.type.str == "ref") {
			decoration.isRef = true;
		// Auto
		} else if (attr.storageClass.token.type.str == "auto") {
			decoration.isAuto = true;
		}

		// Skip if no at attribute
		if (!attr.storageClass.atAttribute || attr.storageClass.atAttribute.identifier is Token.init)
			continue;

		// Property
		Token iden = attr.storageClass.atAttribute.identifier;
		if (iden.type.str == "identifier" && iden.text == "property") {
			decoration.isProperty = true;
		}
	}

	return decoration;
}

VariableData[] getVariableDatas(const VariableDeclaration varDec) {
	VariableData[] datas;

	// Using auto
	if (varDec.autoDeclaration) {
		string[] names = getVariableNames(varDec);
		TokenData token_data = getExpressionReturnTokenData(varDec.autoDeclaration);

		if (token_data is TokenData.init) {
			throw new Exception("Failed to get valid token from auto variable declaration.");
		}

		foreach (name; names) {
			VariableData data;
			data.name = name;
			data.type = token_data.typeData;
			data.isParameter = false;
			data.line = token_data.line;
			data.column = token_data.column;
			datas ~= data;
		}
	// Normal variable
	} else {
		string[] names = getVariableNames(varDec);
		TypeData type = getTypeData(varDec.type);
		foreach (name; names) {
			VariableData data;
			data.name = name;
			data.type = type;
			data.isParameter = false;
			getVariableLineColumn(varDec, name, data.line, data.column);
			datas ~= data;
		}
	}

	return datas;
}

TemplateData[] getTemplateDatas(const TemplateParameters templateParameters) {
	TemplateData[] datas;
	if (templateParameters && templateParameters.templateParameterList) {
		foreach (item; templateParameters.templateParameterList.items) {
			if (item && item.templateTypeParameter) {
				auto identifier = item.templateTypeParameter.identifier;
				if (identifier !is Token.init) {
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

FunctionData getFunctionData(const FunctionDeclaration funcDec) {
	FunctionData data;
	data.name = funcDec.name.text;
	data.templates = getTemplateDatas(funcDec.templateParameters);
	data.returnType = getFunctionReturnTypeData(funcDec);
	data.argTypes = getFunctionArgTypeDatas(funcDec);
	data.line = funcDec.name.line;
	data.column = funcDec.name.column;
	return data;
}

StructData getStructData(const StructDeclaration structDec) {
	StructData data;
	data.name = structDec.name.text;
	data.line = structDec.name.line;
	data.column = structDec.name.column;

	foreach (decl; structDec.structBody.declarations) {
		if (decl.variableDeclaration) {
			foreach (var_data; getVariableDatas(decl.variableDeclaration)) {
				data.fields[var_data.name] = var_data;
			}
		} else if (decl.functionDeclaration) {
			FunctionData func_data = getFunctionData(decl.functionDeclaration);
			data.methods[func_data.name] = func_data;
		}
	}

	return data;
}

ClassData getClassData(const ClassDeclaration classDec) {
	ClassData data;
	data.name = classDec.name.text;
	data.line = classDec.name.line;
	data.column = classDec.name.column;

	foreach (decl; classDec.structBody.declarations) {
		if (decl.variableDeclaration) {
			foreach (var_data; getVariableDatas(decl.variableDeclaration)) {
				data.fields[var_data.name] = var_data;
			}
		} else if (decl.functionDeclaration) {
			FunctionData func_data = getFunctionData(decl.functionDeclaration);
			data.methods[func_data.name] = func_data;
		}
	}
	return data;
}

EnumData getEnumData(const EnumDeclaration enumDec) {
	EnumData data;
	data.name = enumDec.name.text;
	data.line = enumDec.name.line;
	data.column = enumDec.name.column;
	data.type = TypeData("int");
	try {
		data.type = getTypeData(enumDec.type);
	} catch(Exception ex) {
		//
	}

	foreach (member; enumDec.enumBody.enumMembers) {
		if (member) {
			string name = member.name.text;
			FieldData field_data;
			field_data.line = member.name.line;
			field_data.column = member.name.column;
			data.fields[name] = field_data;
		}
	}

	return data;
}

ModuleData getModuleData(const Module mod) {
	ModuleData data;

	// FIXME: What do we do when a module has no name?
	// Get the module name
	if (mod && 
		mod.moduleDeclaration && 
		mod.moduleDeclaration.moduleName &&
		mod.moduleDeclaration.moduleName.identifiers.length) {
		string[] mod_name_chunks;
		foreach (identifier; mod.moduleDeclaration.moduleName.identifiers) {
			mod_name_chunks ~= identifier.text;
		}
		data.name = mod_name_chunks.join(".");
	} else {
		data.name = "unknown";
	}

	foreach (decl; mod.declarations) {

		// Add decorations such as properties, auto, ref, et cetera
		Decoration decoration = getDeclarationDecorations(decl);
		decorations.push(decoration);

		if (decl.functionDeclaration) {
			FunctionData func_data = getFunctionData(decl.functionDeclaration);
			data.functions[func_data.name] = func_data;
		} else if (decl.variableDeclaration) {
			foreach (var_data; getVariableDatas(decl.variableDeclaration)) {
				data.variables[var_data.name] = var_data;
			}
		} else if (decl.structDeclaration) {
			StructData struct_data = getStructData(decl.structDeclaration);
			data.structs[struct_data.name] = struct_data;
		} else if (decl.classDeclaration) {
			ClassData class_data = getClassData(decl.classDeclaration);
			data.classes[class_data.name] = class_data;
		} else if (decl.enumDeclaration) {
			EnumData enum_data = getEnumData(decl.enumDeclaration);
			data.enums[enum_data.name] = enum_data;
		}

		// Remove decorations
		decorations.pop();
	}

	return data;
}

TokenData[] getFunctionCallArguments(const FunctionCallExpression funcCallExp) {
	TokenData[] args;

	// Just return if there are no args
	if (!funcCallExp || 
		!funcCallExp.arguments || 
		!funcCallExp.arguments.argumentList || 
		!funcCallExp.arguments.argumentList.items) {
		return args;
	}

	// Get the token data for each arg
	foreach (item; funcCallExp.arguments.argumentList.items) {
		args ~= getExpressionReturnTokenData(item);
	}

	return args;
}

string getFunctionCallName(const FunctionCallExpression funcExp) {
	string[] chunks;
	auto unaryExp = cast(UnaryExpression) funcExp.unaryExpression;
	while (unaryExp) {
		// Part of the name
		if (unaryExp.identifierOrTemplateInstance) {
			// from a template
			if (unaryExp.identifierOrTemplateInstance.templateInstance) {
				chunks ~= unaryExp.identifierOrTemplateInstance.templateInstance.identifier.text;
			// from a function
			} else {
				chunks ~= unaryExp.identifierOrTemplateInstance.identifier.text;
			}
		}

		// The end of the name
		if (unaryExp.primaryExpression && 
			unaryExp.primaryExpression.identifierOrTemplateInstance) {
			chunks ~= unaryExp.primaryExpression.identifierOrTemplateInstance.identifier.text;
		}

		unaryExp = unaryExp.unaryExpression;
	}

	if (!chunks.length) {
		throw new Exception("Could not get name of function to call.");
	}

	return chunks.reverse.join(".");
}

TypeData getFunctionReturnTypeData(const FunctionDeclaration funcDec) {
	// Normal return type
	if (funcDec.returnType) {
		return getTypeData(funcDec.returnType);
	}

	// Auto return type
	auto decoration = analysis.scope_frame.decorations.peak;
	if (decoration !is Decoration.init && decoration.isAuto) {
		return TypeData("auto");
	}

	return TypeData.init;
}

TypeData[] getFunctionArgTypeDatas(const FunctionDeclaration funcDec) {
	if (funcDec && 
		funcDec.parameters) {
		TypeData[] argTypes;
		foreach (param; funcDec.parameters.parameters) {
			argTypes ~= getTypeData(param.type);
		}
		return argTypes;
	}

	throw new Exception("Could not find function arg type names.");
}

string[] getFunctionArgNames(const FunctionDeclaration funcDec) {
	if (funcDec && 
		funcDec.parameters) {
		string[] arg_names;
		foreach (param; funcDec.parameters.parameters) {
			arg_names ~= param.name.text;
		}
		return arg_names;
	}

	throw new Exception("Could not find function arg names.");
}

string[] getVariableNames(const VariableDeclaration varDec) {
	string[] retval;
	if (varDec) {
		if (varDec.autoDeclaration) {
			foreach (iden; varDec.autoDeclaration.identifiers) {
				retval ~= iden.text;
			}
		} else {
			foreach (d; varDec.declarators) {
				retval ~= d.name.text;
			}
		}
	}

	if (retval.length)
		return retval;

	throw new Exception("Could not find variable names.");
}

void getVariableLineColumn(const VariableDeclaration varDec, string name, ref size_t line, ref size_t column) {
	if (varDec) {
		if (varDec.autoDeclaration) {
			foreach (iden; varDec.autoDeclaration.identifiers) {
				if (iden.text == name) {
					line = iden.line;
					column = iden.column;
					return;
				}
			}
		}

		foreach (d; varDec.declarators) {
			if (d.name.text == name) {
				line = d.name.line;
				column = d.name.column;
				return;
			}
		}
	}

	throw new Exception("Could not find variable line and column.");
}

void markUsedVariables(const ASTNode node) {
	auto unaryExp = cast(const UnaryExpression) node;
	auto ternaryExp = cast(const TernaryExpression) node;
	if (unaryExp || ternaryExp) {
		TokenData data;
		if (unaryExp)
			data = getExpressionReturnTokenData(unaryExp);
		else
			data = getExpressionReturnTokenData(ternaryExp);
		//stderr.writefln("!!! token_data tokenType:%s, data_type:%s, name:%s", data.tokenType, data.typeData, data.name);
		if (data !is TokenData.init && data.name && 
			(data.tokenType == TokenType.variable || data.tokenType == TokenType.field || data.tokenType == TokenType.method)) {
			string name = data.name.before(".");
			setVariableIsUsedByName(name);
		}
	} else if (auto exp = cast(const AssignExpression) node) {
		markUsedVariables(exp.ternaryExpression);
	} else if (auto exp = cast(const AssertExpression) node) {
		markUsedVariables(exp.assertion);
	} else if (auto exp = cast(const CmpExpression) node) {
		markUsedVariables(exp.shiftExpression);
		markUsedVariables(exp.equalExpression);
		markUsedVariables(exp.identityExpression);
		markUsedVariables(exp.relExpression);
		markUsedVariables(exp.inExpression);
	} else if (auto exp = cast(AddExpression) node) {
		markUsedVariables(exp.left);
		markUsedVariables(exp.right);
	} else if (auto exp = cast(AndAndExpression) node) {
		markUsedVariables(exp.left);
		markUsedVariables(exp.right);
	} else if (auto exp = cast(AndExpression) node) {
		markUsedVariables(exp.left);
		markUsedVariables(exp.right);
	} else if (auto exp = cast(EqualExpression) node) {
		markUsedVariables(exp.left);
		markUsedVariables(exp.right);
	} else if (auto exp = cast(IdentityExpression) node) {
		markUsedVariables(exp.left);
		markUsedVariables(exp.right);
	} else if (auto exp = cast(InExpression) node) {
		markUsedVariables(exp.left);
		markUsedVariables(exp.right);
	} else if (auto exp = cast(MulExpression) node) {
		markUsedVariables(exp.left);
		markUsedVariables(exp.right);
	} else if (auto exp = cast(OrExpression) node) {
		markUsedVariables(exp.left);
		markUsedVariables(exp.right);
	} else if (auto exp = cast(OrOrExpression) node) {
		markUsedVariables(exp.left);
		markUsedVariables(exp.right);
	} else if (auto exp = cast(PowExpression) node) {
		markUsedVariables(exp.left);
		markUsedVariables(exp.right);
	} else if (auto exp = cast(RelExpression) node) {
		markUsedVariables(exp.left);
		markUsedVariables(exp.right);
	} else if (auto exp = cast(ShiftExpression) node) {
		markUsedVariables(exp.left);
		markUsedVariables(exp.right);
	} else if (auto exp = cast(XorExpression) node) {
		markUsedVariables(exp.left);
		markUsedVariables(exp.right);
	} else if (node) {
		throw new Exception("!!! markUsedVariables() override needed for: %s".format(typeid(node)));
	}
}

TypeData getTypeData(const Type type) {
	TypeData typeData;
	if (type && type.type2) {
		// Check for a nested type
		if (type.type2.type)
			return getTypeData(type.type2.type);

		// Check if it is an array
		foreach (suffix; type.typeSuffixes) {
			if (suffix.array)
				typeData.isArray = true;
		}

		// Get type from builtinType
		if (type.type2.builtinType) {
			typeData.name = type.type2.builtinType.str;
			return typeData;
		}

		// Get type from symbol
		if (type.type2.symbol && 
			type.type2.symbol.identifierOrTemplateChain) {

			auto chain = type.type2.symbol.identifierOrTemplateChain.identifiersOrTemplateInstances;
			string[] identifiers;
			foreach (idenOrTemp; chain) {
				identifiers ~= idenOrTemp.identifier.text;
			}
			typeData.name = identifiers.join(".");
			return typeData;
		}
	}
	//std.d.inspect.inspect(type, 0, "type");
	throw new Exception("Could not find type name.");
}

bool isSameTokenVariable(const TokenData a, const TokenData b) {
	return 
		a !is TokenData.init && b !is TokenData.init && 
		(a.tokenType == TokenType.variable && b.tokenType == TokenType.variable || 
		a.tokenType == TokenType.field && b.tokenType == TokenType.field) && 
		a.name && b.name && 
		a.name == b.name;
}

TypeData getExpressionReturnType(const ASTNode node, ref size_t line, ref size_t column) {
	TokenData data = getExpressionReturnTokenData(node);
	line = data.line;
	column = data.column;
	return data.typeData;
}

TokenData getExpressionReturnTokenData(const ASTNode node) {
	Token token = getExpressionReturnToken(node, 0);
	TokenData data = getTokenData(token);
	//stderr.writefln("!!!!!! %s, %s, %s, %s, %s", data.tokenType, data.typeData, data.name, data.line, data.column);
	return data;
}

// FIXME Update this so it prints an error and returns init when nothing found
Token getExpressionReturnToken(const ASTNode node, size_t indent) {
	//stderr.writefln("%s??? getExpressionReturnToken: %s", pad(indent++), typeid(node));

	if (auto addExp = cast(const AddExpression) node) {
		auto l = getExpressionReturnToken(addExp.left, indent);
		auto r = getExpressionReturnToken(addExp.right, indent);
		return getPromotedToken(l, r);
	} else if (auto andAndExp = cast(const AndAndExpression) node) {
		auto l = getExpressionReturnToken(andAndExp.left, indent);
		auto r = getExpressionReturnToken(andAndExp.right, indent);
		return Token(tok!"identifier", "bool", l.line, l.column, l.index);
	} else if (auto andExp = cast(const AndExpression) node) {
		auto l = getExpressionReturnToken(andExp.left, indent);
		auto r = getExpressionReturnToken(andExp.right, indent);
		return getPromotedToken(l, r);
	} else if (auto arrayInit = cast(const ArrayInitializer) node) {
		foreach (memberInit; arrayInit.arrayMemberInitializations) {
			return getExpressionReturnToken(memberInit, indent);
		}
	} else if (auto arrayLit = cast(const ArrayLiteral) node) {
		return getExpressionReturnToken(arrayLit.argumentList, indent);
	} else if (auto arrayMemInit = cast(const ArrayMemberInitialization) node) {
		if (arrayMemInit.assignExpression)
			return getExpressionReturnToken(arrayMemInit.assignExpression, indent);
		else if (arrayMemInit.nonVoidInitializer)
			return getExpressionReturnToken(arrayMemInit.nonVoidInitializer, indent);
	} else if (auto argList = cast(const ArgumentList) node) {
		foreach (item; argList.items) {
			return getExpressionReturnToken(item, indent);
		}
	} else if (auto asserExp = cast(const AssertExpression) node) {
		return getExpressionReturnToken(asserExp.assertion, indent);
	} else if (auto assExp = cast(const AssignExpression) node) {
		if (assExp.ternaryExpression) {
			return getExpressionReturnToken(assExp.ternaryExpression, indent);
		} else if (assExp.assignExpression) {
			return getExpressionReturnToken(assExp.assignExpression, indent);
		}
	} else if (auto autoDec = cast(const AutoDeclaration) node) {
//		foreach (iden; autoDec.identifiers)
//			if (iden)
//				return getExpressionReturnToken(iden, indent);

		foreach (init; autoDec.initializers)
			if (init)
				return getExpressionReturnToken(init, indent);
	} else if (auto castExp = cast(const CastExpression) node) {
		if (castExp.type)
			return getExpressionReturnToken(castExp.type, indent);
		if (castExp.castQualifier)
			return getExpressionReturnToken(castExp.castQualifier, indent);
		if (castExp.unaryExpression)
			return getExpressionReturnToken(castExp.unaryExpression, indent);
	} else if (auto cmpExp = cast(const CmpExpression) node) {
		if (cmpExp.shiftExpression)
			return getExpressionReturnToken(cmpExp.shiftExpression, indent);
		else if (cmpExp.equalExpression)
			return getExpressionReturnToken(cmpExp.equalExpression, indent);
		else if (cmpExp.identityExpression)
			return getExpressionReturnToken(cmpExp.identityExpression, indent);
		else if (cmpExp.relExpression)
			return getExpressionReturnToken(cmpExp.relExpression, indent);
		else if (cmpExp.inExpression)
			return getExpressionReturnToken(cmpExp.inExpression, indent);
	} else if (auto decl = cast(const Declarator) node) {
		if (decl.initializer) {
			return getExpressionReturnToken(decl.initializer, indent);
		} else {
			return decl.name;
		}
	} else if (auto delExp = cast(const DeleteExpression) node) {
		if (delExp.unaryExpression)
			return getExpressionReturnToken(delExp.unaryExpression, indent);
	} else if (auto eqlExp = cast(const EqualExpression) node) {
		auto l = getExpressionReturnToken(eqlExp.left, indent);
		auto r = getExpressionReturnToken(eqlExp.right, indent);
		return Token(tok!"identifier", "bool", l.line, l.column, l.index);
	} else if (auto expExp = cast(const Expression) node) {
		foreach (item; expExp.items)
			if (item)
				return getExpressionReturnToken(item, indent);
	} else if (auto funCallExp = cast(const FunctionCallExpression) node) {
		// FIXME: This breaks with UFC
		if (funCallExp.unaryExpression)
			return getExpressionReturnToken(funCallExp.unaryExpression, indent);
	} else if (auto idenOrTemp = cast(const IdentifierOrTemplateChain) node) {
		if (idenOrTemp)
			foreach (inst; idenOrTemp.identifiersOrTemplateInstances)
				return getExpressionReturnToken(inst, indent);
	} else if (auto idenOrTemp = cast(const IdentifierOrTemplateInstance) node) {
		if (idenOrTemp.templateInstance) {
			return getExpressionReturnToken(idenOrTemp.templateInstance, indent);
		} else {
			return idenOrTemp.identifier;
		}
	} else if (auto idenExp = cast(const IdentityExpression) node) {
		auto l = getExpressionReturnToken(idenExp.left, indent);
		auto r = getExpressionReturnToken(idenExp.right, indent);
		return Token(tok!"identifier", "bool", l.line, l.column, l.index);
	} else if (auto impExp = cast(const ImportExpression) node) {
		if (impExp.assignExpression)
			return getExpressionReturnToken(impExp.assignExpression, indent);
	} else if (auto indexExp = cast(const IndexExpression) node) {
		if (indexExp.unaryExpression)
			return getExpressionReturnToken(indexExp.unaryExpression, indent);
	} else if (auto inExp = cast(const InExpression) node) {
		auto l = getExpressionReturnToken(inExp.left, indent);
		auto r = getExpressionReturnToken(inExp.right, indent);
		return Token(tok!"identifier", "bool", l.line, l.column, l.index);
	} else if (auto intl = cast(const Initializer) node) {
		if (intl.nonVoidInitializer)
			return getExpressionReturnToken(intl.nonVoidInitializer, indent);
	} else if (auto mulExp = cast(const MulExpression) node) {
		auto l = getExpressionReturnToken(mulExp.left, indent);
		auto r = getExpressionReturnToken(mulExp.right, indent);
		return getPromotedToken(l, r);
	} else if (auto newExp = cast(const NewExpression) node) {
		if (newExp.type)
			return getExpressionReturnToken(newExp.type, indent);
		if (newExp.newAnonClassExpression)
			return getExpressionReturnToken(newExp.newAnonClassExpression, indent);
		//else if (newExp.arguments)
		//	return getExpressionReturnToken(newExp.arguments, indent);
		if (newExp.assignExpression)
			return getExpressionReturnToken(newExp.assignExpression, indent);
	} else if (auto nonVoidIntl = cast(const NonVoidInitializer) node) {
		if (nonVoidIntl.assignExpression)
			return getExpressionReturnToken(nonVoidIntl.assignExpression, indent);
		if (nonVoidIntl.arrayInitializer)
			return getExpressionReturnToken(nonVoidIntl.arrayInitializer, indent);
		if (nonVoidIntl.structInitializer)
			return getExpressionReturnToken(nonVoidIntl.structInitializer, indent);
	} else if (auto orExp = cast(const OrExpression) node) {
		auto l = getExpressionReturnToken(orExp.left, indent);
		auto r = getExpressionReturnToken(orExp.right, indent);
		return getPromotedToken(l, r);
	} else if (auto orOrExp = cast(const OrOrExpression) node) {
		auto l = getExpressionReturnToken(orOrExp.left, indent);
		auto r = getExpressionReturnToken(orOrExp.right, indent);
		return Token(tok!"identifier", "bool", l.line, l.column, l.index);
	} else if (auto postIncDecExp = cast(const PostIncDecExpression) node) {
		if (postIncDecExp.unaryExpression)
			return getExpressionReturnToken(postIncDecExp.unaryExpression, indent);
	} else if (auto powExp = cast(const PowExpression) node) {
		auto l = getExpressionReturnToken(powExp.left, indent);
		auto r = getExpressionReturnToken(powExp.right, indent);
		return getPromotedToken(l, r);
	} else if (auto preIncDecExp = cast(const PreIncDecExpression) node) {
		if (preIncDecExp.unaryExpression)
			return getExpressionReturnToken(preIncDecExp.unaryExpression, indent);
	} else if (auto primaryExp = cast(const PrimaryExpression) node) {
		if (primaryExp.identifierOrTemplateInstance)
			return getExpressionReturnToken(primaryExp.identifierOrTemplateInstance, indent);
		if (primaryExp.typeofExpression)
			return getExpressionReturnToken(primaryExp.typeofExpression, indent);
		if (primaryExp.typeidExpression)
			return getExpressionReturnToken(primaryExp.typeidExpression, indent);
		if (primaryExp.arrayLiteral)
			return getExpressionReturnToken(primaryExp.arrayLiteral, indent);
		if (primaryExp.assocArrayLiteral)
			return getExpressionReturnToken(primaryExp.assocArrayLiteral, indent);
		if (primaryExp.expression)
			return getExpressionReturnToken(primaryExp.expression, indent);
		if (primaryExp.isExpression)
			return getExpressionReturnToken(primaryExp.isExpression, indent);
		if (primaryExp.lambdaExpression)
			return getExpressionReturnToken(primaryExp.lambdaExpression, indent);
		if (primaryExp.functionLiteralExpression)
			return getExpressionReturnToken(primaryExp.functionLiteralExpression, indent);
		if (primaryExp.traitsExpression)
			return getExpressionReturnToken(primaryExp.traitsExpression, indent);
		if (primaryExp.mixinExpression)
			return getExpressionReturnToken(primaryExp.mixinExpression, indent);
		if (primaryExp.importExpression)
			return getExpressionReturnToken(primaryExp.importExpression, indent);
		if (primaryExp.vector)
			return getExpressionReturnToken(primaryExp.vector, indent);

		// return type
		if (getTokenData(primaryExp.dot) !is TokenData.init)
			return primaryExp.dot;
		if (getTokenData(primaryExp.primary) !is TokenData.init)
			return primaryExp.primary;
		if (getTokenData(primaryExp.basicType) !is TokenData.init)
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
	} else if (auto relExp = cast(const RelExpression) node) {
		auto l = getExpressionReturnToken(relExp.left, indent);
		auto r = getExpressionReturnToken(relExp.right, indent);
		return Token(tok!"identifier", "bool", l.line, l.column, l.index);
	} else if (auto shiftExp = cast(const ShiftExpression) node) {
		auto l = getExpressionReturnToken(shiftExp.left, indent);
		auto r = getExpressionReturnToken(shiftExp.right, indent);
		return getPromotedToken(l, r);
	} else if (auto sliceExp = cast(const SliceExpression) node) {
		if (sliceExp.unaryExpression)
			return getExpressionReturnToken(sliceExp.unaryExpression, indent);
	} else if (auto symbol = cast(const Symbol) node) {
		if (symbol.identifierOrTemplateChain)
			return getExpressionReturnToken(symbol.identifierOrTemplateChain, indent);
	} else if (auto tempArgList = cast(const TemplateArgumentList) node) {
		foreach (item; tempArgList.items) {
			return getExpressionReturnToken(item, indent);
		}
	} else if (auto tempArg = cast(const TemplateArgument) node) {
		if (tempArg.type)
			return getExpressionReturnToken(tempArg.type, indent);
		if (tempArg.assignExpression)
			return getExpressionReturnToken(tempArg.assignExpression, indent);
	} else if (auto tempArgs = cast(const TemplateArguments) node) {
		if (tempArgs.templateArgumentList)
			return getExpressionReturnToken(tempArgs.templateArgumentList, indent);
		if (tempArgs.templateSingleArgument)
			return getExpressionReturnToken(tempArgs.templateSingleArgument, indent);
	} else if (auto tempIns = cast(const TemplateInstance) node) {
		if (tempIns.templateArguments)
			return getExpressionReturnToken(tempIns.templateArguments, indent);
		else
			return tempIns.identifier;
	} else if (auto ternaryExp = cast(const TernaryExpression) node) {
		if (ternaryExp.orOrExpression)
			return getExpressionReturnToken(ternaryExp.orOrExpression, indent);
		if (ternaryExp.expression)
			return getExpressionReturnToken(ternaryExp.expression, indent);
		if (ternaryExp.ternaryExpression)
			return getExpressionReturnToken(ternaryExp.ternaryExpression, indent);
	} else if (auto tempSingArg = cast(const TemplateSingleArgument) node) {
		if (getTokenData(tempSingArg.token) !is TokenData.init)
			return tempSingArg.token;
	} else if (auto type = cast(const Type) node) {
		if (type.type2)
			return getExpressionReturnToken(type.type2, indent);
	} else if (auto type2 = cast(const Type2) node) {
		if (type2.symbol)
			return getExpressionReturnToken(type2.symbol, indent);
		if (type2.typeofExpression)
			return getExpressionReturnToken(type2.typeofExpression, indent);
		if (type2.identifierOrTemplateChain)
			return getExpressionReturnToken(type2.identifierOrTemplateChain, indent);
		if (type2.type)
			return getExpressionReturnToken(type2.type, indent);

		if (type2.builtinType.str != "!ERROR!") {
			return Token(
				tok!"identifier", type2.builtinType.str, 
				// FIXME: Get the real line and column
				0, 0, 0);
		}
		//if (type2.typeConstructor.str != "!ERROR!")
		//	return type2.typeConstructor.str;
	} else if (auto typeidExp = cast(const TypeidExpression) node) {
		if (typeidExp.expression)
			return getExpressionReturnToken(typeidExp.expression, indent);
		if (typeidExp.type)
			return getExpressionReturnToken(typeidExp.type, indent);
	} else if (auto typeofExp = cast(const TypeofExpression) node) {
		if (typeofExp.expression)
			return getExpressionReturnToken(typeofExp.expression, indent);
		//if (typeofExp.return_)
		//	return getExpressionReturnToken(typeofExp.return_, indent);
	} else if (auto unaryExp = cast(const UnaryExpression) node) {
		// Get the prefix such as "this."
		Token firstToken, secondToken;
		if (unaryExp.unaryExpression) {
			firstToken = getExpressionReturnToken(unaryExp.unaryExpression, indent);
		}

		// Get the second token
		if (unaryExp.type)
			secondToken = getExpressionReturnToken(unaryExp.type, indent);
		if (unaryExp.primaryExpression)
			secondToken = getExpressionReturnToken(unaryExp.primaryExpression, indent);
		//if (unaryExp.prefix)
		//	secondToken = getExpressionReturnToken(unaryExp.prefix, indent);
		//if (unaryExp.suffix)
		//	secondToken = getExpressionReturnToken(unaryExp.suffix, indent);
		if (unaryExp.newExpression)
			secondToken = getExpressionReturnToken(unaryExp.newExpression, indent);
		if (unaryExp.deleteExpression)
			secondToken = getExpressionReturnToken(unaryExp.deleteExpression, indent);
		if (unaryExp.castExpression)
			secondToken = getExpressionReturnToken(unaryExp.castExpression, indent);
		if (unaryExp.functionCallExpression)
			secondToken = getExpressionReturnToken(unaryExp.functionCallExpression, indent);
		if (unaryExp.argumentList)
			secondToken = getExpressionReturnToken(unaryExp.argumentList, indent);
		if (unaryExp.identifierOrTemplateInstance)
			secondToken = getExpressionReturnToken(unaryExp.identifierOrTemplateInstance, indent);
		if (unaryExp.assertExpression)
			secondToken = getExpressionReturnToken(unaryExp.assertExpression, indent);
		if (unaryExp.sliceExpression)
			secondToken = getExpressionReturnToken(unaryExp.sliceExpression, indent);
		if (unaryExp.indexExpression)
			secondToken = getExpressionReturnToken(unaryExp.indexExpression, indent);

		// Combine the tokens
		//stderr.writefln("!!! getExpressionReturnToken firstToken type:%s, text:%s", firstToken.type.str, firstToken.text);
		//stderr.writefln("!!! getExpressionReturnToken right type:%s, text:%s", secondToken.type.str, secondToken.text);
		// FIXME: UFC Boom
		Token newToken = combineTokens(firstToken, secondToken);
		//stderr.writefln("!!! t type:%s, text:%s", newToken.type.str, newToken.text);
		return newToken;
	} else if (auto xorExp = cast(const XorExpression) node) {
		auto l = getExpressionReturnToken(xorExp.left, indent);
		auto r = getExpressionReturnToken(xorExp.right, indent);
		return getPromotedToken(l, r);
	}

	if (node !is null) {
		stderr.writefln("!!! getExpressionReturnToken() failed on node: %s", typeid(node));
	}

	return Token.init;
}

// FIXME: Much of this can be replaced with some isBlah functions
const Token getPromotedToken(const Token left, const Token right) {
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

	string a = getTokenData(left).typeData.name;
	string b = getTokenData(right).typeData.name;

	// throw an error if any type names are blank
	if (a is null || b is null || a == "" || b == "") {
		string message = "getPromotedToken() did not expect: \"%s\" or \"%s\".".format(
			getTokenData(left), getTokenData(right));
		throw new Exception(message);
	}

	// throw an error if any type names are unknown
	if (a !in promotions || a !in sizes ||
		b !in promotions || b !in sizes) {

		string message = "getPromotedToken() did not expect the type name: \"%s\" or \"%s\".".format(a, b);
		throw new Exception(message);
	}

	// Types are the same, so return the promotion of one
	if (a == b) {
		return Token(tok!"identifier", promotions[a], left.line, left.column, left.index);
	}

	// Types are different, so return the promotion of the larger
	if (sizes[a] > sizes[b]) {
		return Token(tok!"identifier", promotions[a], left.line, left.column, left.index);
	} else {
		return Token(tok!"identifier", promotions[b], right.line, right.column, right.index);
	}
}

// FIXME: Make this work with UFC for variables and literals
TokenData getTokenData(const Token token) {
	TokenData data;
	//stderr.writefln("!!! getTokenData type:%s text:%s", token.type.str, token.text);

	// Line and column
	if (token.line && token.column) {
		data.line = token.line;
		data.column = token.column;
	}

	// Token is empty
	if (token is Token.init || token.type.str == "!ERROR!") {
		return TokenData.init;
	}

	// Token is null
	if (token.type == tok!"null") {
		data.tokenType = TokenType.null_;
		data.name = null;
		data.typeData = TypeData.init;
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
	if (token.type in LITERAL_TYPE_TO_TYPE_MAP) {
		string typeName = LITERAL_TYPE_TO_TYPE_MAP[token.type];
		data.typeData = TypeData(typeName);
		data.tokenType = TokenType.literal;
		data.name = null;
		data.value = token.text;
		return data;
	}

	// Token is a bool literal
	if (token.type == tok!"true" || token.type == tok!"false") {
		data.tokenType = TokenType.literal;
		data.typeData = TypeData("bool");
		data.name = null;
		data.value = token.type.str;
		return data;
	}

	// Token is a basic type
	if (token.type == tok!"identifier") {
		if (!std.algorithm.find(BASIC_TYPES, token.text).empty) {
			data.tokenType = TokenType.basic_type;
			data.typeData = TypeData(token.text);
			data.name = null;
			return data;
		}
	}

	// Token is an auto declaration
	if (token.type ==  tok!"auto") {
		data.tokenType = TokenType.auto_declaration;
		data.typeData = TypeData.init;
		data.name = null;
		return data;
	}

	// Token is an ref declaration
	if (token.type ==  tok!"ref") {
		data.tokenType = TokenType.ref_declaration;
		data.typeData = TypeData.init;
		data.name = null;
		return data;
	}

	// Token is __FILE__
	if (token.type == tok!"__FILE__") {
		data.tokenType = TokenType.__file__;
		data.name = null;
		data.typeData = TypeData("string");
		return data;
	}

	// Token is __LINE__
	if (token.type == tok!"__LINE__") {
		data.tokenType = TokenType.__line__;
		data.name = null;
		data.typeData = TypeData("int");
		return data;
	}

	// Token is super
	if (token.type == tok!"super") {
		data.tokenType = TokenType.super_;
		data.name = null;
		data.typeData = TypeData.init;
		return data;
	}

	// Token is identifier with "this pointer" prefix
	// this.blah
	if (token.type == tok!"this" && token.text && token.text.length && thisPointers.peak) {
		string member = token.text;

		// Figure out what "this" is
		auto class_data = getClassDataByName(thisPointers.peak);
		auto struct_data = getStructDataByName(thisPointers.peak);

		// Class
		if (class_data !is ClassData.init) {
			// Token is a field
			if (member in class_data.fields) {
				data.tokenType = TokenType.this_field;
				data.name = token.text;
				data.typeData = class_data.fields[member].type;
				return data;
			// Token is a method
			} else if (member in class_data.methods) {
				data.tokenType = TokenType.this_method;
				data.name = token.text;
				data.typeData = class_data.methods[member].returnType;
				return data;
			}
		// Struct
		} else if (struct_data !is StructData.init) {
			// Token is a field
			if (member in struct_data.fields) {
				data.tokenType = TokenType.this_field;
				data.name = token.text;
				data.typeData = struct_data.fields[member].type;
				return data;
			// Token is a method
			} else if (member in struct_data.methods) {
				data.tokenType = TokenType.this_method;
				data.name = token.text;
				data.typeData = struct_data.methods[member].returnType;
				return data;
			}
		}
	}

	// Token is just the "this pointer"
	// this
	if (token.type == tok!"this" && thisPointers.peak) {
		data.tokenType = TokenType.this_;
		data.name = "this";
		data.typeData = TypeData(thisPointers.peak);
		return data;
	}

	// Token is an identifier
	if (token.type == tok!"identifier") {
		// If the identifier has a dot "blah.member", then it is an 
		// identifier and member
		string identifier = null;
		string member = null;
		if (token.text.indexOf(".") != -1) {
			identifier = token.text.before(".");
			member = token.text.after(".");
		} else {
			identifier = token.text;
		}

		auto var_data = getVariableDataByName(identifier);

		// Token is a struct/class instance
		if (var_data !is VariableData.init) {
			string typeName = var_data.type.name;
			auto class_data = getClassDataByName(typeName);
			auto struct_data = getStructDataByName(typeName);

			// Class instance member
			if (member && class_data !is ClassData.init) {
				// Class instance field
				if (member in class_data.fields) {
					data.typeData = class_data.fields[member].type;
					data.name = token.text;
					data.tokenType = TokenType.field;
				// Class instance method
				} else if (member in class_data.methods) {
					data.typeData = class_data.methods[member].returnType;
					data.name = token.text;
					data.tokenType = TokenType.method;
				}
				return data;
			// Struct instance member
			} else if (member && struct_data !is StructData.init) {
				// Struct instance field
				if (member in struct_data.fields) {
					data.typeData = struct_data.fields[member].type;
					data.name = token.text;
					data.tokenType = TokenType.field;
				// Struct instance method
				} else if (member in struct_data.methods) {
					data.typeData = struct_data.methods[member].returnType;
					data.name = token.text;
					data.tokenType = TokenType.method;
				}
				return data;
			}
		}

		// Token is a variable with member
		if (var_data !is VariableData.init && member) {
			// Is a standard property
			switch(member) {
				case "alignof":
					data.typeData = TypeData("size_t");
					data.name = token.text;
					data.tokenType = TokenType.field;
					return data;
				case "init":
					data.typeData = var_data.type;
					data.name = token.text;
					data.tokenType = TokenType.field;
					return data;
				case "mangleof":
					data.typeData = TypeData("string");
					data.name = token.text;
					data.tokenType = TokenType.field;
					return data;
				case "sizeof":
					data.typeData = TypeData("size_t");
					data.name = token.text;
					data.tokenType = TokenType.field;
					return data;
				case "stringof":
					data.typeData = TypeData("string");
					data.name = token.text;
					data.tokenType = TokenType.field;
					return data;
				default:
					break;
			}

			// Is an array
			if (var_data.type.isArray) {
				switch(member) {
					case "length":
						data.typeData = TypeData("size_t");
						data.name = token.text;
						data.tokenType = TokenType.field;
						return data;
					// FIXME: case "ptr":
					case "dup":
					case "idup":
					case "reverse":
					case "sort":
						data.typeData = var_data.type;
						data.name = token.text;
						data.tokenType = TokenType.field;
						return data;
					default:
						break;
				}
			// Is an integral type
			} else if (!std.algorithm.find(INTEGER_TYPES, identifier).empty) {
				switch(member) {
					case "max":
						data.typeData = TypeData("size_t");
						data.name = token.text;
						data.tokenType = TokenType.field;
						return data;
					case "min":
						data.typeData = TypeData("size_t");
						data.name = token.text;
						data.tokenType = TokenType.field;
						return data;
					default:
						break;
				}
			// Is a float type
			} else if (!std.algorithm.find(FLOAT_TYPES, identifier).empty) {
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
						data.typeData = var_data.type;
						data.name = token.text;
						data.tokenType = TokenType.field;
						return data;
					default:
						break;
				}
			}
			throw new Exception("!!! identifier:%s, member:%s, var:%s, type:%s".format(identifier, member, var_data, var_data.type));
		}

		// Token is a variable
		if (var_data !is VariableData.init && identifier) {
			data.typeData = var_data.type;
			data.name = token.text;
			data.tokenType = TokenType.variable;
			return data;
		}

		// Token is a template parameter
		auto temp_data = getTemplateDataByName(identifier);
		if (temp_data !is TemplateData.init) {
			data.tokenType = TokenType.template_;
			data.typeData = TypeData(identifier);
			data.name = null;
			stderr.writefln("!!! template data: %s", data);
			return data;
		}

		// Token is a function name
		auto func_data = getFunctionDataByName(identifier);
		if (func_data !is FunctionData.init) {
			data.tokenType = TokenType.function_;
			data.typeData = func_data.returnType;
			data.name = token.text;
			return data;
		}

		// Token is a class name
		auto class_data = getClassDataByName(identifier);
		if (class_data !is ClassData.init) {
			data.tokenType = TokenType.class_;
			data.typeData = TypeData(token.text);
			data.name = null;
			return data;
		}

		// Token is a struct name
		auto struct_data = getStructDataByName(identifier);
		if (struct_data !is StructData.init) {
			data.tokenType = TokenType.struct_;
			data.typeData = TypeData(token.text);
			data.name = null;
			return data;
		}

		// Token is an enum
		auto enum_data = getEnumDataByName(identifier);
		if (enum_data !is EnumData.init) {
			data.name = token.text;
			// Enum field
			if (member in enum_data.fields) {
				data.tokenType = TokenType.enum_;
				data.typeData = TypeData(identifier);
			// Enum
			} else {
				data.tokenType = TokenType.enum_;
				data.typeData = enum_data.type;
			}
			return data;
		}
	}

	// Token is an identifier that should have used a this pointer
	// blah instead of this.blah
	if (token.type == tok!"identifier" && thisPointers.peak) {
		// Token may be a field/method without the this pointer
		// Figure out what "this" should be
		auto class_data = getClassDataByName(thisPointers.peak);
		auto struct_data = getStructDataByName(thisPointers.peak);
		string identifier = token.text;

		// Class
		if (class_data !is ClassData.init) {
			// Token is a field
			if (identifier in class_data.fields) {
				data.tokenType = TokenType.this_field;
				data.typeData = class_data.fields[identifier].type;
				data.name = token.text;
				return data;
			// Token is a method
			} else if (identifier in class_data.methods) {
				data.tokenType = TokenType.this_method;
				data.typeData = class_data.methods[identifier].returnType;
				data.name = token.text;
				return data;
			}
		// Struct
		} else if (struct_data !is StructData.init) {
			// Token is a field
			if (identifier in struct_data.fields) {
				data.tokenType = TokenType.this_field;
				data.typeData = struct_data.fields[identifier].type;
				data.name = token.text;
				return data;
			// Token is a method
			} else if (identifier in struct_data.methods) {
				data.tokenType = TokenType.this_method;
				data.typeData = struct_data.methods[identifier].returnType;
				data.name = token.text;
				return data;
			}
		}
	}

	// Token uses a module
	if (token.type == tok!"identifier") {
		string full_identifier = token.text;
		string method = null;
		ModuleData module_data;

		// Match full module name
		foreach (mod; analysis.scope_frame.modules) {
			if (full_identifier.startsWith(mod.name) && full_identifier.length > mod.name.length) {
				module_data = mod;
				auto offset = mod.name.length + 1;
				method = full_identifier[offset .. $];
				goto got_module;
			}
		}

		// Match partial module name using imports
		foreach (frame; std.range.retro(analysis.scope_frame.frames)) {
			foreach (importName; frame.imports) {
				if (importName in analysis.scope_frame.modules) {
					auto candidate_module = analysis.scope_frame.modules[importName];

					// FIXME: Make it work with enum fields, static methods, and properties
					if (full_identifier in candidate_module.variables || 
						full_identifier in candidate_module.functions || 
						full_identifier in candidate_module.classes || 
						full_identifier in candidate_module.structs || 
						full_identifier in candidate_module.enums) {
						method = full_identifier;
						module_data = candidate_module;
					}
				}
				if (module_data !is ModuleData.init)
					goto got_module;
			}
		}

		got_module:
		// FIXME: Make it work with enum fields, static methods, and properties

		// Variable match
		if (method in module_data.variables) {
			auto var_data = module_data.variables[method];
			data.typeData = var_data.type;
			data.name = method;
			data.tokenType = TokenType.variable;
			return data;
		// Function match
		} else if (method in module_data.functions) {
			auto func_data = module_data.functions[method];
			data.tokenType = TokenType.function_;
			data.typeData = func_data.returnType;
			data.name = method;
			return data;
		// Class match
		} else if (method in module_data.classes) {
			auto class_data = module_data.classes[method];
			data.tokenType = TokenType.class_;
			data.typeData = TypeData(method);
			data.name = null;
			return data;
		// Struct match
		} else if (method in module_data.structs) {
			auto struct_data = module_data.structs[method];
			data.tokenType = TokenType.struct_;
			data.typeData = TypeData(method);
			data.name = null;
			return data;
		// Enum match
		} else if (method in module_data.enums) {
			auto enum_data = module_data.enums[method];
			data.name = method;
			data.tokenType = TokenType.enum_;
			data.typeData = enum_data.type;
			return data;
		}
	}

	// Token is an attribute
	if (token.type ==  tok!"identifier") {
		if (token.text == "property") {
			data.tokenType = TokenType.property_attribute;
			data.typeData = TypeData(token.text);
			data.name = null;
			return data;
		}
	}

	string fail = "!!! getTokenData() failed on token: type:%s, text:%s, line:%s, column:%s".format(
		token.type.str, token.text, data.line, data.column);
	//throw new Exception(fail);
	stderr.writeln(fail);
	return TokenData.init;
}



string tokenStr(const Token token)
{
	return "Token(type:%s, text:%s, line:%s, column:%s)".format(token.type.str, token.text, token.line, token.column);
}

Token combineTokens(const Token a, const Token b)
{
	//writefln("!!! combineTokens left:%s, right:%s", tokenStr(a), tokenStr(b));
	Token token;

	if (a is Token.init)
		return b;
	else if (b is Token.init)
		return a;

	// This pointer
	if (a.type == tok!"this") {
		token.type = a.type;
		token.text = b.text;
		token.line = a.line;
		token.column = a.column;
	// class/struct member
	} else if (a.type == tok!"identifier") {
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

