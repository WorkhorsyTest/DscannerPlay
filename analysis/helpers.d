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
import analysis.run;
import analysis.scope_analyzer;
import analysis.scope_frame;
import dlang_helper;

const string[] BASIC_TYPES = [
	"byte", "short", "int", "long",
	"ubyte", "ushort", "uint", "ulong",
	"size_t",
	"intptr_t", "uintptr_t",
	"intmax_t", "uintmax_t",
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
	"intmax_t", "uintmax_t",
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
	bool isImported = gScope.isImported(funcSet.importName);

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
	// Reset everything
	if (gScope)
		gScope.clear();

	// Run the code and get any warnings
	string[] rawWarnings = analyze("test", cast(ubyte[]) code, analyzers);
	string[] codeLines = code.split("\n");

	// Get the warnings ordered by line
	string[size_t] warnings;
	for (size_t i=0; i<rawWarnings.length; ++i)
	{
		// Skip the warning if it is on line zero
		size_t rawLine = std.conv.to!size_t(rawWarnings[i].between("test(", ":"));
		if (rawLine == 0)
		{
			stderr.writefln("??? Skipping warning because it is on line zero:\n%s", rawWarnings[i]);
			continue;
		}

		size_t warnLine = line - 1 + rawLine;
		warnings[warnLine] = rawWarnings[i].after(")");
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
		messages[lineNo] = commentPart;
	}

	// Throw an assert error if any messages are not listed in the warnings
	foreach (lineNo, message; messages)
	{
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

	// FIXME: Update this so the errors are ordered by line number
	// Throw an assert error if there were any warnings that were not expected
	string[] unexpectedWarnings;
	foreach (lineNo, warning; warnings)
	{
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
	if (gScope.isImported(importName))
		return;

	// Add the import
	stderr.writefln("??? importing: %s", importName);
	gScope.addImport(importName);

	// Add the module
	string fileName = "%s.d".format(importName);
	loadModule(fileName);
}

void loadModule(string fileName)
{
	// Just return if the file does not exist
	import std.file;
	if (!std.file.exists(fileName) || !std.file.isFile(fileName))
		return;

	// Read the code
	File f = File(fileName);
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
	Module mod = std.d.parser.parseModule(app.data, fileName, p, null);

	// Get data from the module
	ModuleData moduleData = getModuleData(mod);
	gScope.addModule(moduleData);
}

void declareFunction(const FunctionDeclaration funcDec)
{
	FunctionData data = getFunctionData(funcDec);
	gScope.addFunction(data);
}

void declareVariable(const VariableDeclaration varDec)
{
	foreach (varData; getVariableDatas(varDec))
	{
		gScope.addVariable(varData);
	}
}

void declareParameter(const Parameter param)
{
	// Just print a warning and return if the name is invalid
	if (param.name is Token.init || isNullOrBlank(param.name.text))
	{
		stderr.writeln("!!! declareParameter() failed because param.name was init or blank.");
		std.d.inspect.inspect(stderr, param, "Parameter");
		return;
	}

	VariableData varData;
	varData.name = param.name.text;
	varData.type = getTypeData(param.type);
	varData.isParameter = true;
	varData.line = param.name.line;
	varData.column = param.name.column;

	gScope.addVariable(varData);
}

void declareTemplates(const TemplateParameters tempParams)
{
	TemplateData[] tempDatas = getTemplateDatas(tempParams);
	foreach (tempData; tempDatas)
	{
		gScope.addTemplateParameter(tempData);
	}
}

void declareStruct(const StructDeclaration structDec)
{
	StructData structData = getStructData(structDec);
	gScope.addStruct(structData);
}

void declareClass(const ClassDeclaration classDec)
{
	ClassData classData = getClassData(classDec);
	gScope.addClass(classData);
}

void declareEnum(const EnumDeclaration enumDec)
{
	EnumData enumData = getEnumData(enumDec);
	gScope.addEnum(enumData);
}

void declareModule(const Module mod)
{
	ModuleData moduleData = getModuleData(mod);
	gScope.addModule(moduleData);
}

// FIXME: For some reason this never sets decoration.isProperty to true
Decoration getDeclarationDecorations(const Declaration decl)
{
	Decoration decoration;
	foreach (attr; decl.attributes)
	{
		// Skip if no storage class
		if (!attr.storageClass || attr.storageClass.token is Token.init)
			continue;

		// Reference
		if (attr.storageClass.token.type.str == "ref")
		{
			decoration.isRef = true;
		}
		// Auto
		else if (attr.storageClass.token.type.str == "auto")
		{
			decoration.isAuto = true;
		}

		// Skip if no at attribute
		if (!attr.storageClass.atAttribute || attr.storageClass.atAttribute.identifier is Token.init)
			continue;

		// Property
		Token iden = attr.storageClass.atAttribute.identifier;
		if (iden.type.str == "identifier" && iden.text == "property")
		{
			decoration.isProperty = true;
		}
	}

	return decoration;
}

VariableData[] getVariableDatas(const VariableDeclaration varDec)
{
	VariableData[] datas;

	// Using auto
	if (varDec.autoDeclaration)
	{
		string[] names = getVariableNames(varDec);
		if (!names)
		{
			stderr.writeln("??? getVariableDatas() failed to get variable name.");
			std.d.inspect.inspect(stderr, varDec, "VariableDeclaration");
			return null;
		}

		TokenData tokenData = getExpressionReturnTokenData(varDec.autoDeclaration);

		if (tokenData is TokenData.init)
		{
			stderr.writeln("??? getVariableDatas() failed to get valid token from auto variable declaration.");
			std.d.inspect.inspect(stderr, varDec, "VariableDeclaration");
			return null;
		}

		foreach (name; names)
		{
			VariableData data;
			data.name = name;
			data.type = tokenData.typeData;
			data.isParameter = false;
			data.line = tokenData.line;
			data.column = tokenData.column;
			if (data.type !is TypeData.init)
				datas ~= data;
		}
	}
	// Normal variable
	else
	{
		string[] names = getVariableNames(varDec);
		if (!names)
		{
			stderr.writeln("??? getVariableDatas() failed to get variable name.");
			std.d.inspect.inspect(stderr, varDec, "VariableDeclaration");
			return null;
		}

		TypeData type = getTypeData(varDec.type);
		foreach (name; names)
		{
			VariableData data;
			data.name = name;
			data.type = type;
			data.isParameter = false;
			getVariableLineColumn(varDec, name, data.line, data.column);
			if (data.type !is TypeData.init)
				datas ~= data;
		}
	}

	return datas;
}

TemplateData[] getTemplateDatas(const TemplateParameters templateParameters)
{
	TemplateData[] datas;
	if (templateParameters && templateParameters.templateParameterList)
	{
		foreach (item; templateParameters.templateParameterList.items)
		{
			if (item && item.templateTypeParameter)
			{
				auto identifier = item.templateTypeParameter.identifier;
				if (identifier !is Token.init)
				{
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

FunctionData getFunctionData(const FunctionDeclaration funcDec)
{
	FunctionData data;
	data.name = funcDec.name.text;
	data.templates = getTemplateDatas(funcDec.templateParameters);
	data.returnType = getFunctionReturnTypeData(funcDec);
	data.argTypes = getFunctionArgTypeDatas(funcDec);
	data.line = funcDec.name.line;
	data.column = funcDec.name.column;
	return data;
}

StructData getStructData(const StructDeclaration structDec)
{
	StructData data;
	data.name = structDec.name.text;
	data.line = structDec.name.line;
	data.column = structDec.name.column;

	foreach (decl; structDec.structBody.declarations)
	{
		if (decl.variableDeclaration)
		{
			foreach (varData; getVariableDatas(decl.variableDeclaration))
			{
				data.fields[varData.name] = varData;
			}
		}
		else if (decl.functionDeclaration)
		{
			FunctionData funcData = getFunctionData(decl.functionDeclaration);
			data.methods[funcData.name] = funcData;
		}
	}

	return data;
}

ClassData getClassData(const ClassDeclaration classDec)
{
	ClassData data;
	data.name = classDec.name.text;
	data.line = classDec.name.line;
	data.column = classDec.name.column;

	foreach (decl; classDec.structBody.declarations)
	{
		if (decl.variableDeclaration)
		{
			foreach (varData; getVariableDatas(decl.variableDeclaration))
			{
				data.fields[varData.name] = varData;
			}
		}
		else if (decl.functionDeclaration)
		{
			FunctionData funcData = getFunctionData(decl.functionDeclaration);
			data.methods[funcData.name] = funcData;
		}
	}
	return data;
}

EnumData getEnumData(const EnumDeclaration enumDec)
{
	EnumData data;
	data.name = enumDec.name.text;
	data.line = enumDec.name.line;
	data.column = enumDec.name.column;
	data.type = TypeData("int");
	try
	{
		data.type = getTypeData(enumDec.type);
	}
	catch (Exception ex)
	{
		//
	}

	foreach (member; enumDec.enumBody.enumMembers)
	{
		string name;
		auto fieldData = getEnumFieldData(member, name);
		data.fields[name] = fieldData;
	}

	return data;
}

FieldData getEnumFieldData(const EnumMember enumMember, ref string name)
{
	FieldData data;
	if (enumMember)
	{
		name = enumMember.name.text;
		data.line = enumMember.name.line;
		data.column = enumMember.name.column;
	}

	return data;
}

ModuleData getModuleData(const Module mod)
{
	ModuleData data;

	// FIXME: What do we do when a module has no name?
	// Get the module name
	if (mod
		&& mod.moduleDeclaration
		&& mod.moduleDeclaration.moduleName
		&& mod.moduleDeclaration.moduleName.identifiers.length)
	{
		string[] modNameChunks;
		foreach (identifier; mod.moduleDeclaration.moduleName.identifiers)
		{
			modNameChunks ~= identifier.text;
		}
		data.name = modNameChunks.join(".");
	}
	else
	{
		data.name = "unknown";
	}

	foreach (decl; mod.declarations)
	{
		// Add decorations such as properties, auto, ref, et cetera
		Decoration decoration = getDeclarationDecorations(decl);
		gScope.decorationsPush(decoration);

		if (decl.functionDeclaration)
		{
			FunctionData funcData = getFunctionData(decl.functionDeclaration);
			data.functions[funcData.name] = funcData;
		}
		else if (decl.variableDeclaration)
		{
			foreach (varData; getVariableDatas(decl.variableDeclaration))
			{
				data.variables[varData.name] = varData;
			}
		}
		else if (decl.structDeclaration)
		{
			StructData structData = getStructData(decl.structDeclaration);
			data.structs[structData.name] = structData;
		}
		else if (decl.classDeclaration)
		{
			ClassData classData = getClassData(decl.classDeclaration);
			data.classes[classData.name] = classData;
		}
		else if (decl.enumDeclaration)
		{
			EnumData enumData = getEnumData(decl.enumDeclaration);
			data.enums[enumData.name] = enumData;
		}

		// Remove decorations
		gScope.decorationsPop();
	}

	return data;
}

TokenData[] getFunctionCallArguments(const FunctionCallExpression funcCallExp)
{
	TokenData[] args;

	// Just return if there are no args
	if (!funcCallExp
		|| !funcCallExp.arguments
		|| !funcCallExp.arguments.argumentList
		|| !funcCallExp.arguments.argumentList.items)
	{
		return args;
	}

	// Get the token data for each arg
	foreach (item; funcCallExp.arguments.argumentList.items)
	{
		args ~= getExpressionReturnTokenData(item);
	}

	return args;
}

string getFunctionCallName(const FunctionCallExpression funcExp)
{
	string[] chunks;
	auto unaryExp = cast(UnaryExpression) funcExp.unaryExpression;

	// The name is in a token such as "super"
	if (funcExp
		&& funcExp.unaryExpression
		&& funcExp.unaryExpression.primaryExpression
		&& funcExp.unaryExpression.primaryExpression.primary !is Token.init)
	{
		return funcExp.unaryExpression.primaryExpression.primary.type.str;
	}

	// The name is in an identifier chain
	while (unaryExp)
	{
		// Part of the name
		if (unaryExp.identifierOrTemplateInstance)
		{
			// from a template
			if (unaryExp.identifierOrTemplateInstance.templateInstance)
			{
				chunks ~= unaryExp.identifierOrTemplateInstance.templateInstance.identifier.text;
			// from a function
			}
			else
			{
				chunks ~= unaryExp.identifierOrTemplateInstance.identifier.text;
			}
		}

		// The end of the name
		if (unaryExp.primaryExpression
			&& unaryExp.primaryExpression.identifierOrTemplateInstance)
		{
			chunks ~= unaryExp.primaryExpression.identifierOrTemplateInstance.identifier.text;
		}

		unaryExp = unaryExp.unaryExpression;
	}

	if (!chunks.length)
	{
		stderr.writefln("??? getFunctionCallName() failed to get name of function to call.");
		std.d.inspect.inspect(stderr, funcExp, "FunctionCallExpression");
		return null;
	}

	return chunks.reverse.join(".");
}

TypeData getFunctionReturnTypeData(const FunctionDeclaration funcDec)
{
	// Normal return type
	if (funcDec.returnType)
	{
		return getTypeData(funcDec.returnType);
	}

	// Auto return type
	auto decoration = gScope.decorationsPeak();
	if (decoration !is Decoration.init && decoration.isAuto)
	{
		return TypeData("auto");
	}

	return TypeData.init;
}

TypeData[] getFunctionArgTypeDatas(const FunctionDeclaration funcDec)
{
	if (funcDec && funcDec.parameters)
	{
		TypeData[] argTypes;
		foreach (param; funcDec.parameters.parameters)
		{
			argTypes ~= getTypeData(param.type);
		}
		return argTypes;
	}

	stderr.writefln("??? getFunctionArgTypeDatas() failed to find function arg type names.");
	std.d.inspect.inspect(stderr, funcDec, "FunctionDeclaration");
	return null;
}

string[] getFunctionArgNames(const FunctionDeclaration funcDec)
{
	if (funcDec && funcDec.parameters)
	{
		string[] argNames;
		foreach (param; funcDec.parameters.parameters)
		{
			argNames ~= param.name.text;
		}
		return argNames;
	}

	stderr.writefln("??? getFunctionArgNames() filed to get function arg names.");
	std.d.inspect.inspect(stderr, funcDec, "FunctionDeclaration");
	return null;
}

string[] getVariableNames(const VariableDeclaration varDec)
{
	string[] retval;
	if (varDec)
	{
		if (varDec.autoDeclaration)
		{
			foreach (iden; varDec.autoDeclaration.identifiers)
			{
				retval ~= iden.text;
			}
		}
		else
		{
			foreach (d; varDec.declarators)
			{
				retval ~= d.name.text;
			}
		}
	}

	if (retval.length)
		return retval;

	stderr.writefln("??? getVariableNames() failed to find variable names.");
	std.d.inspect.inspect(stderr, varDec, "VariableDeclaration");
	return null;
}

void getVariableLineColumn(const VariableDeclaration varDec, string name, ref size_t line, ref size_t column)
{
	if (varDec)
	{
		if (varDec.autoDeclaration)
		{
			foreach (iden; varDec.autoDeclaration.identifiers)
			{
				if (iden.text == name)
				{
					line = iden.line;
					column = iden.column;
					return;
				}
			}
		}

		foreach (d; varDec.declarators)
		{
			if (d.name.text == name)
			{
				line = d.name.line;
				column = d.name.column;
				return;
			}
		}
	}

	stderr.writefln("??? getVariableLineColumn() failed to find variable line and column.");
	std.d.inspect.inspect(stderr, varDec, "VariableDeclaration");
}

void markUsedVariables(const ASTNode node)
{
	auto unaryExp = cast(const UnaryExpression) node;
	auto ternaryExp = cast(const TernaryExpression) node;
	if (unaryExp || ternaryExp)
	{
		TokenData data;
		if (unaryExp)
			data = getExpressionReturnTokenData(unaryExp);
		else
			data = getExpressionReturnTokenData(ternaryExp);

		if (data !is TokenData.init && data.name && 
			(data.tokenType == TokenType.variable || data.tokenType == TokenType.field || data.tokenType == TokenType.method))
		{
			string name = data.name.before(".");
			gScope.setVariableIsUsedByName(name);
		}
	}
	else if (auto exp = cast(const AssignExpression) node)
	{
		markUsedVariables(exp.ternaryExpression);
	}
	else if (auto exp = cast(const AssertExpression) node)
	{
		markUsedVariables(exp.assertion);
	}
	else if (auto exp = cast(const CmpExpression) node)
	{
		markUsedVariables(exp.shiftExpression);
		markUsedVariables(exp.equalExpression);
		markUsedVariables(exp.identityExpression);
		markUsedVariables(exp.relExpression);
		markUsedVariables(exp.inExpression);
	}
	else if (auto exp = cast(AddExpression) node)
	{
		markUsedVariables(exp.left);
		markUsedVariables(exp.right);
	}
	else if (auto exp = cast(AndAndExpression) node)
	{
		markUsedVariables(exp.left);
		markUsedVariables(exp.right);
	}
	else if (auto exp = cast(AndExpression) node)
	{
		markUsedVariables(exp.left);
		markUsedVariables(exp.right);
	}
	else if (auto exp = cast(EqualExpression) node)
	{
		markUsedVariables(exp.left);
		markUsedVariables(exp.right);
	}
	else if (auto exp = cast(IdentityExpression) node)
	{
		markUsedVariables(exp.left);
		markUsedVariables(exp.right);
	}
	else if (auto exp = cast(InExpression) node)
	{
		markUsedVariables(exp.left);
		markUsedVariables(exp.right);
	}
	else if (auto exp = cast(MulExpression) node)
	{
		markUsedVariables(exp.left);
		markUsedVariables(exp.right);
	}
	else if (auto exp = cast(OrExpression) node)
	{
		markUsedVariables(exp.left);
		markUsedVariables(exp.right);
	}
	else if (auto exp = cast(OrOrExpression) node)
	{
		markUsedVariables(exp.left);
		markUsedVariables(exp.right);
	}
	else if (auto exp = cast(PowExpression) node)
	{
		markUsedVariables(exp.left);
		markUsedVariables(exp.right);
	}
	else if (auto exp = cast(RelExpression) node)
	{
		markUsedVariables(exp.left);
		markUsedVariables(exp.right);
	}
	else if (auto exp = cast(ShiftExpression) node)
	{
		markUsedVariables(exp.left);
		markUsedVariables(exp.right);
	}
	else if (auto exp = cast(XorExpression) node)
	{
		markUsedVariables(exp.left);
		markUsedVariables(exp.right);
	}
	else if (node)
	{
		stderr.writefln("!!! markUsedVariables() override needed for: '%s'".format(typeid(node)));
	}
}

TypeData getTypeData(const Type type)
{
	TypeData typeData;
	if (type && type.type2)
	{
		// Check for a nested type
		if (type.type2.type)
			return getTypeData(type.type2.type);

		// Check if it is an array
		foreach (suffix; type.typeSuffixes)
		{
			if (suffix.array)
				typeData.isArray = true;
		}

		// Get type from builtinType
		if (type.type2.builtinType !is IdType.init)
		{
			typeData.name = type.type2.builtinType.str;
			return typeData;
		}

		// Get type from symbol
		if (type.type2.symbol && type.type2.symbol.identifierOrTemplateChain)
		{

			auto chain = type.type2.symbol.identifierOrTemplateChain.identifiersOrTemplateInstances;
			string[] identifiers;
			foreach (idenOrTemp; chain)
			{
				identifiers ~= idenOrTemp.identifier.text;
			}
			typeData.name = identifiers.join(".");
			return typeData;
		}
	}

	stderr.writeln("!!! getTypeData() failed on type");
	std.d.inspect.inspect(stderr, type, "Type");
	return TypeData.init;
}

bool isSameTokenVariable(const TokenData a, const TokenData b)
{
	return
		a !is TokenData.init && b !is TokenData.init
		&& (a.tokenType == TokenType.variable && b.tokenType == TokenType.variable
		|| a.tokenType == TokenType.field && b.tokenType == TokenType.field)
		&& a.name && b.name
		&& a.name == b.name;
}

TypeData getExpressionReturnType(const ASTNode node, ref size_t line, ref size_t column)
{
	TokenData data = getExpressionReturnTokenData(node);
	line = data.line;
	column = data.column;
	return data.typeData;
}

TokenData getExpressionReturnTokenData(const ASTNode node)
{
	Token token = getExpressionReturnToken(node, 0);
	TokenData data = getTokenData(token);
	return data;
}

Token getExpressionReturnToken(const ASTNode node, size_t indent)
{
	//if (node !is null)
	//	stderr.writefln("%s??? getExpressionReturnToken: %s", pad(indent++), typeid(node));

	if (auto addExp = cast(const AddExpression) node)
	{
		auto l = getExpressionReturnToken(addExp.left, indent);
		auto r = getExpressionReturnToken(addExp.right, indent);
		return getPromotedToken(l, r);
	}
	else if (auto andAndExp = cast(const AndAndExpression) node)
	{
		auto l = getExpressionReturnToken(andAndExp.left, indent);
		auto r = getExpressionReturnToken(andAndExp.right, indent);
		return Token(tok!"identifier", "bool", l.line, l.column, l.index);
	}
	else if (auto andExp = cast(const AndExpression) node)
	{
		auto l = getExpressionReturnToken(andExp.left, indent);
		auto r = getExpressionReturnToken(andExp.right, indent);
		return getPromotedToken(l, r);
	}
	else if (auto arrayInit = cast(const ArrayInitializer) node)
	{
		foreach (memberInit; arrayInit.arrayMemberInitializations)
			if (memberInit)
				return getExpressionReturnToken(memberInit, indent);
	}
	else if (auto arrayLit = cast(const ArrayLiteral) node)
	{
		return getExpressionReturnToken(arrayLit.argumentList, indent);
	}
	else if (auto arrayMemInit = cast(const ArrayMemberInitialization) node)
	{
		if (arrayMemInit.assignExpression)
			return getExpressionReturnToken(arrayMemInit.assignExpression, indent);
		if (arrayMemInit.nonVoidInitializer)
			return getExpressionReturnToken(arrayMemInit.nonVoidInitializer, indent);
	}
	else if (auto argList = cast(const ArgumentList) node)
	{
		foreach (item; argList.items)
			if (item)
				return getExpressionReturnToken(item, indent);
	}
	else if (auto asserExp = cast(const AssertExpression) node)
	{
		return getExpressionReturnToken(asserExp.assertion, indent);
	}
	else if (auto assExp = cast(const AssignExpression) node)
	{
		if (assExp.ternaryExpression)
			return getExpressionReturnToken(assExp.ternaryExpression, indent);
		if (assExp.assignExpression)
			return getExpressionReturnToken(assExp.assignExpression, indent);
	}
	else if (auto autoDec = cast(const AutoDeclaration) node)
	{
		foreach (init; autoDec.initializers)
			if (init)
				return getExpressionReturnToken(init, indent);

		foreach (iden; autoDec.identifiers)
			if (iden !is Token.init)
				return iden;
	}
	else if (auto castExp = cast(const CastExpression) node)
	{
		if (castExp.type)
			return getExpressionReturnToken(castExp.type, indent);
		if (castExp.castQualifier)
			return getExpressionReturnToken(castExp.castQualifier, indent);
		if (castExp.unaryExpression)
			return getExpressionReturnToken(castExp.unaryExpression, indent);
	}
	else if (auto cmpExp = cast(const CmpExpression) node)
	{
		if (cmpExp.shiftExpression)
			return getExpressionReturnToken(cmpExp.shiftExpression, indent);
		if (cmpExp.equalExpression)
			return getExpressionReturnToken(cmpExp.equalExpression, indent);
		if (cmpExp.identityExpression)
			return getExpressionReturnToken(cmpExp.identityExpression, indent);
		if (cmpExp.relExpression)
			return getExpressionReturnToken(cmpExp.relExpression, indent);
		if (cmpExp.inExpression)
			return getExpressionReturnToken(cmpExp.inExpression, indent);
	}
	else if (auto decl = cast(const Declarator) node)
	{
		if (decl.initializer)
			return getExpressionReturnToken(decl.initializer, indent);
		if (decl.name !is Token.init)
			return decl.name;
	}
	else if (auto delExp = cast(const DeleteExpression) node)
	{
		if (delExp.unaryExpression)
			return getExpressionReturnToken(delExp.unaryExpression, indent);
	}
	else if (auto eqlExp = cast(const EqualExpression) node)
	{
		auto l = getExpressionReturnToken(eqlExp.left, indent);
		auto r = getExpressionReturnToken(eqlExp.right, indent);
		return Token(tok!"identifier", "bool", l.line, l.column, l.index);
	}
	else if (auto expExp = cast(const Expression) node)
	{
		foreach (item; expExp.items)
			if (item)
				return getExpressionReturnToken(item, indent);
	}
	else if (auto funCallExp = cast(const FunctionCallExpression) node)
	{
		// FIXME: This breaks with UFC
		if (funCallExp.unaryExpression)
			return getExpressionReturnToken(funCallExp.unaryExpression, indent);
	}
	else if (auto idenOrTemp = cast(const IdentifierOrTemplateChain) node)
	{
		if (idenOrTemp)
			foreach (inst; idenOrTemp.identifiersOrTemplateInstances)
				if (inst)
					return getExpressionReturnToken(inst, indent);
	}
	else if (auto idenOrTemp = cast(const IdentifierOrTemplateInstance) node)
	{
		if (idenOrTemp.templateInstance)
			return getExpressionReturnToken(idenOrTemp.templateInstance, indent);
		if (idenOrTemp.identifier !is Token.init)
			return idenOrTemp.identifier;
	}
	else if (auto idenExp = cast(const IdentityExpression) node)
	{
		auto l = getExpressionReturnToken(idenExp.left, indent);
		auto r = getExpressionReturnToken(idenExp.right, indent);
		return Token(tok!"identifier", "bool", l.line, l.column, l.index);
	}
	else if (auto impExp = cast(const ImportExpression) node)
	{
		if (impExp.assignExpression)
			return getExpressionReturnToken(impExp.assignExpression, indent);
	}
	else if (auto indexExp = cast(const IndexExpression) node)
	{
		if (indexExp.unaryExpression)
			return getExpressionReturnToken(indexExp.unaryExpression, indent);
	}
	else if (auto inExp = cast(const InExpression) node)
	{
		auto l = getExpressionReturnToken(inExp.left, indent);
		auto r = getExpressionReturnToken(inExp.right, indent);
		return Token(tok!"identifier", "bool", l.line, l.column, l.index);
	}
	else if (auto intl = cast(const Initializer) node)
	{
		if (intl.nonVoidInitializer)
			return getExpressionReturnToken(intl.nonVoidInitializer, indent);
	}
	else if (auto lambdaExp = cast(const LambdaExpression) node)
	{
		if (lambdaExp.parameters)
			return getExpressionReturnToken(lambdaExp.parameters, indent);
		foreach (functionAttribute; lambdaExp.functionAttributes)
			if (functionAttribute)
				return getExpressionReturnToken(functionAttribute, indent);
		if (lambdaExp.assignExpression)
			return getExpressionReturnToken(lambdaExp.assignExpression, indent);
		// FIXME: Get the real line and column
		if (lambdaExp.functionType !is IdType.init)
			return Token(tok!"identifier", lambdaExp.functionType.str, 0, 0, 0);
		if (lambdaExp.identifier !is Token.init)
			return lambdaExp.identifier;
	}
	else if (auto mulExp = cast(const MulExpression) node)
	{
		auto l = getExpressionReturnToken(mulExp.left, indent);
		auto r = getExpressionReturnToken(mulExp.right, indent);
		return getPromotedToken(l, r);
	}
	else if (auto newExp = cast(const NewExpression) node)
	{
		if (newExp.type)
			return getExpressionReturnToken(newExp.type, indent);
		if (newExp.newAnonClassExpression)
			return getExpressionReturnToken(newExp.newAnonClassExpression, indent);
		if (newExp.arguments)
			return getExpressionReturnToken(newExp.arguments, indent);
		if (newExp.assignExpression)
			return getExpressionReturnToken(newExp.assignExpression, indent);
	}
	else if (auto nonVoidIntl = cast(const NonVoidInitializer) node)
	{
		if (nonVoidIntl.assignExpression)
			return getExpressionReturnToken(nonVoidIntl.assignExpression, indent);
		if (nonVoidIntl.arrayInitializer)
			return getExpressionReturnToken(nonVoidIntl.arrayInitializer, indent);
		if (nonVoidIntl.structInitializer)
			return getExpressionReturnToken(nonVoidIntl.structInitializer, indent);
	}
	else if (auto orExp = cast(const OrExpression) node)
	{
		auto l = getExpressionReturnToken(orExp.left, indent);
		auto r = getExpressionReturnToken(orExp.right, indent);
		return getPromotedToken(l, r);
	}
	else if (auto orOrExp = cast(const OrOrExpression) node)
	{
		auto l = getExpressionReturnToken(orOrExp.left, indent);
		auto r = getExpressionReturnToken(orOrExp.right, indent);
		return Token(tok!"identifier", "bool", l.line, l.column, l.index);
	}
	else if (auto postIncDecExp = cast(const PostIncDecExpression) node)
	{
		if (postIncDecExp.unaryExpression)
			return getExpressionReturnToken(postIncDecExp.unaryExpression, indent);
	}
	else if (auto powExp = cast(const PowExpression) node)
	{
		auto l = getExpressionReturnToken(powExp.left, indent);
		auto r = getExpressionReturnToken(powExp.right, indent);
		return getPromotedToken(l, r);
	}
	else if (auto preIncDecExp = cast(const PreIncDecExpression) node)
	{
		if (preIncDecExp.unaryExpression)
			return getExpressionReturnToken(preIncDecExp.unaryExpression, indent);
	}
	else if (auto primaryExp = cast(const PrimaryExpression) node)
	{
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
		if (primaryExp.identifierOrTemplateInstance)
			return getExpressionReturnToken(primaryExp.identifierOrTemplateInstance, indent);

		// return type
		if (primaryExp.dot !is Token.init)
			return primaryExp.dot;
		if (primaryExp.primary !is Token.init)
			return primaryExp.primary;
		if (primaryExp.basicType !is Token.init)
			return primaryExp.basicType;
	}
	else if (auto relExp = cast(const RelExpression) node)
	{
		auto l = getExpressionReturnToken(relExp.left, indent);
		auto r = getExpressionReturnToken(relExp.right, indent);
		return Token(tok!"identifier", "bool", l.line, l.column, l.index);
	}
	else if (auto shiftExp = cast(const ShiftExpression) node)
	{
		auto l = getExpressionReturnToken(shiftExp.left, indent);
		auto r = getExpressionReturnToken(shiftExp.right, indent);
		return getPromotedToken(l, r);
	}
	else if (auto sliceExp = cast(const SliceExpression) node)
	{
		if (sliceExp.unaryExpression)
			return getExpressionReturnToken(sliceExp.unaryExpression, indent);
	}
	else if (auto symbol = cast(const Symbol) node)
	{
		if (symbol.identifierOrTemplateChain)
			return getExpressionReturnToken(symbol.identifierOrTemplateChain, indent);
	}
	else if (auto tempArgList = cast(const TemplateArgumentList) node)
	{
		foreach (item; tempArgList.items)
			if (item)
				return getExpressionReturnToken(item, indent);
	}
	else if (auto tempArg = cast(const TemplateArgument) node)
	{
		if (tempArg.type)
			return getExpressionReturnToken(tempArg.type, indent);
		if (tempArg.assignExpression)
			return getExpressionReturnToken(tempArg.assignExpression, indent);
	}
	else if (auto tempArgs = cast(const TemplateArguments) node)
	{
		if (tempArgs.templateArgumentList)
			return getExpressionReturnToken(tempArgs.templateArgumentList, indent);
		if (tempArgs.templateSingleArgument)
			return getExpressionReturnToken(tempArgs.templateSingleArgument, indent);
	}
	else if (auto tempIns = cast(const TemplateInstance) node)
	{
		if (tempIns.templateArguments)
			return getExpressionReturnToken(tempIns.templateArguments, indent);
		if (tempIns.identifier !is Token.init)
			return tempIns.identifier;
	}
	else if (auto ternaryExp = cast(const TernaryExpression) node)
	{
		if (ternaryExp.orOrExpression)
			return getExpressionReturnToken(ternaryExp.orOrExpression, indent);
		if (ternaryExp.expression)
			return getExpressionReturnToken(ternaryExp.expression, indent);
		if (ternaryExp.ternaryExpression)
			return getExpressionReturnToken(ternaryExp.ternaryExpression, indent);
	}
	else if (auto tempSingArg = cast(const TemplateSingleArgument) node)
	{
		if (getTokenData(tempSingArg.token) !is TokenData.init)
			return tempSingArg.token;
	}
	else if (auto type = cast(const Type) node)
	{
		if (type.type2)
			return getExpressionReturnToken(type.type2, indent);
	}
	else if (auto type2 = cast(const Type2) node)
	{
		if (type2.symbol)
			return getExpressionReturnToken(type2.symbol, indent);
		if (type2.typeofExpression)
			return getExpressionReturnToken(type2.typeofExpression, indent);
		if (type2.type)
			return getExpressionReturnToken(type2.type, indent);
		if (type2.identifierOrTemplateChain)
			return getExpressionReturnToken(type2.identifierOrTemplateChain, indent);
		// FIXME: Get the real line and column
		if (type2.builtinType !is IdType.init)
			return Token(tok!"identifier", type2.builtinType.str, 0, 0, 0);
		// FIXME: Get the real line and column
		if (type2.typeConstructor !is IdType.init)
			return Token(tok!"identifier", type2.typeConstructor.str, 0, 0, 0);
	}
	else if (auto typeidExp = cast(const TypeidExpression) node)
	{
		if (typeidExp.expression)
			return getExpressionReturnToken(typeidExp.expression, indent);
		if (typeidExp.type)
			return getExpressionReturnToken(typeidExp.type, indent);
	}
	else if (auto typeofExp = cast(const TypeofExpression) node)
	{
		if (typeofExp.expression)
			return getExpressionReturnToken(typeofExp.expression, indent);
		if (typeofExp.return_ !is Token.init)
			return typeofExp.return_;
	}
	else if (auto unaryExp = cast(const UnaryExpression) node)
	{
		// Get the prefix such as "this."
		Token firstToken, secondToken;
		if (unaryExp.unaryExpression)
			firstToken = getExpressionReturnToken(unaryExp.unaryExpression, indent);

		// Get the second token
		if (unaryExp.type)
			secondToken = getExpressionReturnToken(unaryExp.type, indent);
		if (unaryExp.primaryExpression)
			secondToken = getExpressionReturnToken(unaryExp.primaryExpression, indent);
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
		if (unaryExp.assertExpression)
			secondToken = getExpressionReturnToken(unaryExp.assertExpression, indent);
		if (unaryExp.sliceExpression)
			secondToken = getExpressionReturnToken(unaryExp.sliceExpression, indent);
		if (unaryExp.indexExpression)
			secondToken = getExpressionReturnToken(unaryExp.indexExpression, indent);
		if (unaryExp.identifierOrTemplateInstance)
			secondToken = getExpressionReturnToken(unaryExp.identifierOrTemplateInstance, indent);
		//if (unaryExp.prefix)
		//	secondToken = getExpressionReturnToken(unaryExp.prefix, indent);
		//if (unaryExp.suffix)
		//	secondToken = getExpressionReturnToken(unaryExp.suffix, indent);

		// Combine the tokens
		// FIXME: UFC Boom
		Token newToken = combineTokens(firstToken, secondToken);
		return newToken;
	}
	else if (auto xorExp = cast(const XorExpression) node)
	{
		auto l = getExpressionReturnToken(xorExp.left, indent);
		auto r = getExpressionReturnToken(xorExp.right, indent);
		return getPromotedToken(l, r);
	}

	if (node !is null)
		stderr.writefln("!!! getExpressionReturnToken() failed on node: %s", typeid(node));

	return Token.init;
}

// FIXME: Much of this can be replaced with some isBlah functions
const Token getPromotedToken(const Token left, const Token right)
{
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

	// print an error if any type names are blank
	if (isNullOrBlank(a) || isNullOrBlank(b))
	{
		string message = "!!! getPromotedToken() failed on tokens: '%s' or '%s'.".format(
			getTokenData(left), getTokenData(right));
		stderr.writeln(message);
		return Token.init;
	}

	// print an error if any type names are unknown
	if (a !in promotions || a !in sizes
		|| b !in promotions || b !in sizes)
	{
		string message = "!!! getPromotedToken() failed with the type name: '%s' or '%s'.".format(a, b);
		stderr.writeln(message);
		return Token.init;
	}

	// Types are the same, so return the promotion of one
	if (a == b)
		return Token(tok!"identifier", promotions[a], left.line, left.column, left.index);

	// Types are different, so return the promotion of the larger
	if (sizes[a] > sizes[b])
		return Token(tok!"identifier", promotions[a], left.line, left.column, left.index);
	else
		return Token(tok!"identifier", promotions[b], right.line, right.column, right.index);
}

// FIXME: Make this work with UFC for variables and literals
TokenData getTokenData(const Token token)
{
	TokenData data;

	// Line and column
	if (token.line && token.column)
	{
		data.line = token.line;
		data.column = token.column;
	}

	// Token is empty
	if (token is Token.init)
	{
		return TokenData.init;
	}

	// Token is null
	if (token.type == tok!"null")
	{
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
	if (token.type in LITERAL_TYPE_TO_TYPE_MAP)
	{
		string typeName = LITERAL_TYPE_TO_TYPE_MAP[token.type];
		data.typeData = TypeData(typeName);
		data.tokenType = TokenType.literal;
		data.name = null;
		data.value = token.text;
		return data;
	}

	// Token is a bool literal
	if (token.type == tok!"true" || token.type == tok!"false")
	{
		data.tokenType = TokenType.literal;
		data.typeData = TypeData("bool");
		data.name = null;
		data.value = token.type.str;
		return data;
	}

	// Token is a basic type
	if (token.type == tok!"identifier")
	{
		if (!std.algorithm.find(BASIC_TYPES, token.text).empty)
		{
			data.tokenType = TokenType.basic_type;
			data.typeData = TypeData(token.text);
			data.name = null;
			return data;
		}
	}

	// Token is an auto declaration
	if (token.type ==  tok!"auto")
	{
		data.tokenType = TokenType.auto_declaration;
		data.typeData = TypeData.init;
		data.name = null;
		return data;
	}

	// Token is an ref declaration
	if (token.type ==  tok!"ref")
	{
		data.tokenType = TokenType.ref_declaration;
		data.typeData = TypeData.init;
		data.name = null;
		return data;
	}

	// Token is __FILE__
	if (token.type == tok!"__FILE__")
	{
		data.tokenType = TokenType.__file__;
		data.name = null;
		data.typeData = TypeData("string");
		return data;
	}

	// Token is __LINE__
	if (token.type == tok!"__LINE__")
	{
		data.tokenType = TokenType.__line__;
		data.name = null;
		data.typeData = TypeData("int");
		return data;
	}

	// Token is super
	if (token.type == tok!"super")
	{
		data.tokenType = TokenType.super_;
		data.name = null;
		data.typeData = TypeData.init;
		return data;
	}

	// Token is identifier with "this pointer" prefix
	// this.blah
	if (token.type == tok!"this" && token.text && token.text.length && gScope.thisPointersPeak())
	{
		string member = token.text;

		// Figure out what "this" is
		auto classData = gScope.getClass(gScope.thisPointersPeak());
		auto structData = gScope.getStruct(gScope.thisPointersPeak());

		// Class
		if (classData !is ClassData.init)
		{
			// Token is a field
			if (member in classData.fields)
			{
				data.tokenType = TokenType.this_field;
				data.name = token.text;
				data.typeData = classData.fields[member].type;
				return data;
			}
			// Token is a method
			else if (member in classData.methods)
			{
				data.tokenType = TokenType.this_method;
				data.name = token.text;
				data.typeData = classData.methods[member].returnType;
				return data;
			}
		}
		// Struct
		else if (structData !is StructData.init)
		{
			// Token is a field
			if (member in structData.fields)
			{
				data.tokenType = TokenType.this_field;
				data.name = token.text;
				data.typeData = structData.fields[member].type;
				return data;
			}
			// Token is a method
			else if (member in structData.methods)
			{
				data.tokenType = TokenType.this_method;
				data.name = token.text;
				data.typeData = structData.methods[member].returnType;
				return data;
			}
		}
	}

	// Token is just the "this pointer"
	// this
	if (token.type == tok!"this" && gScope.thisPointersPeak())
	{
		data.tokenType = TokenType.this_;
		data.name = "this";
		data.typeData = TypeData(gScope.thisPointersPeak());
		return data;
	}

	// Token is an identifier
	if (token.type == tok!"identifier")
	{
		// If the identifier has a dot "blah.member", then it is an 
		// identifier and member
		string identifier = null;
		string member = null;
		if (token.text.indexOf(".") != -1)
		{
			identifier = token.text.before(".");
			member = token.text.after(".");
		}
		else
		{
			identifier = token.text;
		}

		auto varData = gScope.getVariable(identifier);

		// Token is a struct/class instance
		if (varData !is VariableData.init)
		{
			string typeName = varData.type.name;
			auto classData = gScope.getClass(typeName);
			auto structData = gScope.getStruct(typeName);

			// Class instance member
			if (member && classData !is ClassData.init)
			{
				// Class instance field
				if (member in classData.fields)
				{
					data.typeData = classData.fields[member].type;
					data.name = token.text;
					data.tokenType = TokenType.field;
				}
				// Class instance method
				else if (member in classData.methods)
				{
					data.typeData = classData.methods[member].returnType;
					data.name = token.text;
					data.tokenType = TokenType.method;
				}
				return data;
			}
			// Struct instance member
			else if (member && structData !is StructData.init)
			{
				// Struct instance field
				if (member in structData.fields)
				{
					data.typeData = structData.fields[member].type;
					data.name = token.text;
					data.tokenType = TokenType.field;
				}
				// Struct instance method
				else if (member in structData.methods)
				{
					data.typeData = structData.methods[member].returnType;
					data.name = token.text;
					data.tokenType = TokenType.method;
				}
				return data;
			}
		}

		// Token is a variable with member
		if (varData !is VariableData.init && member)
		{
			// Is a standard property
			switch (member)
			{
				case "alignof":
					data.typeData = TypeData("size_t");
					data.name = token.text;
					data.tokenType = TokenType.field;
					return data;
				case "init":
					data.typeData = varData.type;
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
			if (varData.type.isArray)
			{
				switch (member)
				{
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
						data.typeData = varData.type;
						data.name = token.text;
						data.tokenType = TokenType.field;
						return data;
					default:
						break;
				}
			}
			// Is an integral type
			else if (!std.algorithm.find(INTEGER_TYPES, identifier).empty)
			{
				switch (member)
				{
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
			}
			// Is a float type
			else if (!std.algorithm.find(FLOAT_TYPES, identifier).empty)
			{
				switch (member)
				{
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
						data.typeData = varData.type;
						data.name = token.text;
						data.tokenType = TokenType.field;
						return data;
					default:
						break;
				}
			}
			string message = "!!! getTokenData() failed on identifier:%s, member:%s, var:%s, type:%s".format(identifier, member, varData, varData.type);
			stderr.writeln(message);
			return TokenData.init;
		}

		// Token is a variable
		if (varData !is VariableData.init && identifier)
		{
			data.typeData = varData.type;
			data.name = token.text;
			data.tokenType = TokenType.variable;
			return data;
		}

		// Token is a template parameter
		auto tempData = gScope.getTemplate(identifier);
		if (tempData !is TemplateData.init)
		{
			data.tokenType = TokenType.template_;
			data.typeData = TypeData(identifier);
			data.name = null;
			return data;
		}

		// Token is a function name
		auto funcData = gScope.getFunction(identifier);
		if (funcData !is FunctionData.init)
		{
			data.tokenType = TokenType.function_;
			data.typeData = funcData.returnType;
			data.name = token.text;
			return data;
		}

		// Token is a class name
		auto classData = gScope.getClass(identifier);
		if (classData !is ClassData.init)
		{
			data.tokenType = TokenType.class_;
			data.typeData = TypeData(token.text);
			data.name = null;
			return data;
		}

		// Token is a struct name
		auto structData = gScope.getStruct(identifier);
		if (structData !is StructData.init)
		{
			data.tokenType = TokenType.struct_;
			data.typeData = TypeData(token.text);
			data.name = null;
			return data;
		}

		// Token is an enum
		auto enumData = gScope.getEnum(identifier);
		if (enumData !is EnumData.init)
		{
			data.name = token.text;
			// Enum field
			if (member in enumData.fields)
			{
				data.tokenType = TokenType.enum_;
				data.typeData = TypeData(identifier);
			}
			// Enum
			else
			{
				data.tokenType = TokenType.enum_;
				data.typeData = enumData.type;
			}
			return data;
		}
	}

	// Token is an identifier that should have used a this pointer
	// blah instead of this.blah
	if (token.type == tok!"identifier" && gScope.thisPointersPeak())
	{
		// Token may be a field/method without the this pointer
		// Figure out what "this" should be
		auto classData = gScope.getClass(gScope.thisPointersPeak());
		auto structData = gScope.getStruct(gScope.thisPointersPeak());
		string identifier = token.text;

		// Class
		if (classData !is ClassData.init)
		{
			// Token is a field
			if (identifier in classData.fields)
			{
				data.tokenType = TokenType.this_field;
				data.typeData = classData.fields[identifier].type;
				data.name = token.text;
				return data;
			}
			// Token is a method
			else if (identifier in classData.methods)
			{
				data.tokenType = TokenType.this_method;
				data.typeData = classData.methods[identifier].returnType;
				data.name = token.text;
				return data;
			}
		}
		// Struct
		else if (structData !is StructData.init)
		{
			// Token is a field
			if (identifier in structData.fields)
			{
				data.tokenType = TokenType.this_field;
				data.typeData = structData.fields[identifier].type;
				data.name = token.text;
				return data;
			// Token is a method
			}
			else if (identifier in structData.methods)
			{
				data.tokenType = TokenType.this_method;
				data.typeData = structData.methods[identifier].returnType;
				data.name = token.text;
				return data;
			}
		}
	}

	// Token uses a module
	if (token.type == tok!"identifier")
	{
		string fullIdentifier = token.text;
		string method = null;
		ModuleData moduleData;

		// Match full module name
		foreach (mod; gScope.modules)
		{
			if (fullIdentifier.startsWith(mod.name) && fullIdentifier.length > mod.name.length)
			{
				moduleData = mod;
				auto offset = mod.name.length + 1;
				method = fullIdentifier[offset .. $];
				goto got_module;
			}
		}

		// Match partial module name using imports
		foreach (frame; std.range.retro(gScope.frames))
		{
			foreach (importName; frame.imports)
			{
				if (importName in gScope.modules)
				{
					auto candidateModule = gScope.modules[importName];

					// FIXME: Make it work with enum fields, static methods, and properties
					if (fullIdentifier in candidateModule.variables
						|| fullIdentifier in candidateModule.functions
						|| fullIdentifier in candidateModule.classes
						|| fullIdentifier in candidateModule.structs
						|| fullIdentifier in candidateModule.enums)
					{
						method = fullIdentifier;
						moduleData = candidateModule;
					}
				}
				if (moduleData !is ModuleData.init)
					goto got_module;
			}
		}

		got_module:
		// FIXME: Make it work with enum fields, static methods, and properties

		// Variable match
		if (method in moduleData.variables)
		{
			auto varData = moduleData.variables[method];
			data.typeData = varData.type;
			data.name = method;
			data.tokenType = TokenType.variable;
			return data;
		}
		// Function match
		else if (method in moduleData.functions)
		{
			auto funcData = moduleData.functions[method];
			data.tokenType = TokenType.function_;
			data.typeData = funcData.returnType;
			data.name = method;
			return data;
		}
		// Class match
		else if (method in moduleData.classes)
		{
			auto classData = moduleData.classes[method];
			data.tokenType = TokenType.class_;
			data.typeData = TypeData(method);
			data.name = null;
			return data;
		}
		// Struct match
		else if (method in moduleData.structs)
		{
			auto structData = moduleData.structs[method];
			data.tokenType = TokenType.struct_;
			data.typeData = TypeData(method);
			data.name = null;
			return data;
		}
		// Enum match
		else if (method in moduleData.enums)
		{
			auto enumData = moduleData.enums[method];
			data.name = method;
			data.tokenType = TokenType.enum_;
			data.typeData = enumData.type;
			return data;
		}
	}

	// Token is an attribute
	if (token.type ==  tok!"identifier")
	{
		if (token.text == "property")
		{
			data.tokenType = TokenType.property_attribute;
			data.typeData = TypeData(token.text);
			data.name = null;
			return data;
		}
	}

	string fail = "!!! getTokenData() failed on token: type:%s, text:%s, line:%s, column:%s".format(
		token.type.str, token.text, data.line, data.column);
	stderr.writeln(fail);
	return TokenData.init;
}

string tokenStr(const Token token)
{
	return "Token(type:%s, text:%s, line:%s, column:%s)".format(token.type.str, token.text, token.line, token.column);
}

Token combineTokens(const Token a, const Token b)
{
	Token token;

	if (a is Token.init)
		return b;
	else if (b is Token.init)
		return a;

	// This pointer
	if (a.type == tok!"this")
	{
		token.type = a.type;
		token.text = b.text;
		token.line = a.line;
		token.column = a.column;
	}
	// class/struct member
	else if (a.type == tok!"identifier")
	{
		token.type = a.type;
		token.text = a.text ~ "." ~ b.text;
		token.line = a.line;
		token.column = a.column;
	}
	else
	{
		stderr.writefln("!!! combineTokens() failed on tokens: '%s:%s', '%s:%s'",
		a.type.str, a.text, b.type.str, b.text);
		return Token.init;
	}

	return token;
}

