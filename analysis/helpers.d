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
import analysis.scope_frame;
import analysis.expressions;
import dlang_helper;


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


