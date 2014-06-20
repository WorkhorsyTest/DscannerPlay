// Copyright (c) 2014, Matthew Brennan Jones <matthew.brennan.jones@gmail.com>
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)


module analysis.ast_helpers;

import std.stdio;
import std.array;
import std.string;
import std.stdint;
import std.conv;

import std.d.ast;
import std.d.lexer;
import analysis.run;
import analysis.scope_frame;
import analysis.tokens;
import analysis.expressions;
import dlang_helper;


string getAttributeName(const Attribute attribute, ref size_t line, ref size_t column)
{
	// Get the name from the attribute identifier
	if (attribute
		&& attribute.storageClass
		&& attribute.storageClass.atAttribute
		&& attribute.storageClass.atAttribute.identifier !is Token.init
		&& attribute.storageClass.atAttribute.identifier.text
		&& attribute.storageClass.atAttribute.identifier.text.length)
	{
		auto token = attribute.storageClass.atAttribute.identifier;
		line = token.line;
		column = token.column;
		return token.text;
	}

	// Get the attribute from the storage class token
	if (attribute
		&& attribute.storageClass
		&& attribute.storageClass.token !is Token.init)
	{
		auto token = attribute.storageClass.token;
		line = token.line;
		column = token.column;
		return token.type.str;
	}

	return null;
}

string getAttributeName(const MemberFunctionAttribute memberFunctionAttribute, ref size_t line, ref size_t column)
{
	// Get the name from the tokenType
	if (memberFunctionAttribute
		&& memberFunctionAttribute.tokenType !is IdType.init
		&& memberFunctionAttribute.tokenType.str
		&& memberFunctionAttribute.tokenType.str.length)
	{
		// FIXME: How do we get the line/column number?
		return memberFunctionAttribute.tokenType.str;
	}

	// Get the name from the attribute identifier
	if (memberFunctionAttribute
		&& memberFunctionAttribute.atAttribute
		&& memberFunctionAttribute.atAttribute.identifier !is Token.init
		&& memberFunctionAttribute.atAttribute.identifier.type == tok!"identifier"
		&& memberFunctionAttribute.atAttribute.identifier.text
		&& memberFunctionAttribute.atAttribute.identifier.text.length)
	{
		auto iden = memberFunctionAttribute.atAttribute.identifier;
		line = iden.line;
		column = iden.column;
		return iden.text;
	}

	return null;
}

Decoration getDeclarationDecorations(Scope scope_, const Declaration decl)
{
	Decoration decoration;

	void setDecoration(T)(T attribute)
	{
		// Get the attribute
		size_t line, column;
		string name = getAttributeName(attribute, line, column);

		// Skip if invalid
		if (!name || line==0 || column==0)
			return;

		switch (name)
		{
			case "ref":
				decoration.isRef = true;
				break;
			case "auto":
				decoration.isAuto = true;
				break;
			case "property":
				decoration.isProperty = true;
				break;
			case "pure":
				decoration.isPure = true;
				break;
			case "safe":
				decoration.isSafe = true;
				break;
			case "trusted":
				decoration.isTrusted = true;
				break;
			case "system":
				decoration.isSystem = true;
				break;
			case "nothrow":
				decoration.isNoThrow = true;
				break;
			default:
				break;
		}
	}

	// Normal attributes
	foreach (attribute; decl.attributes)
	{
		setDecoration(attribute);
	}

	// Just return if missing function attribute nodes
	if (!decl.functionDeclaration
		|| !decl.functionDeclaration.memberFunctionAttributes)
		return decoration;


	// Function attributes
	foreach (memberFunctionAttribute; decl.functionDeclaration.memberFunctionAttributes)
	{
		setDecoration(memberFunctionAttribute);
	}

	return decoration;
}

TemplateData[] getTemplateDatas(Scope scope_, const TemplateParameters templateParameters)
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

StructData getStructData(Scope scope_, const StructDeclaration structDec)
{
	StructData data;
	data.name = structDec.name.text;
	data.line = structDec.name.line;
	data.column = structDec.name.column;

	foreach (decl; structDec.structBody.declarations)
	{
		if (decl.variableDeclaration)
		{
			foreach (varData; getVariableDatas(scope_, decl.variableDeclaration))
			{
				data.fields[varData.name] = varData;
			}
		}
		else if (decl.functionDeclaration)
		{
			FunctionData funcData = getFunctionData(scope_, decl.functionDeclaration);
			data.methods[funcData.name] = funcData;
		}
	}

	return data;
}

ClassData getClassData(Scope scope_, const ClassDeclaration classDec)
{
	ClassData data;
	data.name = classDec.name.text;
	data.line = classDec.name.line;
	data.column = classDec.name.column;

	foreach (decl; classDec.structBody.declarations)
	{
		if (decl.variableDeclaration)
		{
			foreach (varData; getVariableDatas(scope_, decl.variableDeclaration))
			{
				data.fields[varData.name] = varData;
			}
		}
		else if (decl.functionDeclaration)
		{
			FunctionData funcData = getFunctionData(scope_, decl.functionDeclaration);
			data.methods[funcData.name] = funcData;
		}
	}
	return data;
}

EnumData getEnumData(Scope scope_, const EnumDeclaration enumDec)
{
	EnumData data;
	data.name = enumDec.name.text;
	data.line = enumDec.name.line;
	data.column = enumDec.name.column;
	data.type = TypeData("int");
	data.type = getTypeData(enumDec.type);

	foreach (member; enumDec.enumBody.enumMembers)
	{
		string name;
		auto fieldData = getEnumFieldData(scope_, member, name);
		data.fields[name] = fieldData;
	}

	return data;
}

FieldData getEnumFieldData(Scope scope_, const EnumMember enumMember, ref string name)
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

ModuleData getModuleData(Scope scope_, const Module mod)
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
		Decoration decoration = getDeclarationDecorations(scope_, decl);
		scope_.decorations.push(decoration);

		if (decl.functionDeclaration)
		{
			FunctionData funcData = getFunctionData(scope_, decl.functionDeclaration);
			data.functions[funcData.name] = funcData;
		}
		else if (decl.variableDeclaration)
		{
			foreach (varData; getVariableDatas(scope_, decl.variableDeclaration))
			{
				data.variables[varData.name] = varData;
			}
		}
		else if (decl.structDeclaration)
		{
			StructData structData = getStructData(scope_, decl.structDeclaration);
			data.structs[structData.name] = structData;
		}
		else if (decl.classDeclaration)
		{
			ClassData classData = getClassData(scope_, decl.classDeclaration);
			data.classes[classData.name] = classData;
		}
		else if (decl.enumDeclaration)
		{
			EnumData enumData = getEnumData(scope_, decl.enumDeclaration);
			data.enums[enumData.name] = enumData;
		}

		// Remove decorations
		scope_.decorations.pop();
	}

	return data;
}

FunctionData getFunctionData(Scope scope_, const FunctionDeclaration funcDec)
{
	FunctionData data;
	data.name = funcDec.name.text;
	data.templates = getTemplateDatas(scope_, funcDec.templateParameters);
	data.returnType = getFunctionReturnTypeData(scope_, funcDec);
	data.argTypes = getFunctionArgTypeDatas(scope_, funcDec);
	data.isPure = scope_.decorations.peak.isPure;
	data.line = funcDec.name.line;
	data.column = funcDec.name.column;
	return data;
}

TokenData[] getFunctionCallArguments(Scope scope_, const FunctionCallExpression funcCallExp)
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
		args ~= getExpressionReturnTokenData(scope_, item);
	}

	return args;
}

string getFunctionCallName(Scope scope_, const FunctionCallExpression funcExp)
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

TypeData getFunctionReturnTypeData(Scope scope_, const FunctionDeclaration funcDec)
{
	// Normal return type
	if (funcDec.returnType)
	{
		return getTypeData(funcDec.returnType);
	}

	// Auto return type
	auto decoration = scope_.decorations.peak();
	if (decoration !is Decoration.init && decoration.isAuto)
	{
		return TypeData("auto");
	}

	return TypeData.init;
}

TypeData[] getFunctionArgTypeDatas(Scope scope_, const FunctionDeclaration funcDec)
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

string[] getFunctionArgNames(Scope scope_, const FunctionDeclaration funcDec)
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

VariableData[] getVariableDatas(Scope scope_, const VariableDeclaration varDec)
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

		TokenData tokenData = getExpressionReturnTokenData(scope_, varDec.autoDeclaration);

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



