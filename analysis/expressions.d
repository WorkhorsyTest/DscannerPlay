// Copyright (c) 2014, Matthew Brennan Jones <matthew.brennan.jones@gmail.com>
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.expressions;

import std.stdio;
import std.array;
import std.string;
import std.stdint;
import std.conv;

import std.d.ast;
import std.d.lexer;
import analysis.run;
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
	TokenData data = getTokenData(token, gScope);
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
		if (getTokenData(tempSingArg.token, gScope) !is TokenData.init)
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

	string a = getTokenData(left, gScope).typeData.name;
	string b = getTokenData(right, gScope).typeData.name;

	// print an error if any type names are blank
	if (isNullOrBlank(a) || isNullOrBlank(b))
	{
		string message = "!!! getPromotedToken() failed on tokens: '%s' or '%s'.".format(
			getTokenData(left, gScope), getTokenData(right, gScope));
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

// FIXME: Make this work with UFC for variables and literals
TokenData getTokenData(const Token token, Scope scope_)
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
	if (token.type == tok!"this" && token.text && token.text.length && scope_.thisPointersPeak())
	{
		string member = token.text;

		// Figure out what "this" is
		auto classData = scope_.getClass(scope_.thisPointersPeak());
		auto structData = scope_.getStruct(scope_.thisPointersPeak());

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
	if (token.type == tok!"this" && scope_.thisPointersPeak())
	{
		data.tokenType = TokenType.this_;
		data.name = "this";
		data.typeData = TypeData(scope_.thisPointersPeak());
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

		auto varData = scope_.getVariable(identifier);

		// Token is a struct/class instance
		if (varData !is VariableData.init)
		{
			string typeName = varData.type.name;
			auto classData = scope_.getClass(typeName);
			auto structData = scope_.getStruct(typeName);

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
		auto tempData = scope_.getTemplate(identifier);
		if (tempData !is TemplateData.init)
		{
			data.tokenType = TokenType.template_;
			data.typeData = TypeData(identifier);
			data.name = null;
			return data;
		}

		// Token is a function name
		auto funcData = scope_.getFunction(identifier);
		if (funcData !is FunctionData.init)
		{
			data.tokenType = TokenType.function_;
			data.typeData = funcData.returnType;
			data.name = token.text;
			return data;
		}

		// Token is a class name
		auto classData = scope_.getClass(identifier);
		if (classData !is ClassData.init)
		{
			data.tokenType = TokenType.class_;
			data.typeData = TypeData(token.text);
			data.name = null;
			return data;
		}

		// Token is a struct name
		auto structData = scope_.getStruct(identifier);
		if (structData !is StructData.init)
		{
			data.tokenType = TokenType.struct_;
			data.typeData = TypeData(token.text);
			data.name = null;
			return data;
		}

		// Token is an enum
		auto enumData = scope_.getEnum(identifier);
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
	if (token.type == tok!"identifier" && scope_.thisPointersPeak())
	{
		// Token may be a field/method without the this pointer
		// Figure out what "this" should be
		auto classData = scope_.getClass(scope_.thisPointersPeak());
		auto structData = scope_.getStruct(scope_.thisPointersPeak());
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
		foreach (mod; scope_.modules)
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
		foreach (frame; std.range.retro(scope_.frames))
		{
			foreach (importName; frame.imports)
			{
				if (importName in scope_.modules)
				{
					auto candidateModule = scope_.modules[importName];

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


