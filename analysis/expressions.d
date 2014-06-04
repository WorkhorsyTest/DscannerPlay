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
import analysis.tokens;
import dlang_helper;


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

