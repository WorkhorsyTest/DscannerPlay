// Copyright (c) 2014, Matthew Brennan Jones <matthew.brennan.jones@gmail.com>
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)


module std.d.inspect;

import std.d.ast;
import std.d.lexer;

import analysis.helpers;
import dlang_helper;

import std.algorithm;
import std.array;
import std.string;
import std.stdio;


void extraInspect(T)(T thing, uint indent, string name)
{
	if (indent == 0)
	{
		writeln("!!! inspect extra:");
	}

	static if (is(T == const Token))
	{
		TokenData data = getTokenData(thing);
		if (data != TokenData.init)
		{
			writefln("%s%s : %s", pad(indent++), name, "Token");
			//writefln("%stext: \"%s\"", pad(indent), thing.text);
			//writefln("%stype: \"%s\"", pad(indent), thing.type.str);
			writefln("%svalue: typeData:%s, tokenType:%s, line:%s, column:%s", pad(indent), data.typeData, data.tokenType, data.line, data.column);
		}
	}
	else static if (is(T == const IdType))
	{
		if (thing != IdType.init)
		{
			writefln("%s%s: %s", pad(indent), name, "IdType");
			writefln("%str: \"%s\"", pad(indent), thing.str);
		}
	}
	else
	{
		stderr.writefln("%s!!! extraInspect(T) override needed for: %s", pad(indent), thing);
	}
}

void inspect(const ASTNode node, size_t indent, string name)
{
	if (indent == 0)
	{
		if (node)
			writefln("!!! inspect: %s", typeid(node));
		else
			writeln("!!! inspect: null");
	}

	if (auto addExp = cast(const AddExpression) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(addExp));
		addExp.left.inspect(indent, "left");
		addExp.right.inspect(indent, "right");
		addExp.operator.extraInspect(indent, "operator");
	}
	else if (auto attr = cast(const Attribute) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(attr));
		attr.linkageAttribute.inspect(indent, "linkageAttribute");
		attr.alignAttribute.inspect(indent, "alignAttribute");
		attr.pragmaExpression.inspect(indent, "pragmaExpression");
		attr.storageClass.inspect(indent, "storageClass");
		attr.attribute.extraInspect(indent, "attribute");
	}
	else if (auto andAndExp = cast(const AndAndExpression) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(andAndExp));
		andAndExp.left.inspect(indent, "left");
		andAndExp.right.inspect(indent, "right");
	}
	else if (auto andExp = cast(const AndExpression) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(andExp));
		andExp.left.inspect(indent, "left");
		andExp.right.inspect(indent, "right");
	}
	else if (auto assertExp = cast(const AssertExpression) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(assertExp));
		assertExp.assertion.inspect(indent, "assertion");
		assertExp.message.inspect(indent, "message");
	}
	else if (auto argList = cast(const ArgumentList) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(argList));
		foreach (item; argList.items)
		{
			item.inspect(indent, "items[]");
		}
	}
	else if (auto argu = cast(const Arguments) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(argu));
		argu.argumentList.inspect(indent, "argumentList");
	}
	else if (auto arrayInit = cast(const ArrayInitializer) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(arrayInit));
		foreach (a; arrayInit.arrayMemberInitializations)
		{
			a.inspect(indent, "arrayMemberInitializations[]");
		}
	}
	else if (auto assExp = cast(const AssignExpression) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(assExp));
		assExp.ternaryExpression.inspect(indent, "ternaryExpression");
		assExp.assignExpression.inspect(indent, "assignExpression");
		assExp.operator.extraInspect(indent, "operator");
	}
	else if (auto atTrr = cast(const AtAttribute) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(atTrr));
		atTrr.functionCallExpression.inspect(indent, "functionCallExpression");
		atTrr.argumentList.inspect(indent, "argumentList");
		atTrr.identifier.extraInspect(indent, "identifier");
	}
	else if (auto autoDec = cast(const AutoDeclaration) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(autoDec));
		foreach (iden; autoDec.identifiers)
		{
			iden.extraInspect(indent, "identifiers[]");
		}
		foreach (init; autoDec.initializers)
		{
			init.inspect(indent, "initializers[]");
		}
	}
	else if (auto blkSta = cast(const BlockStatement) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(blkSta));
		//blkSta.startLocation.inspect(indent, "startLocation");
		//blkSta.endLocation.inspect(indent, "endLocation");
		blkSta.declarationsAndStatements.inspect(indent, "declarationsAndStatements");
	}
	else if (auto castExp = cast(const CastExpression) node)
	{
		castExp.type.inspect(indent, "type");
		castExp.castQualifier.inspect(indent, "castQualifier");
		castExp.unaryExpression.inspect(indent, "unaryExpression");
	}
	else if (auto cmpExp = cast(const CmpExpression) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(cmpExp));
		cmpExp.shiftExpression.inspect(indent, "shiftExpression");
		cmpExp.equalExpression.inspect(indent, "equalExpression");
		cmpExp.identityExpression.inspect(indent, "identityExpression");
		cmpExp.relExpression.inspect(indent, "relExpression");
		cmpExp.inExpression.inspect(indent, "inExpression");
	}
	else if (auto constr = cast(const Constraint) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(constr));
		constr.expression.inspect(indent, "expression");
	}
	else if (auto decl = cast(const Declaration) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(decl));
		foreach (attribute; decl.attributes)
		{
			attribute.inspect(indent, "attributes[]");
		}
		decl.attributeDeclaration.inspect(indent, "attributeDeclaration");
		decl.importDeclaration.inspect(indent, "importDeclaration");
		decl.functionDeclaration.inspect(indent, "functionDeclaration");
		decl.variableDeclaration.inspect(indent, "variableDeclaration");
		decl.aliasThisDeclaration.inspect(indent, "aliasThisDeclaration");
		decl.structDeclaration.inspect(indent, "structDeclaration");
		decl.classDeclaration.inspect(indent, "classDeclaration");
		decl.interfaceDeclaration.inspect(indent, "interfaceDeclaration");
		decl.unionDeclaration.inspect(indent, "unionDeclaration");
		decl.enumDeclaration.inspect(indent, "enumDeclaration");
		decl.aliasDeclaration.inspect(indent, "aliasDeclaration");
		decl.mixinDeclaration.inspect(indent, "mixinDeclaration");
		decl.mixinTemplateDeclaration.inspect(indent, "mixinTemplateDeclaration");
		decl.unittest_.inspect(indent, "unittest_");
		decl.staticAssertDeclaration.inspect(indent, "staticAssertDeclaration");
		decl.templateDeclaration.inspect(indent, "templateDeclaration");
		decl.constructor.inspect(indent, "constructor");
		decl.destructor.inspect(indent, "destructor");
		decl.staticConstructor.inspect(indent, "staticConstructor");
		decl.staticDestructor.inspect(indent, "staticDestructor");
		decl.sharedStaticDestructor.inspect(indent, "sharedStaticDestructor");
		decl.sharedStaticConstructor.inspect(indent, "sharedStaticConstructor");
		decl.conditionalDeclaration.inspect(indent, "conditionalDeclaration");
		decl.pragmaDeclaration.inspect(indent, "pragmaDeclaration");
		decl.versionSpecification.inspect(indent, "versionSpecification");
		decl.invariant_.inspect(indent, "invariant_");
		decl.postblit.inspect(indent, "postblit");
		foreach (declaration; decl.declarations)
		{
			declaration.inspect(indent, "declarations[]");
		}
	}
	else if (auto dclrAndSta = cast(const DeclarationsAndStatements) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(dclrAndSta));
		foreach (d; dclrAndSta.declarationsAndStatements)
		{
			d.inspect(indent, "declarationsAndStatements[]");
		}
	}
	else if (auto dclrOrSta = cast(const DeclarationOrStatement) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(dclrOrSta));
		dclrOrSta.declaration.inspect(indent, "declaration");
		dclrOrSta.statement.inspect(indent, "statement");
	}
	else if (auto declar = cast(const Declarator) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(declar));
		declar.name.extraInspect(indent, "name");
		declar.initializer.inspect(indent, "initializer");
	}
	else if (auto expr = cast(const Expression) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(expr));
		foreach (i; expr.items)
		{
			i.inspect(indent, "items[]");
		}
	}
	else if (auto equalExp = cast(const EqualExpression) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(equalExp));
		equalExp.left.inspect(indent, "left");
		equalExp.right.inspect(indent, "right");
		equalExp.operator.extraInspect(indent, "operator");
	}
	else if (auto exprSta = cast(const ExpressionStatement) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(exprSta));
		exprSta.expression.inspect(indent, "expression");
	}
	else if (auto forEach = cast(const ForeachStatement) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(forEach));
		//forEach.type.inspect(indent, "type");
		forEach.foreachTypeList.inspect(indent, "foreachTypeList");
		forEach.foreachType.inspect(indent, "foreachType");
		forEach.low.inspect(indent, "low");
		forEach.high.inspect(indent, "high");
		forEach.declarationOrStatement.inspect(indent, "declarationOrStatement");
		//forEach.startIndex.inspect(indent, "startIndex");
	}
	else if (auto forEachType = cast(const ForeachType) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(forEachType));
		//forEachType.typeConstructors.inspect(indent, "typeConstructors");
		forEachType.type.inspect(indent, "type");
		forEachType.identifier.extraInspect(indent, "identifier");
	}
	else if (auto forEachTypeList = cast(const ForeachTypeList) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(forEachTypeList));
		foreach (item; forEachTypeList.items)
		{
			item.inspect(indent, "items[]");
		}
	}
	else if (auto funcBody = cast(const FunctionBody) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(funcBody));
		funcBody.blockStatement.inspect(indent, "blockStatement");
		funcBody.bodyStatement.inspect(indent, "bodyStatement");
		funcBody.outStatement.inspect(indent, "outStatement");
		funcBody.inStatement.inspect(indent, "inStatement");
	}
	else if (auto fncExp = cast(const FunctionCallExpression) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(fncExp));
		fncExp.unaryExpression.inspect(indent, "unaryExpression");
		fncExp.templateArguments.inspect(indent, "templateArguments");
		fncExp.arguments.inspect(indent, "arguments");
	}
	else if (auto funcDec = cast(const FunctionDeclaration) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(funcDec));
		writefln("%shasAuto: %s", pad(indent), funcDec.hasAuto);
		writefln("%shasRef: %s", pad(indent), funcDec.hasRef);
		funcDec.returnType.inspect(indent, "returnType");
		funcDec.name.extraInspect(indent, "name");
		funcDec.templateParameters.inspect(indent, "templateParameters");
		funcDec.parameters.inspect(indent, "parameters");
		funcDec.constraint.inspect(indent, "constraint");
		funcDec.functionBody.inspect(indent, "functionBody");
		//funcDec.memberFunctionAttributes.inspect(indent, "memberFunctionAttributes");
		//funcDec.comment.inspect(indent, "comment");
	}
	else if (auto idenChain = cast(const IdentifierChain) node)
	{
		foreach (iden; idenChain.identifiers)
		{
			iden.extraInspect(indent, "identifiers[]");
		}
	}
	else if (auto idenOrTempChain = cast(const IdentifierOrTemplateChain) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(idenOrTempChain));
		foreach (i; idenOrTempChain.identifiersOrTemplateInstances)
		{
			i.inspect(indent, "identifiersOrTemplateInstances[]");
		}
	}
	else if (auto idenTemp = cast(const IdentifierOrTemplateInstance) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(idenTemp));
		idenTemp.templateInstance.inspect(indent, "templateInstance");
		idenTemp.identifier.extraInspect(indent, "identifier");
	}
	else if (auto idenExp = cast(const IdentityExpression) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(idenExp));
		idenExp.left.inspect(indent, "left");
		idenExp.right.inspect(indent, "right");
	}
	else if (auto indexExp = cast(const IndexExpression) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(indexExp));
		indexExp.unaryExpression.inspect(indent, "unaryExpression");
		indexExp.argumentList.inspect(indent, "argumentList");
	}
	else if (auto ifStatement = cast(const IfStatement) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(ifStatement));
		ifStatement.identifier.extraInspect(indent, "identifier");
		ifStatement.type.inspect(indent, "type");
		ifStatement.expression.inspect(indent, "expression");
		ifStatement.thenStatement.inspect(indent, "thenStatement");
		ifStatement.elseStatement.inspect(indent, "elseStatement");
		writefln("%sstartIndex: %s", pad(indent), ifStatement.startIndex);
	}
	else if (auto importDec = cast(const ImportDeclaration) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(importDec));
		foreach (singleImport; importDec.singleImports)
		{
			singleImport.inspect(indent, "singleImports[]");
		}
		importDec.importBindings.inspect(indent, "importBindings[]");
	}
	else if (auto init = cast(const Initializer) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(init));
		init.nonVoidInitializer.inspect(indent, "nonVoidInitializer");
	}
	else if (auto mod = cast(const Module) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(mod));
		mod.moduleDeclaration.inspect(indent, "moduleDeclaration");
		foreach (dec; mod.declarations)
		{
			dec.inspect(indent, "Declaration[]");
		}
	}
	else if (auto modDec = cast(const ModuleDeclaration) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(modDec));
		modDec.moduleName.inspect(indent, "moduleName");
	}
	else if (auto mulExp = cast(const MulExpression) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(mulExp));
		mulExp.left.inspect(indent, "left");
		mulExp.right.inspect(indent, "right");
		mulExp.operator.extraInspect(indent, "operator");
	}
	else if (auto newExp = cast(const NewExpression) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(newExp));
		newExp.type.inspect(indent, "type");
		newExp.newAnonClassExpression.inspect(indent, "newAnonClassExpression");
		newExp.arguments.inspect(indent, "arguments");
		newExp.assignExpression.inspect(indent, "assignExpression");
	}
	else if (auto nonVoidInit = cast(const NonVoidInitializer) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(nonVoidInit));
		nonVoidInit.assignExpression.inspect(indent, "assignExpression");
		nonVoidInit.arrayInitializer.inspect(indent, "arrayInitializer");
		nonVoidInit.structInitializer.inspect(indent, "structInitializer");
	}
	else if (auto orOrExp = cast(const OrOrExpression) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(orOrExp));
		orOrExp.left.inspect(indent, "left");
		orOrExp.right.inspect(indent, "right");
	}
	else if (auto orExp = cast(const OrExpression) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(orExp));
		orExp.left.inspect(indent, "left");
		orExp.right.inspect(indent, "right");
	}
	else if (auto param = cast(const Parameter) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(param));
		foreach (parameterAttribute; param.parameterAttributes)
			parameterAttribute.extraInspect(indent, "parameterAttributes[]");
		param.type.inspect(indent, "type");
		param.name.extraInspect(indent, "name");
		param.vararg.extraInspect(indent, "vararg");
		param.default_.inspect(indent, "default_");
	}
	else if (auto params = cast(const Parameters) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(params));
		foreach (parameter; params.parameters)
		{
			parameter.inspect(indent, "parameters[]");
		}
		//params.hasVarargs.inspect(indent, "hasVarargs");
	}
	else if (auto powExp = cast(const PowExpression) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(powExp));
		powExp.left.inspect(indent, "left");
		powExp.right.inspect(indent, "right");
	}
	else if (auto priExp = cast(const PrimaryExpression) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(priExp));
		priExp.dot.extraInspect(indent, "dot");
		priExp.primary.extraInspect(indent, "primary");
		priExp.identifierOrTemplateInstance.inspect(indent, "identifierOrTemplateInstance");
		priExp.basicType.extraInspect(indent, "basicType");
		priExp.typeofExpression.inspect(indent, "typeofExpression");
		priExp.typeidExpression.inspect(indent, "typeidExpression");
		priExp.arrayLiteral.inspect(indent, "arrayLiteral");
		priExp.assocArrayLiteral.inspect(indent, "assocArrayLiteral");
		priExp.expression.inspect(indent, "expression");
		priExp.isExpression.inspect(indent, "isExpression");
		priExp.lambdaExpression.inspect(indent, "lambdaExpression");
		priExp.functionLiteralExpression.inspect(indent, "functionLiteralExpression");
		priExp.traitsExpression.inspect(indent, "traitsExpression");
		priExp.mixinExpression.inspect(indent, "mixinExpression");
		priExp.importExpression.inspect(indent, "importExpression");
		priExp.vector.inspect(indent, "vector");
	}
	else if (auto relExp = cast(const RelExpression) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(relExp));
		relExp.left.inspect(indent, "left");
		relExp.right.inspect(indent, "right");
		relExp.operator.extraInspect(indent, "operator");
	}
	else if (auto ret = cast(const ReturnStatement) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(ret));
		ret.expression.inspect(indent, "expression");
	}
	else if (auto shiftExp = cast(const ShiftExpression) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(shiftExp));
		shiftExp.left.inspect(indent, "left");
		shiftExp.right.inspect(indent, "right");
		shiftExp.operator.extraInspect(indent, "operator");
	}
	else if (auto singImpo = cast(const SingleImport) node)
	{
		singImpo.rename.extraInspect(indent, "rename");
		singImpo.identifierChain.inspect(indent, "identifierChain");
	}
	else if (auto stat = cast(const Statement) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(stat));
		stat.statementNoCaseNoDefault.inspect(indent, "statementNoCaseNoDefault");
		stat.caseStatement.inspect(indent, "caseStatement");
		stat.caseRangeStatement.inspect(indent, "caseRangeStatement");
		stat.defaultStatement.inspect(indent, "defaultStatement");
	}
	else if (auto stateNoCaseNoDef = cast(const StatementNoCaseNoDefault) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(stateNoCaseNoDef));
		stateNoCaseNoDef.labeledStatement.inspect(indent, "labeledStatement");
		stateNoCaseNoDef.blockStatement.inspect(indent, "blockStatement");
		stateNoCaseNoDef.ifStatement.inspect(indent, "ifStatement");
		stateNoCaseNoDef.whileStatement.inspect(indent, "whileStatement");
		stateNoCaseNoDef.doStatement.inspect(indent, "doStatement");
		stateNoCaseNoDef.forStatement.inspect(indent, "forStatement");
		stateNoCaseNoDef.foreachStatement.inspect(indent, "foreachStatement");
		stateNoCaseNoDef.switchStatement.inspect(indent, "switchStatement");
		stateNoCaseNoDef.finalSwitchStatement.inspect(indent, "finalSwitchStatement");
		stateNoCaseNoDef.continueStatement.inspect(indent, "continueStatement");
		stateNoCaseNoDef.breakStatement.inspect(indent, "breakStatement");
		stateNoCaseNoDef.returnStatement.inspect(indent, "returnStatement");
		stateNoCaseNoDef.gotoStatement.inspect(indent, "gotoStatement");
		stateNoCaseNoDef.withStatement.inspect(indent, "withStatement");
		stateNoCaseNoDef.synchronizedStatement.inspect(indent, "synchronizedStatement");
		stateNoCaseNoDef.tryStatement.inspect(indent, "tryStatement");
		stateNoCaseNoDef.throwStatement.inspect(indent, "throwStatement");
		stateNoCaseNoDef.scopeGuardStatement.inspect(indent, "scopeGuardStatement");
		stateNoCaseNoDef.asmStatement.inspect(indent, "asmStatement");
		stateNoCaseNoDef.conditionalStatement.inspect(indent, "conditionalStatement");
		stateNoCaseNoDef.staticAssertStatement.inspect(indent, "staticAssertStatement");
		stateNoCaseNoDef.versionSpecification.inspect(indent, "versionSpecification");
		stateNoCaseNoDef.debugSpecification.inspect(indent, "debugSpecification");
		stateNoCaseNoDef.functionCallStatement.inspect(indent, "functionCallStatement");
		stateNoCaseNoDef.expressionStatement.inspect(indent, "expressionStatement");
	}
	else if (auto storageClass = cast(const StorageClass) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(storageClass));
		storageClass.atAttribute.inspect(indent, "atAttribute");
		storageClass.deprecated_.inspect(indent, "deprecated_");
		storageClass.token.extraInspect(indent, "token");
	}
	else if (auto structBod = cast(const StructBody) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(structBod));
		foreach (dec; structBod.declarations)
		{
			dec.inspect(indent, "declarations[]");
		}
	}
	else if (auto structDec = cast(const StructDeclaration) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(structDec));
		//structDec.name.inspect(indent, "name");
		structDec.templateParameters.inspect(indent, "templateParameters");
		structDec.constraint.inspect(indent, "constraint");
		structDec.structBody.inspect(indent, "structBody");
		//structDec.comment.inspect(indent, "comment");
	}
	else if (auto symbol = cast(const Symbol) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(symbol));
		symbol.identifierOrTemplateChain.inspect(indent, "identifierOrTemplateChain");
		writefln("%sdot: %s", pad(indent), symbol.dot);
	}
	else if (auto tempArg = cast(const TemplateArgument) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(tempArg));
		tempArg.type.inspect(indent, "type");
		tempArg.assignExpression.inspect(indent, "assignExpression");
	}
	else if (auto tempArgList = cast(const TemplateArgumentList) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(tempArgList));
		foreach (tempArg; tempArgList.items)
		{
			tempArg.inspect(indent, "items[]");
		}
	}
	else if (auto tempArgs = cast(const TemplateArguments) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(tempArgs));
		tempArgs.templateArgumentList.inspect(indent, "templateArgumentList");
		tempArgs.templateSingleArgument.inspect(indent, "templateSingleArgument");
	}
	else if (auto tempInst = cast(const TemplateInstance) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(tempInst));
		tempInst.identifier.extraInspect(indent, "identifier");
		tempInst.templateArguments.inspect(indent, "templateArguments");
	}
	else if (auto tempParamList = cast(const TemplateParameterList) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(tempParamList));
		foreach (item; tempParamList.items)
		{
			item.inspect(indent, "items[]");
		}
	}
	else if (auto tempParams = cast(const TemplateParameters) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(tempParams));
		tempParams.templateParameterList.inspect(indent, "templateParameterList");
	}
	else if (auto tempParam = cast(const TemplateParameter) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(tempParam));
		tempParam.templateTypeParameter.inspect(indent, "templateTypeParameter");
		tempParam.templateValueParameter.inspect(indent, "templateValueParameter");
		tempParam.templateAliasParameter.inspect(indent, "templateAliasParameter");
		tempParam.templateTupleParameter.inspect(indent, "templateTupleParameter");
		tempParam.templateThisParameter.inspect(indent, "templateThisParameter");
	}
	else if (auto tempSingArg = cast(const TemplateSingleArgument) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(tempSingArg));
		tempSingArg.token.extraInspect(indent, "tempSingArg");
	}
	else if (auto tempTypeParam = cast(const TemplateTypeParameter) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(tempTypeParam));
		tempTypeParam.identifier.extraInspect(indent, "identifier");
		tempTypeParam.colonType.inspect(indent, "colonType");
		tempTypeParam.assignType.inspect(indent, "assignType");
	}
	else if (auto terExp = cast(const TernaryExpression) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(terExp));
		terExp.orOrExpression.inspect(indent, "orOrExpression");
		terExp.expression.inspect(indent, "expression");
		terExp.ternaryExpression.inspect(indent, "ternaryExpression");
	}
	else if (auto astType = cast(const Type) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(astType));
		foreach (c; astType.typeConstructors)
		{
			//c.inspect(indent, "typeConstructors[]");
		}
		foreach (s; astType.typeSuffixes)
		{
			s.inspect(indent, "typeSuffixes[]");
		}
		astType.type2.inspect(indent, "type2");
	}
	else if (auto typeIdExpr = cast(const TypeidExpression) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(typeIdExpr));
		typeIdExpr.type.inspect(indent, "type");
		typeIdExpr.expression.inspect(indent, "expression");
	}
	else if (auto astType2 = cast(const Type2) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(astType2));
		astType2.builtinType.extraInspect(indent, "builtinType");
		astType2.symbol.inspect(indent, "symbol");
		astType2.typeofExpression.inspect(indent, "typeofExpression");
		astType2.identifierOrTemplateChain.inspect(indent, "identifierOrTemplateChain");
		//astType2.typeConstructor.inspect(indent, "typeConstructor");
		astType2.type.inspect(indent, "type");
	}
	else if (auto typeSuf = cast(const TypeSuffix) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(typeSuf));
		//typeSuf.delegateOrFunction.inspect(indent, "delegateOrFunction");
		//typeSuf.star.inspect(indent, "star");
		//typeSuf.array.inspect(indent, "array");
		typeSuf.type.inspect(indent, "type");
		typeSuf.low.inspect(indent, "low");
		typeSuf.high.inspect(indent, "high");
		typeSuf.parameters.inspect(indent, "parameters");
		foreach (memFunAttr; typeSuf.memberFunctionAttributes)
		{
			memFunAttr.inspect(indent, "memberFunctionAttributes[]");
		}
	}
	else if (auto unrExp = cast(const UnaryExpression) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(unrExp));
		unrExp.type.inspect(indent, "type");
		unrExp.primaryExpression.inspect(indent, "primaryExpression");
		unrExp.unaryExpression.inspect(indent, "unaryExpression");
		unrExp.newExpression.inspect(indent, "newExpression");
		unrExp.deleteExpression.inspect(indent, "deleteExpression");
		unrExp.castExpression.inspect(indent, "castExpression");
		unrExp.functionCallExpression.inspect(indent, "functionCallExpression");
		unrExp.argumentList.inspect(indent, "argumentList");
		unrExp.identifierOrTemplateInstance.inspect(indent, "identifierOrTemplateInstance");
		unrExp.assertExpression.inspect(indent, "assertExpression");
		unrExp.sliceExpression.inspect(indent, "sliceExpression");
		unrExp.indexExpression.inspect(indent, "indexExpression");
	}
	else if (auto varDec = cast(const VariableDeclaration) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(varDec));
		varDec.type.inspect(indent, "type");
		foreach (declarator; varDec.declarators)
		{
			declarator.inspect(indent, "declarators[]");
		}
		varDec.storageClass.inspect(indent, "storageClass");
		varDec.autoDeclaration.inspect(indent, "autoDeclaration");
		writefln("%scomment: %s", pad(indent), varDec.comment);
	}
	else if (auto xOrExp = cast(const XorExpression) node)
	{
		writefln("%s%s : %s", pad(indent++), name, typeid(xOrExp));
		xOrExp.left.inspect(indent, "left");
		xOrExp.right.inspect(indent, "right");
	}
	else if (node)
	{
		stderr.writefln("%s!!! inspect() override needed for: %s", pad(indent), typeid(node));
	}
}

