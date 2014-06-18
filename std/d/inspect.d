// Copyright (c) 2014, Matthew Brennan Jones <matthew.brennan.jones@gmail.com>
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)


module std.d.inspect;

import std.d.ast;
import std.d.lexer;

import analysis.ast_helpers;
import dlang_helper;

import std.algorithm;
import std.array;
import std.string;
import std.stdio;

template generateInspect(types ...)
{
	private string generator()
	{
		string[] result;

		foreach (n; types)
		{
			result ~= " if (auto node = cast(const " ~ __traits(identifier, n) ~ ") astNode) {";
			result ~= "    output.writefln(\"%s%s : %s\", pad(indent++), name, typeid(astNode));";
			foreach (member; __traits(allMembers, n))
			{
				// FIXME: The way we are hard coding the names of methods to black list
				// here is stupid. There has to be a way to just ignore all methods.
				static if (!is(typeof(member) == function)
					&& !is(typeof(member) == delegate)
					&& !is(member == function)
					&& !is(member == delegate)
					&& !is(member == class)
					&& !is(member == struct)
					&& !is(member == enum)
					&& !is(member == interface)
					&& !is(member == union)
					&& member.stringof != "\"accept\""
					&& member.stringof != "\"opEquals\""
					&& member.stringof != "\"toString\""
					&& member.stringof != "\"toHash\""
					&& member.stringof != "\"opCmp\""
					&& member.stringof != "\"Monitor\""
					&& member.stringof != "\"factory\"")
				{
					result ~= "    output.inspect(node." ~ member ~ ", " ~ member.stringof ~ ", indent);";
				}
			}
			result ~= "}";
		}

		return std.string.join(result, "\n");
	}

	immutable generateInspect = generator();
}

void inspect(T)(File output, T thing, string name, size_t indent = 0)
{
	if (indent == 0)
		output.writeln("!!! inspect fallback:");

	// Array
	static if(std.traits.isArray!T)
	{
		foreach (n; thing)
			output.inspect(n, name ~ "[]", indent);
	}
	// Basic type
	else static if(std.traits.isBasicType!T || std.traits.isSomeString!T)
	{
		output.writefln("%s%s : %s", pad(indent++), name, thing);
	}
	// Token
	else static if (is(T == const Token))
	{
		if (thing !is Token.init)
		{
			output.writefln("%s%s : %s", pad(indent++), name, "Token");
			output.writefln("%s%s : %s", pad(indent), "type", thing.type.str);
			output.writefln("%s%s : %s", pad(indent), "text", thing.text);
			output.writefln("%s%s : %s", pad(indent), "line", thing.line);
			output.writefln("%s%s : %s", pad(indent), "column", thing.column);
		}
	}
	// IdType
	else static if (is(T == const IdType))
	{
		if (thing !is IdType.init)
		{
			output.writefln("%s%s : %s", pad(indent++), name, "IdType");
			output.writefln("%s%s : %s", pad(indent), "str", thing.str);
		}
	}
	// Everything else that is not null
	else static if (!is(T == typeof(null)))
	{
		stderr.writefln("%s!!! inspect(T) fallback override needed for: %s", pad(indent), typeid(thing));
	}
}

void inspect(File output, const ASTNode astNode, string name, size_t indent = 0)
{
	if (indent == 0)
	{
		if (astNode)
			output.writefln("!!! inspect: %s", typeid(astNode));
		else
			return;
	}

	// All AST nodes
	mixin(generateInspect!(
	AddExpression, AliasDeclaration, AliasInitializer, AliasThisDeclaration, AlignAttribute, AndAndExpression,
	AndExpression, ArgumentList, Arguments, ArrayInitializer, ArrayLiteral, ArrayMemberInitialization,
	AsmAddExp, AsmAndExp, AsmBrExp, AsmEqualExp, AsmExp, AsmInstruction, AsmLogAndExp, AsmLogOrExp,
	AsmMulExp, AsmOrExp, AsmPrimaryExp, AsmRelExp, AsmShiftExp, AsmStatement, AsmTypePrefix, AsmUnaExp,
	AsmXorExp, AssertExpression, AssignExpression, AssocArrayLiteral, AtAttribute, Attribute,
	AttributeDeclaration, AutoDeclaration, BlockStatement, BodyStatement, BreakStatement, BaseClass,
	BaseClassList, CaseRangeStatement, CaseStatement, CastExpression, CastQualifier, Catch, Catches,
	ClassDeclaration, CmpExpression, CompileCondition, ConditionalDeclaration, ConditionalStatement,
	Constraint, Constructor, ContinueStatement, DebugCondition, DebugSpecification, Declaration,
	DeclarationOrStatement, DeclarationsAndStatements, Declarator, DefaultStatement, DeleteExpression,
	DeleteStatement, Deprecated, Destructor, DoStatement, EnumBody, EnumDeclaration, EnumMember,
	EponymousTemplateDeclaration, EqualExpression, Expression, ExpressionStatement,
	FinalSwitchStatement, Finally, ForStatement, ForeachStatement, ForeachType, ForeachTypeList,
	FunctionAttribute, FunctionBody, FunctionCallExpression, FunctionCallStatement, FunctionDeclaration,
	FunctionLiteralExpression, GotoStatement, IdentifierChain, IdentifierList, IdentifierOrTemplateChain,
	IdentifierOrTemplateInstance, IdentityExpression, IfStatement, ImportBind, ImportBindings,
	ImportDeclaration, ImportExpression, IndexExpression, InExpression, InStatement, Initialize,
	Initializer, InterfaceDeclaration, Invariant , IsExpression, KeyValuePair, KeyValuePairs,
	LabeledStatement, LambdaExpression, LastCatch, LinkageAttribute, MemberFunctionAttribute,
	MixinDeclaration, MixinExpression, MixinTemplateDeclaration, MixinTemplateName, Module,
	ModuleDeclaration, MulExpression, NewAnonClassExpression, NewExpression, NonVoidInitializer,
	Operand, Operands, OrExpression, OrOrExpression, OutStatement, Parameter, Parameters, Postblit,
	PostIncDecExpression, PowExpression, PragmaDeclaration, PragmaExpression, PreIncDecExpression,
	PrimaryExpression, Register, RelExpression, ReturnStatement, ScopeGuardStatement,
	SharedStaticConstructor, SharedStaticDestructor, ShiftExpression, SingleImport,
	SliceExpression, Statement, StatementNoCaseNoDefault, StaticAssertDeclaration, StaticAssertStatement,
	StaticConstructor, StaticDestructor, StaticIfCondition, StorageClass, StructBody,
	StructDeclaration, StructInitializer, StructMemberInitializer, StructMemberInitializers,
	SwitchStatement, Symbol, SynchronizedStatement, TemplateAliasParameter, TemplateArgument,
	TemplateArgumentList, TemplateArguments, TemplateDeclaration, TemplateInstance,
	TemplateMixinExpression, TemplateParameter, TemplateParameterList, TemplateParameters,
	TemplateSingleArgument, TemplateThisParameter, TemplateTupleParameter, TemplateTypeParameter,
	TemplateValueParameter, TemplateValueParameterDefault, TernaryExpression, ThrowStatement,
	TraitsExpression, TryStatement, Type, Type2, TypeSpecialization, TypeSuffix, TypeidExpression,
	TypeofExpression, UnaryExpression, UnionDeclaration, Unittest, VariableDeclaration, Vector,
	VersionCondition, VersionSpecification, WhileStatement, WithStatement, XorExpression
	));
}

