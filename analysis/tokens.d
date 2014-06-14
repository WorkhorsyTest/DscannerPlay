// Copyright (c) 2014, Matthew Brennan Jones <matthew.brennan.jones@gmail.com>
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.tokens;

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


enum int[string] TYPE_SIZES = [
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

enum string[string] TYPE_PROMOTIONS = [
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

// FIXME: Make all literals accounted for instead of being converted to
// variable types.
enum string[ubyte] LITERAL_TYPE_TO_TYPE_MAP = [
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

immutable string[] BASIC_TYPES = [
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

immutable string[] INTEGER_TYPES = [
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

immutable string[] FLOAT_TYPES = [
	"float", "double", "real",
	"ifloat", "idouble", "ireal"
];

immutable string[] BOOL_TYPES = [
	"bool"
];

immutable string[] STRING_TYPES = [
	"string", "wstring", "dstring"
];

immutable string[] CHAR_TYPES = [
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

string tokenStr(const Token token)
{
	return "Token(type:%s, text:%s, line:%s, column:%s)".format(token.type.str, token.text, token.line, token.column);
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

// FIXME: Much of this can be replaced with some isBlah functions
const Token getPromotedToken(Scope scope_, const Token left, const Token right)
{
	string a = getTokenData(scope_, left).typeData.name;
	string b = getTokenData(scope_, right).typeData.name;

	// print an error if any type names are blank
	if (isNullOrBlank(a) || isNullOrBlank(b))
	{
		string message = "!!! getPromotedToken() failed on tokens: '%s' or '%s'.".format(
			getTokenData(scope_, left), getTokenData(scope_, right));
		stderr.writeln(message);
		return Token.init;
	}

	// print an error if any type names are unknown
	if (a !in TYPE_PROMOTIONS || a !in TYPE_SIZES
		|| b !in TYPE_PROMOTIONS || b !in TYPE_SIZES)
	{
		string message = "!!! getPromotedToken() failed with the type name: '%s' or '%s'.".format(a, b);
		stderr.writeln(message);
		return Token.init;
	}

	// Types are the same, so return the promotion of one
	if (a == b)
		return Token(tok!"identifier", TYPE_PROMOTIONS[a], left.line, left.column, left.index);

	// Types are different, so return the promotion of the larger
	if (TYPE_SIZES[a] > TYPE_SIZES[b])
		return Token(tok!"identifier", TYPE_PROMOTIONS[a], left.line, left.column, left.index);
	else
		return Token(tok!"identifier", TYPE_PROMOTIONS[b], right.line, right.column, right.index);
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
TokenData getTokenData(Scope scope_, const Token token)
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
	if (token.type == tok!"this" && token.text && token.text.length && scope_.thisPointers.peak())
	{
		string member = token.text;

		// Figure out what "this" is
		auto classData = scope_.getClass(scope_.thisPointers.peak());
		auto structData = scope_.getStruct(scope_.thisPointers.peak());

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
	if (token.type == tok!"this" && scope_.thisPointers.peak())
	{
		data.tokenType = TokenType.this_;
		data.name = "this";
		data.typeData = TypeData(scope_.thisPointers.peak());
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
	if (token.type == tok!"identifier" && scope_.thisPointers.peak())
	{
		// Token may be a field/method without the this pointer
		// Figure out what "this" should be
		auto classData = scope_.getClass(scope_.thisPointers.peak());
		auto structData = scope_.getStruct(scope_.thisPointers.peak());
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


