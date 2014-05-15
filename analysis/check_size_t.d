// Copyright (c) 2014, Matthew Brennan Jones <matthew.brennan.jones@gmail.com>
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.check_size_t;

import std.stdio;
import std.array;
import std.string;
import std.stdint;

import std.d.ast;
import std.d.inspect;
import std.d.lexer;
import analysis.base;
import analysis.helpers;
import analysis.scope_frame;
import analysis.scope_analyzer;

/**
 * Checks for errors with using size_t:
 * size_t = long fails on 32bit but not on 64bit
 * int = size_t fails on 64bit but not on 32bit
 */
class SizeTCheck : ScopeAnalyzer {
	alias visit = ScopeAnalyzer.visit;

	this(string fileName) {
		super(fileName, false);
	}

	override void visit(const VariableDeclaration varDec) {
		check_size_t_initializer(varDec);
		varDec.accept(this);
	}

	override void visit(const AssignExpression assExp) {
		assExp.accept(this);
		check_size_t_expression(assExp);
	}

	override void visit(const FunctionCallExpression fncExp) {
		fncExp.accept(this);
		check_size_t_function_calls(fncExp);
	}

	void check_size_t_initializer(const VariableDeclaration varDec) {
		// Just return if any args are null
		if(!varDec || !varDec.type || !varDec.declarators || varDec.declarators.length==0) {
			return;
		}

		// Get the types
		auto var_dec = varDec.declarators[0];
		size_t line, column;
		TypeData to_type = get_type_data(varDec.type);
		TypeData from_type = get_expression_return_type(var_dec, line, column);
		if(line == 0 || column == 0) {
			get_variable_line_column(varDec, var_dec.name.text, line, column);
		}
/*
		string[] to_names = get_variable_names(varDec);

		stderr.writefln("??? VariableDeclaration - to_names: %s, to_type: %s, from_type: %s, line: %d, column: %d", 
			to_names, 
			to_type, 
			from_type, 
			line, 
			column
		);
*/
		check_size_t(to_type, from_type, line, column);
	}

	// checks:
	// function arguments
	// function parameters
	void check_size_t_function_calls(const FunctionCallExpression funcExp) {
		// Just return if any args are null
		if(!funcExp || 
			!funcExp.arguments || 
			!funcExp.arguments.argumentList ||
			!funcExp.arguments.argumentList.items) {
			return;
		}

		// Get the name and args of the function to call
		string name = get_function_call_name(funcExp);
		auto func_data = get_function_data_by_name(name);
		TypeData[] arg_types = func_data.arg_types;

		// Just return if the args length does not match
		if(!arg_types || arg_types.length != funcExp.arguments.argumentList.items.length)
			return;

		foreach(i, assExp; funcExp.arguments.argumentList.items) {
			// Get the expression return types
			size_t line, column;

			TypeData to_type = arg_types[i];
			TypeData from_type = get_expression_return_type(assExp, line, column);
/*
			stderr.writefln("??? FunctionCallExpression - to_type: %s, from_type: %s, line: %d, column: %d", 
				to_type, 
				from_type, 
				line, 
				column
			);
*/
			string message = "For function argument %d, ".format(i);
			check_size_t(to_type, from_type, line, column, message);
		}
	}

	/*
	FIXME: Make it work with:
	. unit tests
	. class static variables
	. templates
	*/
	// checks:
	// size_t = ulong on 32bit
	// uint = ulong on 64bit
	void check_size_t_expression(const AssignExpression assExp) {
		// Just return if any args are null
		if(!assExp || !assExp.ternaryExpression || !assExp.assignExpression) {
			return;
		}

		// Just return if the expression is wrong
		if(assExp.operator != tok!"=" && 
			assExp.operator != tok!"+=" && 
			assExp.operator != tok!"-=" && 
			assExp.operator != tok!"*=" && 
			assExp.operator != tok!"/=" && 
			assExp.operator != tok!"%=" && 
			assExp.operator != tok!"&=" && 
			assExp.operator != tok!"|=" && 
			assExp.operator != tok!"^=" && 
			assExp.operator != tok!"<<=" && 
			assExp.operator != tok!">>=" && 
			assExp.operator != tok!">>>=" && 
			assExp.operator != tok!"^^=") {

			return;
		}

		// Get the expression return types
		size_t line, column;
		TypeData to_type = get_expression_return_type(assExp.ternaryExpression, line, column);
		TypeData from_type = get_expression_return_type(assExp.assignExpression, line, column);
/*
		TokenData to_name = get_expression_return_token_data(assExp.ternaryExpression);
		TokenData from_name = get_expression_return_token_data(assExp.assignExpression);

		stderr.writefln("??? AssignExpression - to_name: %s, from_name: %s, to_type: %s, from_type: %s, op: %s, line: %d, column: %d", 
			to_name, 
			from_name, 
			to_type, 
			from_type, 
			assExp.operator.str, 
			line, 
			column
		);
*/
		check_size_t(to_type, from_type, line, column);
	}

	void check_size_t(TypeData to_type, TypeData from_type, size_t line, size_t column, string message=null) {
		//assert(line > 0, "Line needs to be greater than zero.");
		//assert(column > 0, "Column needs to be greater than zero.");

		if(!message)
			message = "";

		// size_t = long fails on 32bit
		if(to_type.toString() == "size_t") {
			switch(from_type.toString()) {
				case "long":
				case "ulong":
				case "int64_t":
				case "uint64_t":
					message ~= std.string.format("%s will overflow %s on 32bit.", from_type, to_type);
					addErrorMessage(line, column, message);
					break;
				default:
					break;
			}
		// int = size_t fails on 64bit
		} else if(from_type.toString() == "size_t") {
			switch(to_type.toString()) {
				case "int":
				case "uint":
				case "int32_t":
				case "uint32_t":
					message ~= std.string.format("%s will overflow %s on 64bit.", from_type, to_type);
					addErrorMessage(line, column, message);
					break;
				default:
					break;
			}
		}
	}
}

unittest {
	should_warn(q{
		size_t g_size_t = cast(ulong) 8; // [warn]: ulong will overflow size_t on 32bit.

		void test_size_t() {
			size_t a = 0;

			// standard types
			int z_int = 9;
			long z_long = 9;
			uint z_uint = 9;
			ulong z_ulong = 9;

			// stdint types
			int32_t z_int32 = 9;
			int64_t z_int64 = 9;
			uint32_t z_uint32 = 9;
			uint64_t z_uint64 = 9;

			// standard types implicit conversions
			a = z_int;
			a = z_long; // [warn]: long will overflow size_t on 32bit.
			a = z_uint;
			a = z_ulong; // [warn]: ulong will overflow size_t on 32bit.

			// stdint types implicit conversions
			a = z_int32;
			a = z_int64; // [warn]: int64_t will overflow size_t on 32bit.
			a = z_uint32;
			a = z_uint64; // [warn]: uint64_t will overflow size_t on 32bit.

			// standard types implicit conversions
			z_int = a; // [warn]: size_t will overflow int on 64bit.
			z_long = a;
			z_uint = a; // [warn]: size_t will overflow uint on 64bit.
			z_ulong = a;

			// stdint types implicit conversions
			z_int32 = a; // [warn]: size_t will overflow int32_t on 64bit.
			z_int64 = a;
			z_uint32 = a; // [warn]: size_t will overflow uint32_t on 64bit.
			z_uint64 = a;

			// assignment expressions
			a = z_uint64; // [warn]: uint64_t will overflow size_t on 32bit.
			a += z_uint64; // [warn]: uint64_t will overflow size_t on 32bit.
			a -= z_uint64; // [warn]: uint64_t will overflow size_t on 32bit.
			a *= z_uint64; // [warn]: uint64_t will overflow size_t on 32bit.
			a /= z_uint64; // [warn]: uint64_t will overflow size_t on 32bit.
			a %= z_uint64; // [warn]: uint64_t will overflow size_t on 32bit.
			a &= z_uint64; // [warn]: uint64_t will overflow size_t on 32bit.
			a |= z_uint64; // [warn]: uint64_t will overflow size_t on 32bit.
			a ^= z_uint64; // [warn]: uint64_t will overflow size_t on 32bit.
			a <<= z_uint64; // [warn]: uint64_t will overflow size_t on 32bit.
			a >>= z_uint64; // [warn]: uint64_t will overflow size_t on 32bit.
			a >>>= z_uint64; // [warn]: uint64_t will overflow size_t on 32bit.
			a ^^= z_uint64; // [warn]: uint64_t will overflow size_t on 32bit.

			// assignment from assignment expressions
			a = a + z_uint64; // [warn]: ulong will overflow size_t on 32bit.
			a = a - z_uint64; // [warn]: ulong will overflow size_t on 32bit.
			a = a * z_uint64; // [warn]: ulong will overflow size_t on 32bit.
			a = a / z_uint64; // [warn]: ulong will overflow size_t on 32bit.
			a = a % z_uint64; // [warn]: ulong will overflow size_t on 32bit.
			a = a & z_uint64; // [warn]: ulong will overflow size_t on 32bit.
			a = a | z_uint64; // [warn]: ulong will overflow size_t on 32bit.
			a = a ^ z_uint64; // [warn]: ulong will overflow size_t on 32bit.
			a = a << z_uint64; // [warn]: ulong will overflow size_t on 32bit.
			a = a >> z_uint64; // [warn]: ulong will overflow size_t on 32bit.
			a = a >>> z_uint64; // [warn]: ulong will overflow size_t on 32bit.
			a = a ^^ z_uint64; // [warn]: ulong will overflow size_t on 32bit.

			// Globals
			g_size_t = z_uint64; // [warn]: uint64_t will overflow size_t on 32bit.

			// Casting
			size_t blah = 0;
			//blah = cast(ulong) 7; // FIXME: [warn]: ulong will overflow size_t on 32bit.

			// Initialization
			size_t herp1 = z_ulong;  // [warn]: ulong will overflow size_t on 32bit.
			size_t derp1 = cast(ulong) 8; // [warn]: ulong will overflow size_t on 32bit.
			uint herp2 = a;  // [warn]: size_t will overflow uint on 64bit.
			uint derp2 = cast(size_t) 8; // [warn]: size_t will overflow uint on 64bit.

			// Auto
			auto auto_size_t = a;
			auto auto_int = 9;
			auto_size_t = z_ulong;  // [warn]: ulong will overflow size_t on 32bit.
			auto_int = a;  // [warn]: size_t will overflow int on 64bit.

			// Function result
			ulong get_bleh() {
				return 5;
			}
			size_t bleh = get_bleh(); // [warn]: ulong will overflow size_t on 32bit.
			bleh = get_bleh(); // [warn]: ulong will overflow size_t on 32bit.

			// Function result with auto
			auto get_auto_bleh() {
				ulong ret = 9;
				return ret;
			}
			bleh = get_auto_bleh(); // FIXME: [warn]: ulong will overflow size_t on 32bit.

			// Function argument
			void func_arg_test(size_t arg1_size_t, uint arg2_uint) {
				assert(arg1_size_t);
				assert(arg2_uint);
			}
			func_arg_test(
				z_ulong, // [warn]: For function argument 0, ulong will overflow size_t on 32bit.
				herp1 // [warn]: For function argument 1, size_t will overflow uint on 64bit.
			);

			// Function parameters
			void func_param_test(uint param_uint, size_t param_size_t) {
				param_uint = a; // [warn]: size_t will overflow uint on 64bit.
				param_size_t = z_long; // [warn]: long will overflow size_t on 32bit.
			}
version(none) {
			// Calling library functions
			uint a_count = std.string.countchars("abaab", "a"); // FIXME: boom on 32bit
			uint padding = 4;
			string left_padded = std.string.leftJustify("Hello", padding); // FIXME: boom on 64bit
			string left_padded = std.string.leftJustify!(string)("Hello", padding); // FIXME: boom on 64bit
}
			// Calling template functions
			//bleh = to!ulong(27); // boom on 32bit

			// Class members
			class Dog {
				size_t weight;
				size_t height;

				void blah() {
					int w = this.weight; // [warn]: size_t will overflow int on 64bit.
					int h = this.height; // [warn]: size_t will overflow int on 64bit.

					w = weight; // [warn]: size_t will overflow int on 64bit.
					h = height; // [warn]: size_t will overflow int on 64bit.
				}
			}

			// Class methods
			class Cat {
				size_t get_weight() {
					return 4;
				}
			}
			auto cat = new Cat();
			int cat_weight = cat.get_weight(); // [warn]: size_t will overflow int on 64bit.

			// Struct members
			struct Puma {
				size_t weight;
				size_t height;

				void blah() {
					int w = this.weight; // [warn]: size_t will overflow int on 64bit.
					int h = this.height; // [warn]: size_t will overflow int on 64bit.

					w = weight; // [warn]: size_t will overflow int on 64bit.
					h = height; // [warn]: size_t will overflow int on 64bit.
				}
			}

			// Struct methods
			struct Platypus {
				size_t get_weight() {
					return 8;
				}
			}
			auto platypus = Platypus();
			int platypus_weight = platypus.get_weight(); // [warn]: size_t will overflow int on 64bit.

			// Enum members
			enum Colors : size_t {
				red, 
				green, 
				blue
			}
			//int color = Colors.green; // FIXME: [warn]: size_t will overflow int on 64bit.
		}
	}c, analysis.run.AnalyzerCheck.size_t_check);
	stderr.writeln("Unittest for SizeTCheck passed.");
}

