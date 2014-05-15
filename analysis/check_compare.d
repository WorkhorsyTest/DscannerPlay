// Copyright (c) 2014, Matthew Brennan Jones <matthew.brennan.jones@gmail.com>
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.check_compare;

import std.stdio;
import std.array;
import std.string;
import std.stdint;

import std.d.ast;
import std.d.inspect;
import std.d.lexer;
import analysis.base;
import analysis.helpers;
import analysis.scope_analyzer;

/**
 * Checks for comparing a variable with itself.
 */
class CompareCheck : ScopeAnalyzer {
	alias visit = ScopeAnalyzer.visit;

	this(string fileName) {
		super(fileName, false);
	}

	// a is a
	// a !is a
	override void visit(const IdentityExpression idenExp) {
		TokenData left = get_expression_return_token_data(idenExp.left);
		TokenData right = get_expression_return_token_data(idenExp.right);

		const string operator = idenExp.negated ? "!is" : "is";

		if(is_same_token_variable(left, right)) {
			string type = left.token_type.capitalize();
			string message = "%s \"%s\" identified(%s) with itself.".format(type, left.name, operator);
			addErrorMessage(left.line, left.column, message);
		}
		idenExp.accept(this);
	}

	// a == a
	// a != a
	override void visit(const EqualExpression eqlExp) {
		bool is_operator = (
			eqlExp.operator == tok!"!=" || 
			eqlExp.operator == tok!"=="
		);

		TokenData left = get_expression_return_token_data(eqlExp.left);
		TokenData right = get_expression_return_token_data(eqlExp.right);

		if(is_same_token_variable(left, right) && is_operator) {
			string type = left.token_type.capitalize();
			string message = "%s \"%s\" equaled(%s) with itself.".format(type, left.name, eqlExp.operator.str);
			addErrorMessage(left.line, left.column, message);
		}
		eqlExp.accept(this);
	}

	// a > a
	// a < a
	// a >= a
	// a <= a
	override void visit(const RelExpression relExp) {
		bool is_operator = (
			relExp.operator == tok!">" || 
			relExp.operator == tok!"<" || 
			relExp.operator == tok!">=" || 
			relExp.operator == tok!"<="
		);

		TokenData left = get_expression_return_token_data(relExp.left);
		TokenData right = get_expression_return_token_data(relExp.right);

		if(is_same_token_variable(left, right) && is_operator) {
			string type = left.token_type.capitalize();
			string message = "%s \"%s\" relationed(%s) with itself.".format(type, left.name, relExp.operator.str);
			addErrorMessage(left.line, left.column, message);
		}
		relExp.accept(this);
	}

	override void visit(const AndAndExpression exp) {
		check_logical(exp);
		exp.accept(this);
	}

	override void visit(const OrOrExpression exp) {
		check_logical(exp);
		exp.accept(this);
	}

	override void visit(const AndExpression exp) {
		check_bitwise(exp);
		exp.accept(this);
	}

	override void visit(const OrExpression exp) {
		check_bitwise(exp);
		exp.accept(this);
	}

	override void visit(const XorExpression exp) {
		check_bitwise(exp);
		exp.accept(this);
	}

	// a && a
	// a || a
	void check_logical(const ExpressionNode exp) {
		TokenData left, right;
		Token operator;

		if(auto orOrExp = cast(const OrOrExpression) exp) {
			left = get_expression_return_token_data(orOrExp.left);
			right = get_expression_return_token_data(orOrExp.right);
			operator = cast(Token) tok!"||";
		} else if(auto andAndExp = cast(const AndAndExpression) exp) {
			left = get_expression_return_token_data(andAndExp.left);
			right = get_expression_return_token_data(andAndExp.right);
			operator = cast(Token) tok!"&&";
		}

		if(is_same_token_variable(left, right)) {
			string type = left.token_type.capitalize();
			string message = null;
			if(operator == tok!"||") {
				message = "%s \"%s\" logical ored(||) with itself.".format(type, left.name);
			} else {
				message = "%s \"%s\" logical anded(&&) with itself.".format(type, left.name);
			}
			addErrorMessage(left.line, left.column, message);
		}
	}

	// a & a
	// a | a
	// a ^ a
	void check_bitwise(const ExpressionNode exp) {
		TokenData left, right;
		Token operator;

		if(auto andExp = cast(const AndExpression) exp) {
			left = get_expression_return_token_data(andExp.left);
			right = get_expression_return_token_data(andExp.right);
			operator = cast(Token) tok!"&";
		} else if(auto orExp = cast(const OrExpression) exp) {
			left = get_expression_return_token_data(orExp.left);
			right = get_expression_return_token_data(orExp.right);
			operator = cast(Token) tok!"|";
		} else if(auto xorExp = cast(const XorExpression) exp) {
			left = get_expression_return_token_data(xorExp.left);
			right = get_expression_return_token_data(xorExp.right);
			operator = cast(Token) tok!"^";
		}

		if(is_same_token_variable(left, right)) {
			string type = left.token_type.capitalize();
			string message = null;
			if(operator == tok!"&") {
				message = "%s \"%s\" bitwise anded(&) with itself.".format(type, left.name);
			} else if(operator == tok!"|") {
				message = "%s \"%s\" bitwise ored(|) with itself.".format(type, left.name);
			} else if(operator == tok!"^") {
				message = "%s \"%s\" bitwise xored(^) with itself.".format(type, left.name);
			}
			addErrorMessage(left.line, left.column, message);
		}
	}
}

unittest {
	should_warn(q{
		void test_compare() {
			int a;
			bool b;

			// Check for comparisons of a variable and itself
			b = a is a; // [warn]: Variable "a" identified(is) with itself.
			b = a !is a; // [warn]: Variable "a" identified(!is) with itself.

			b = a != a; // [warn]: Variable "a" equaled(!=) with itself.
			b = a == a; // [warn]: Variable "a" equaled(==) with itself.

			b = a >= a; // [warn]: Variable "a" relationed(>=) with itself.
			b = a <= a; // [warn]: Variable "a" relationed(<=) with itself.
			b = a > a; // [warn]: Variable "a" relationed(>) with itself.
			b = a < a; // [warn]: Variable "a" relationed(<) with itself.

			b = a && a; // [warn]: Variable "a" logical anded(&&) with itself.
			b = a || a; // [warn]: Variable "a" logical ored(||) with itself.

			a = a & a; // [warn]: Variable "a" bitwise anded(&) with itself.
			a = a | a; // [warn]: Variable "a" bitwise ored(|) with itself.
			a = a ^ a; // [warn]: Variable "a" bitwise xored(^) with itself.

			// Check for comparisons of an object field and itself
			struct Dog {
				int weight;
				int get_weight() {
					return this.weight;
				}
			}
			Dog dog;

			b = dog.get_weight() == dog.get_weight(); // ok
			b = dog.weight == dog.weight; // [warn]: Field "dog.weight" equaled(==) with itself.
			b = dog.weight is dog.weight; // [warn]: Field "dog.weight" identified(is) with itself.
			b = dog.weight !is dog.weight; // [warn]: Field "dog.weight" identified(!is) with itself.

			b = dog.weight != dog.weight; // [warn]: Field "dog.weight" equaled(!=) with itself.
			b = dog.weight == dog.weight; // [warn]: Field "dog.weight" equaled(==) with itself.

			b = dog.weight >= dog.weight; // [warn]: Field "dog.weight" relationed(>=) with itself.
			b = dog.weight <= dog.weight; // [warn]: Field "dog.weight" relationed(<=) with itself.
			b = dog.weight > dog.weight; // [warn]: Field "dog.weight" relationed(>) with itself.
			b = dog.weight < dog.weight; // [warn]: Field "dog.weight" relationed(<) with itself.

			b = dog.weight && dog.weight; // [warn]: Field "dog.weight" logical anded(&&) with itself.
			b = dog.weight || dog.weight; // [warn]: Field "dog.weight" logical ored(||) with itself.

			a = dog.weight & dog.weight; // [warn]: Field "dog.weight" bitwise anded(&) with itself.
			a = dog.weight | dog.weight; // [warn]: Field "dog.weight" bitwise ored(|) with itself.
			a = dog.weight ^ dog.weight; // [warn]: Field "dog.weight" bitwise xored(^) with itself.
		}
	}c, analysis.run.AnalyzerCheck.compare_check);

	stderr.writeln("Unittest for CompareCheck passed.");
}

