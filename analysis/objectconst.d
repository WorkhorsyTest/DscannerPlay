//          Copyright Brian Schott (Sir Alaran) 2014.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.objectconst;

import std.stdio;
import std.regex;
import std.d.ast;
import std.d.lexer;
import analysis.base;
import analysis.helpers;

/**
 * Checks that opEquals, opCmp, toHash, and toString are either const,
 * immutable, or inout.
 */
class ObjectConstCheck : BaseAnalyzer
{
	alias visit = BaseAnalyzer.visit;

	this(string fileName)
	{
		super(fileName);
	}

	mixin visitTemplate!ClassDeclaration;
	mixin visitTemplate!InterfaceDeclaration;
	mixin visitTemplate!UnionDeclaration;
	mixin visitTemplate!StructDeclaration;

	override void visit(const Declaration d)
	{
		if (inAggregate && d.functionDeclaration !is null
			&& isInteresting(d.functionDeclaration.name.text)
			&& (!hasConst(d.attributes)
			&& !hasConst(d.functionDeclaration.memberFunctionAttributes)))
		{
			addErrorMessage(d.functionDeclaration.name.line,
				d.functionDeclaration.name.column, "opCmp, toHash, opEquals,"
					~ " and toString should be declared const");
		}
		d.accept(this);
	}

	private static bool hasConst(const Attribute[] attributes)
	{
		import std.algorithm;
		return attributes.any!(a => a.attribute == tok!"const"
			|| (a.storageClass !is null && a.storageClass.token == tok!"const"));
	}

	private static bool hasConst(const MemberFunctionAttribute[] attributes)
	{
		import std.algorithm;
		return attributes.any!(a => a.tokenType == tok!"const"
			|| a.tokenType == tok!"immutable"
			|| a.tokenType == tok!"inout");
	}

	private static bool isInteresting(string name)
	{
		return name == "opCmp" || name == "toHash" || name == "opEquals"
			|| name == "toString";
	}

	private bool looking = false;

}

unittest {
	assertAnalyzerWarnings(q{
		void test_consts() {
			// Will be ok because all are declared const/immutable
			class Cat {
				const bool opEquals(Object a, Object b) { // ok
					return true;
				}

				const int opCmp(Object o) { // ok
					return 1;
				}

				const hash_t toHash() { // ok
					return 0;
				}

				const string toString() { // ok
					return "Cat";
				}
			}

			// Will warn, because none are const
			class Dog {
				bool opEquals(Object a, Object b) { // [warn]: opCmp, toHash, opEquals, and toString should be declared const
					return true;
				}

				int opCmp(Object o) { // [warn]: opCmp, toHash, opEquals, and toString should be declared const
					return 1;
				}

				hash_t toHash() { // [warn]: opCmp, toHash, opEquals, and toString should be declared const
					return 0;
				}

				string toString() { // [warn]: opCmp, toHash, opEquals, and toString should be declared const
					return "Dog";
				}
			}
		}
	}c, analysis.run.AnalyzerCheck.object_const_check);

	stderr.writeln("Unittest for ObjectConstCheck passed.");
}

