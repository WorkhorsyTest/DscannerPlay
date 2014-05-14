// Copyright (c) 2014, Matthew Brennan Jones <matthew.brennan.jones@gmail.com>
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.check_name_clash;

import std.stdio;
import std.array;
import std.string;
import std.stdint;

import std.d.ast;
import std.d.inspect;
import std.d.lexer;
import analysis.base;
import analysis.helpers;
import analysis.stack_frame;
import analysis.walking_analyzer;

/**
 * Checks for name clashes in classes, structs, variables, parameters, enums, and members.
 */
class NameClashCheck : BaseWalkingAnalyzer {
	alias visit = BaseWalkingAnalyzer.visit;

	this(string fileName) {
		super(fileName, false);
	}

	override void visit(const Module mod) {
		mod.accept(this);

		foreach(name, positions; get_name_clashes()) {
			// Skip if there are less than two
			if(positions.length < 2)
				continue;

			// Get the original position
			auto orig = positions[0];

			// Add an error for each clashing name
			foreach(pos; positions[1 .. $]) {
				string message = "The %s '%s' clashes with the %s at (%s:%s).".format(
					pos.type_name(), 
					name, 
					orig.type_name(), 
					orig.line, 
					orig.column
				);
				addErrorMessage(pos.line, pos.column, message);
			}
		}
	}
}

unittest {
	// FIXME: Make it work with class/struct static fields and methods.
	// FIXME: Make it work with functions, and delegates too.
	should_warn(q{
		// Variable vs nested variable
		string sound = "quack";
		void talk() {
			string sound = "oink"; // [warn]: The variable 'sound' clashes with the variable at (3:10).
		}

		// Function vs nested variable
		void name() {
			string name; // [warn]: The variable 'name' clashes with the function at (9:8).
		}

		// Variable vs function parameter
		int doughnut;
		void eat(int doughnut) { // [warn]: The parameter 'doughnut' clashes with the variable at (14:7).
		}

		// Function vs function parameter
		void cake(int cake) { // [warn]: The parameter 'cake' clashes with the function at (19:8).
		}

		// Function vs nested function
		void pie() {
			void pie() { // [warn]: The function 'pie' clashes with the function at (23:8).
			}
		}

		// Function vs nested struct
		void monkey() {
			struct monkey { // [warn]: The struct 'monkey' clashes with the function at (29:8).
			}
		}

		// Struct vs member variable vs nested variable
		struct dog {
			int dog; // [warn]: The field 'dog' clashes with the struct at (35:10).
			void bark() {
				int dog; // [warn]: The variable 'dog' clashes with the struct at (35:10).
			}
		}

		// Struct vs member method vs nested variable
		struct cat {
			void cat() { // [warn]: The method 'cat' clashes with the struct at (43:10).
				int cat; // [warn]: The variable 'cat' clashes with the struct at (43:10).
			}
		}

		// Struct member variable vs variable vs nested variable
		int weight;
		struct bird {
			int weight; // [warn]: The field 'weight' clashes with the variable at (50:7).
			void fly() {
				int weight; // [warn]: The variable 'weight' clashes with the variable at (50:7).
			}
		}

		// Class member variable vs variable
		string flavor;
		class pig {
			string flavor; // [warn]: The field 'flavor' clashes with the variable at (59:10).
			void oink() {
				string pig = "honey baked ham"; // [warn]: The variable 'pig' clashes with the class at (60:9).
				this.flavor = "canadian bacon";
			}
		}

		// Enum vs variable
		int coffee;
		void drinks() {
			enum coffee {// [warn]: The enum 'coffee' clashes with the variable at (69:7).
				coffee // [warn]: The field 'coffee' clashes with the variable at (69:7).
			}
		}
	}c, analysis.run.AnalyzerCheck.name_clash_check);

	stderr.writeln("Unittest for NameClashCheck passed.");
}

