// Copyright (c) 2014, Matthew Brennan Jones <matthew.brennan.jones@gmail.com>
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module dlang_helper;

import std.stdio;
import std.string;
import std.stdint;

bool g_log_info = false;

void info(Char, A...)(in Char[] fmt, A args) {
	if(g_log_info)
		writefln(fmt, args);
}

string pad(size_t len) {
	return std.array.replicate("    ", len);
}

S between(S)(S value, S before, S after) 
if(isSomeString!S) {
	return value.after(before).before(after);
}

S before(S)(S value, S separator) 
if(isSomeString!S) {
	auto i = indexOf(value, separator);

	if(i == -1)
		return value;

	return value[0 .. i];
}

S after(S)(S value, S separator) 
if(isSomeString!S) {
	auto i = indexOf(value, separator);

	if(i == -1)
		return "";

	size_t start = i + separator.length;

	return value[start .. $];
}

S after_last(S)(S value, S separator) 
if(isSomeString!S) {
	size_t i = rindex(value, separator);

	if(i == value.length)
		return "";

	size_t start = i + separator.length;

	return value[start .. $];
}

bool ends_with(S)(S value, S match) 
if(isSomeString!S) {
	if(value is null || match is null)
		return false;

	if(value.length < match.length)
		return false;

	return value[$-match.length .. $] == match;
}

bool starts_with(S)(S value, S match) 
if(isSomeString!S) {
	if(value is null || match is null)
		return false;

	if(value.length < match.length)
		return false;

	return value[0 .. match.length] == match;
}

S capitalize(S)(S value)
if(isSomeString!S) {
	S result;
	foreach(i, dchar c; value) {
		if(i == 0)
			result ~= std.uni.toUpper(c);
		else
			result ~= std.uni.toLower(c);
	}
	return cast(S) result;
}

struct Queue(T) {
	private T[] items;

	T peak() {
		if(this.items.length == 0)
			return T.init;

		return this.items[$-1];
	}

	void push(T item) {
		this.items ~= item;
	}

	void pop() {
		this.items = this.items[0 .. $-1];
	}

	size_t length() {
		return this.items.length;
	}

	void clear() {
		this.items.clear();
	}

	T opIndex(size_t i) {
		return this.items[i];
	}

	string toString() {
		return std.conv.to!string(this.items);
	}
}

