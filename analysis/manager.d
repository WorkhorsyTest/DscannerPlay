// Copyright (c) 2014, Matthew Brennan Jones <matthew.brennan.jones@gmail.com>
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.manager;

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
import analysis.expressions;
import analysis.ast_helpers;
import dlang_helper;


struct ModuleFunctionSet
{
	string importName;
	string[] functions;

	// Returns true if a ModuleFunctionSet has a function
	bool hasFunction(Scope scope_, string funcName) const
	{
		return getFunctionFullName(scope_, funcName) !is null;
	}

	string getFunctionFullName(Scope scope_, string funcName) const
	{
		bool isImported = scope_.isImported(this.importName);

		foreach (func; this.functions)
		{
			string fullFuncName = "%s.%s".format(this.importName, func);

			// If the module is imported the short function name may be used
			if (isImported && funcName == func)
			{
				return fullFuncName;
			}

			// If the module is not imported, the full function name must be used
			if (funcName == fullFuncName)
			{
				return fullFuncName;
			}
		}

		return null;
	}
}

void assertAnalyzerWarnings(string code, analysis.run.AnalyzerCheck analyzers, string file=__FILE__, size_t line=__LINE__)
{
	// Run the code and get any warnings
	string[] rawWarnings = analyze("test", cast(ubyte[]) code, analyzers);
	string[] codeLines = code.split("\n");

	// Get the warnings ordered by line
	string[size_t] warnings;
	for (size_t i=0; i<rawWarnings.length; ++i)
	{
		// Skip the warning if it is on line zero
		size_t rawLine = std.conv.to!size_t(rawWarnings[i].between("test(", ":"));
		if (rawLine == 0)
		{
			stderr.writefln("??? Skipping warning because it is on line zero:\n%s", rawWarnings[i]);
			continue;
		}

		size_t warnLine = line - 1 + rawLine;
		warnings[warnLine] = rawWarnings[i].after(")");
	}

	// Get all the messages from the comments in the code
	string[size_t] messages;
	foreach (i, codeLine; codeLines)
	{
		// Skip if no [warn] comment
		if (codeLine.indexOf("// [warn]:") == -1)
			continue;

		// Skip if there is no comment or code
		string codePart = codeLine.before("// ");
		string commentPart = codeLine.after("// ");
		if (!codePart.length || !commentPart.length)
			continue;

		// Get the line of this code line
		size_t lineNo = i + line;

		// Get the message
		messages[lineNo] = commentPart;
	}

	// Throw an assert error if any messages are not listed in the warnings
	foreach (lineNo, message; messages)
	{
		// No warning
		if (lineNo !in warnings)
		{
			string errors = "Expected warning:\n%s\nFrom source code at (%s:?):\n%s".format(
				messages[lineNo],
				lineNo,
				codeLines[lineNo - line]
			);
			throw new core.exception.AssertError(errors, file, lineNo);
		// Different warning
		}
		else if (warnings[lineNo] != messages[lineNo])
		{
			string errors = "Expected warning:\n%s\nBut was:\n%s\nFrom source code at (%s:?):\n%s".format(
				messages[lineNo],
				warnings[lineNo],
				lineNo,
				codeLines[lineNo - line]
			);
			throw new core.exception.AssertError(errors, file, lineNo);
		}
	}

	// FIXME: Update this so the errors are ordered by line number
	// Throw an assert error if there were any warnings that were not expected
	string[] unexpectedWarnings;
	foreach (lineNo, warning; warnings)
	{
		// Unexpected warning
		if (lineNo !in messages)
		{
			unexpectedWarnings ~= "%s\nFrom source code at (%s:?):\n%s".format(
				warning,
				lineNo,
				codeLines[lineNo - line]
			);
		}
	}
	if (unexpectedWarnings.length)
	{
		string message = "Unexpected warnings:\n" ~ unexpectedWarnings.join("\n");
		throw new core.exception.AssertError(message, file, line);
	}
}

class ScopeManager
{
	Scope scope_;

	this()
	{
		scope_ = new Scope();
	}

	void pushFrame()
	{
		// Add a new scope frame
		ScopeFrame frame;
		scope_.frames ~= frame;
	}

	void popFrame()
	{
		// Remove the current scope frame
		scope_.frames = scope_.frames[0 .. $-1];
	}

	void parentsPush(IdentifierType parent)
	{
		scope_.parents.push(parent);
	}

	void parentsPop()
	{
		scope_.parents.pop();
	}

	IdentifierType parentsPeak()
	{
		return scope_.parents.peak();
	}

	void thisPointersPush(string thisPointer)
	{
		scope_.thisPointers.push(thisPointer);
	}

	void thisPointersPop()
	{
		scope_.thisPointers.pop();
	}

	string thisPointersPeak()
	{
		return scope_.thisPointers.peak();
	}

	void decorationsPush(Decoration decoration)
	{
		scope_.decorations.push(decoration);
	}

	void decorationsPop()
	{
		scope_.decorations.pop();
	}

	Decoration decorationsPeak()
	{
		return scope_.decorations.peak();
	}

	void declareImport(const SingleImport singImpo)
	{
		// Just return if anything is null
		if (!singImpo && !singImpo.identifierChain)
		{
			return;
		}

		// Get the whole name from each identifier chunk
		string[] chunks;
		foreach (identifier; singImpo.identifierChain.identifiers)
		{
			chunks ~= identifier.text;
		}
		string importName = chunks.join(".");

		// Just return if already imported
		if (scope_.isImported(importName))
			return;

		// Add the import
		stderr.writefln("??? importing: %s", importName);
		scope_.addImport(importName);

		// Add the module
		string fileName = "%s.d".format(importName);
		loadModule(fileName);
	}

	void declareFunction(const FunctionDeclaration funcDec)
	{
		FunctionData data = getFunctionData(scope_, funcDec);
		scope_.addFunction(data);
	}

	void declareVariable(const VariableDeclaration varDec)
	{
		foreach (varData; getVariableDatas(scope_, varDec))
		{
			scope_.addVariable(varData);
		}
	}

	void declareParameter(const Parameter param)
	{
		// Just print a warning and return if the name is invalid
		if (param.name is Token.init || isNullOrBlank(param.name.text))
		{
			stderr.writeln("!!! declareParameter() failed because param.name was init or blank.");
			std.d.inspect.inspect(stderr, param, "Parameter");
			return;
		}

		VariableData varData;
		varData.name = param.name.text;
		varData.type = getTypeData(param.type);
		varData.isParameter = true;
		varData.line = param.name.line;
		varData.column = param.name.column;

		scope_.addVariable(varData);
	}

	void declareTemplates(const TemplateParameters tempParams)
	{
		TemplateData[] tempDatas = getTemplateDatas(scope_, tempParams);
		foreach (tempData; tempDatas)
		{
			scope_.addTemplateParameter(tempData);
		}
	}

	void declareStruct(const StructDeclaration structDec)
	{
		StructData structData = getStructData(scope_, structDec);
		scope_.addStruct(structData);
	}

	void declareClass(const ClassDeclaration classDec)
	{
		ClassData classData = getClassData(scope_, classDec);
		scope_.addClass(classData);
	}

	void declareEnum(const EnumDeclaration enumDec)
	{
		EnumData enumData = getEnumData(scope_, enumDec);
		scope_.addEnum(enumData);
	}

	void declareModule(const Module mod)
	{
		ModuleData moduleData = getModuleData(scope_, mod);
		scope_.addModule(moduleData);
	}

	void markUsedVariables(const ASTNode node)
	{
		auto unaryExp = cast(const UnaryExpression) node;
		auto ternaryExp = cast(const TernaryExpression) node;
		if (unaryExp || ternaryExp)
		{
			TokenData data;
			if (unaryExp)
				data = getExpressionReturnTokenData(scope_, unaryExp);
			else
				data = getExpressionReturnTokenData(scope_, ternaryExp);

			if (data !is TokenData.init && data.name && 
				(data.tokenType == TokenType.variable || data.tokenType == TokenType.field || data.tokenType == TokenType.method))
			{
				string name = data.name.before(".");
				scope_.setVariableIsUsedByName(name);
			}
		}
		else if (auto exp = cast(const AssignExpression) node)
		{
			markUsedVariables(exp.ternaryExpression);
		}
		else if (auto exp = cast(const AssertExpression) node)
		{
			markUsedVariables(exp.assertion);
		}
		else if (auto exp = cast(const CmpExpression) node)
		{
			markUsedVariables(exp.shiftExpression);
			markUsedVariables(exp.equalExpression);
			markUsedVariables(exp.identityExpression);
			markUsedVariables(exp.relExpression);
			markUsedVariables(exp.inExpression);
		}
		else if (auto exp = cast(AddExpression) node)
		{
			markUsedVariables(exp.left);
			markUsedVariables(exp.right);
		}
		else if (auto exp = cast(AndAndExpression) node)
		{
			markUsedVariables(exp.left);
			markUsedVariables(exp.right);
		}
		else if (auto exp = cast(AndExpression) node)
		{
			markUsedVariables(exp.left);
			markUsedVariables(exp.right);
		}
		else if (auto exp = cast(EqualExpression) node)
		{
			markUsedVariables(exp.left);
			markUsedVariables(exp.right);
		}
		else if (auto exp = cast(IdentityExpression) node)
		{
			markUsedVariables(exp.left);
			markUsedVariables(exp.right);
		}
		else if (auto exp = cast(InExpression) node)
		{
			markUsedVariables(exp.left);
			markUsedVariables(exp.right);
		}
		else if (auto exp = cast(MulExpression) node)
		{
			markUsedVariables(exp.left);
			markUsedVariables(exp.right);
		}
		else if (auto exp = cast(OrExpression) node)
		{
			markUsedVariables(exp.left);
			markUsedVariables(exp.right);
		}
		else if (auto exp = cast(OrOrExpression) node)
		{
			markUsedVariables(exp.left);
			markUsedVariables(exp.right);
		}
		else if (auto exp = cast(PowExpression) node)
		{
			markUsedVariables(exp.left);
			markUsedVariables(exp.right);
		}
		else if (auto exp = cast(RelExpression) node)
		{
			markUsedVariables(exp.left);
			markUsedVariables(exp.right);
		}
		else if (auto exp = cast(ShiftExpression) node)
		{
			markUsedVariables(exp.left);
			markUsedVariables(exp.right);
		}
		else if (auto exp = cast(XorExpression) node)
		{
			markUsedVariables(exp.left);
			markUsedVariables(exp.right);
		}
		else if (node)
		{
			stderr.writefln("!!! markUsedVariables() override needed for: '%s'".format(typeid(node)));
		}
	}

	void loadModule(string fileName)
	{
		// Just return if the file does not exist
		import std.file;
		if (!std.file.exists(fileName) || !std.file.isFile(fileName))
			return;

		// Read the code
		File f = File(fileName);
		auto code = uninitializedArray!(ubyte[])(to!size_t(f.size));
		f.rawRead(code);

		// Lex the code
		auto lexer = byToken(code);
		auto app = appender!(typeof(lexer.front)[])();
		while (!lexer.empty)
		{
			app.put(lexer.front);
			lexer.popFront();
		}

		// Parse the code
		auto p = new std.d.parser.ParseAllocator();
		Module mod = std.d.parser.parseModule(app.data, fileName, p, null);

		// Get data from the module
		ModuleData moduleData = getModuleData(scope_, mod);
		scope_.addModule(moduleData);
	}
}


