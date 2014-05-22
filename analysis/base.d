module analysis.base;

import std.container;
import std.string;
import std.d.ast;
import std.array;

struct Message
{
	string fileName;
	size_t line;
	size_t column;
	string message;
}

enum comparitor = q{ a.line < b.line || a.line < b.line };

// FIXME: This was changed to now allow duplicates. If there are duplicates,
// then there can be multiple messages for each line. Many of the tests
// will break if there can be duplicates.
alias MessageSet = RedBlackTree!(Message, comparitor);
//alias MessageSet = RedBlackTree!(Message, comparitor, true);

abstract class BaseAnalyzer : ASTVisitor
{
public:
	this(string fileName)
	{
		this.fileName = fileName;
		_messages = new MessageSet;
	}

	Message[] messages()
	{
		return _messages[].array;
	}

protected:

	bool inAggregate = false;

	template visitTemplate(T)
	{
		override void visit(const T structDec)
		{
			inAggregate = true;
			structDec.accept(this);
			inAggregate = false;
		}
	}

	import core.vararg;

	void addErrorMessage(size_t line, size_t column, string message)
	{
		_messages.insert(Message(fileName, line, column, message));
	}

	/**
	 * The file name
	 */
	string fileName;

	MessageSet _messages;
}

