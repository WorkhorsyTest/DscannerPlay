// Copyright (c) 2014, Matthew Brennan Jones <matthew.brennan.jones@gmail.com>
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

module analysis.walking_analyzer;

import std.stdio;
import std.string;
import std.stdint;

import std.d.ast;
import std.d.codegen;
import std.d.lexer;
import analysis.base;
import analysis.helpers;
import analysis.stack_frame;
import dlang_helper;


/*
This works like the BaseAnalyzer but tracks all the variables, classes, 
functions, et cetera, and know what is in scope and hold meta data 
about everything.
*/
class BaseWalkingAnalyzer : BaseAnalyzer {
	alias visit = BaseAnalyzer.visit;

	private bool _log_info = false;

	this(string fileName, bool log_info) {
		super(fileName);
		_log_info = log_info;
	}

	override void visit(const Module mod) {
		// Turn logging on if desired
		bool prev_log = g_log_info;
		g_log_info = _log_info;

		//std.d.inspect.inspect(mod, 0, "mod");
		mod.accept(this);

		// Return to previous logging mode
		g_log_info = prev_log;
	}

	override void visit(const Token token) {
//		stderr.writefln("???? walk called: %s", typeid(token));
//		info("%stoken type:%s, text:%s, line:%s, column:%s", 
//			pad(indent++), token.type.str, token.text, token.line, token.column);

//		token.accept(this);
	}

//	override void visit(const IdType idType) {
//		stderr.writefln("???? walk called: %s", typeid(idType));
//		idType.accept(this);
//	}

	void visit(const ASTNode node) {
//		if(node)
//			stderr.writefln("???? walk called: %s", typeid(node));
		// Just return if the node is null
		if(node is null)
			return;

		// Move to the next stack frame if needed
		if(cast(const BlockStatement) node || cast(const Module) node) {
			info("start frame by %s", typeid(node));
			stack_frame_start();
		}

		// Call the start events for this type of node
		if(!(cast(const FunctionDeclaration) node)) {
			this.call_visit(node);
		}

		if(node) {
//			info("%s%s : %s", pad(indent++), name, typeid(node));
		}

		if(auto addExp =  cast(const AddExpression) node) {
			mark_used_variables(addExp);

			addExp.accept(this);
		} else if(auto aliasDec =  cast(const AliasDeclaration) node) {
			aliasDec.accept(this);
		} else if(auto aliasThisDecl =  cast(const AliasThisDeclaration) node) {
			aliasThisDecl.accept(this);
		} else if(auto alignAttr =  cast(const AlignAttribute) node) {
			alignAttr.accept(this);
		} else if(auto andAndExp =  cast(const AndAndExpression) node) {
			mark_used_variables(andAndExp);

			andAndExp.accept(this);
		} else if(auto andExp =  cast(const AndExpression) node) {
			mark_used_variables(andExp);

			andExp.accept(this);
		} else if(auto argList = cast(const ArgumentList) node) {
			argList.accept(this);
		} else if(auto argu = cast(const Arguments) node) {
			argu.accept(this);
		} else if(auto arrayInit = cast(const ArrayInitializer) node) {
			arrayInit.accept(this);
		} else if(auto arrayLit = cast(const ArrayLiteral) node) {
			arrayLit.accept(this);
		} else if(auto arrayMemInit = cast(const ArrayMemberInitialization) node) {
			arrayMemInit.accept(this);
		} else if(auto asmAddExp = cast(const AsmAddExp) node) {
			asmAddExp.accept(this);
		} else if(auto asmBrExp = cast(const AsmBrExp) node) {
			asmBrExp.accept(this);
		} else if(auto asmEqualExp = cast(const AsmEqualExp) node) {
			asmEqualExp.accept(this);
		} else if(auto asmExp = cast(const AsmExp) node) {
			asmExp.accept(this);
		} else if(auto asmInstruction = cast(const AsmInstruction) node) {
			asmInstruction.accept(this);
		} else if(auto asmLogAndExp = cast(const AsmLogAndExp) node) {
			asmLogAndExp.accept(this);
		} else if(auto asmLogOrExp = cast(const AsmLogOrExp) node) {
			asmLogOrExp.accept(this);
		} else if(auto asmMulExp = cast(const AsmMulExp) node) {
			asmMulExp.accept(this);
		} else if(auto asmOrExp = cast(const AsmOrExp) node) {
			asmOrExp.accept(this);
		} else if(auto asmPrimaryExp = cast(const AsmPrimaryExp) node) {
			asmPrimaryExp.accept(this);
		} else if(auto asmRelExp = cast(const AsmRelExp) node) {
			asmRelExp.accept(this);
		} else if(auto asmShiftExp = cast(const AsmShiftExp) node) {
			asmShiftExp.accept(this);
		} else if(auto asmStatement = cast(const AsmStatement) node) {
			asmStatement.accept(this);
		} else if(auto asmTypePrefix = cast(const AsmTypePrefix) node) {
			asmTypePrefix.accept(this);
		} else if(auto asmUnaExp = cast(const AsmUnaExp) node) {
			asmUnaExp.accept(this);
		} else if(auto asmXorExp = cast(const AsmXorExp) node) {
			asmXorExp.accept(this);
		} else if(auto assertExp = cast(const AssertExpression) node) {
			mark_used_variables(assertExp);

			assertExp.accept(this);
		} else if(auto assExp = cast(const AssignExpression) node) {
			mark_used_variables(assExp);

			assExp.accept(this);
		} else if(auto assocArrayLiteral = cast(const AssocArrayLiteral) node) {
			assocArrayLiteral.accept(this);
		} else if(auto atAttribute = cast(const AtAttribute) node) {
			atAttribute.accept(this);
		} else if(auto attr = cast(const Attribute) node) {
			attr.accept(this);
		} else if(auto attribDec = cast(const AttributeDeclaration) node) {
			attribDec.accept(this);
		} else if(auto autoDec = cast(const AutoDeclaration) node) {
			autoDec.accept(this);
		} else if(auto baseClass = cast(const BaseClass) node) {
			baseClass.accept(this);
		} else if(auto baseClassList = cast(const BaseClassList) node) {
			baseClassList.accept(this);
		} else if(auto blkSta = cast(const BlockStatement) node) {
			blkSta.accept(this);
		} else if(auto bodyStatement = cast(const BodyStatement) node) {
			bodyStatement.accept(this);
		} else if(auto breakSta = cast(const BreakStatement) node) {
			breakSta.accept(this);
		} else if(auto caseRangeStatement = cast(const CaseRangeStatement) node) {
			caseRangeStatement.accept(this);
		} else if(auto caseSta = cast(const CaseStatement) node) {
			caseSta.accept(this);
		} else if(auto castExp = cast(const CastExpression) node) {
			castExp.accept(this);
		} else if(auto castQualifier = cast(const CastQualifier) node) {
			castQualifier.accept(this);
		} else if(auto catches = cast(const Catches) node) {
			catches.accept(this);
		} else if(auto catch_ = cast(const Catch) node) {
			catch_.accept(this);
		} else if(auto classDec = cast(const ClassDeclaration) node) {
			parents.push(IdentifierType.class_);
			this_pointers.push(classDec.name.text);

			declare_class(classDec);

			classDec.accept(this);

			this_pointers.pop();
			parents.pop();
		} else if(auto cmpExp = cast(const CmpExpression) node) {
			mark_used_variables(cmpExp);

			cmpExp.accept(this);
		} else if(auto compSta = cast(const CompileCondition) node) {
			compSta.accept(this);
		} else if(auto condDec = cast(const ConditionalDeclaration) node) {
			condDec.accept(this);
		} else if(auto condSta = cast(const ConditionalStatement) node) {
			condSta.accept(this);
		} else if(auto constr = cast(const Constraint) node) {
			constr.accept(this);
		} else if(auto constr = cast(const Constructor) node) {
			constr.accept(this);
		} else if(auto continueStatement = cast(const ContinueStatement) node) {
			continueStatement.accept(this);
		} else if(auto debugCond = cast(const DebugCondition) node) {
			debugCond.accept(this);
		} else if(auto debugSpecification = cast(const DebugSpecification) node) {
			debugSpecification.accept(this);
		} else if(auto decl = cast(const Declaration) node) {
			// Add decorations such as properties, auto, ref, et cetera
			Decoration decoration = get_declaration_decorations(decl);
			decorations.push(decoration);

			decl.accept(this);

			decorations.pop();
		} else if(auto dclrAndSta = cast(const DeclarationsAndStatements) node) {
			dclrAndSta.accept(this);
		} else if(auto dclrOrSta = cast(const DeclarationOrStatement) node) {
			dclrOrSta.accept(this);
		} else if(auto declar = cast(const Declarator) node) {
			declar.accept(this);
		} else if(auto defaultSta = cast(const DefaultStatement) node) {
			defaultSta.accept(this);
		} else if(auto delExpr = cast(const DeleteExpression) node) {
			delExpr.accept(this);
		} else if(auto deleteStatement = cast(const DeleteStatement) node) {
			deleteStatement.accept(this);
		} else if(auto deprecated_ = cast(const Deprecated) node) {
			deprecated_.accept(this);
		} else if(auto destructor = cast(const Destructor) node) {
			destructor.accept(this);
		} else if(auto doSta = cast(const DoStatement) node) {
			doSta.accept(this);
		} else if(auto enumBody = cast(const EnumBody) node) {
			enumBody.accept(this);
		} else if(auto enumDec = cast(const EnumDeclaration) node) {
			parents.push(IdentifierType.enum_);

			declare_enum(enumDec);

			enumDec.accept(this);

			parents.pop();
		} else if(auto enumMem = cast(const EnumMember) node) {
			enumMem.accept(this);
		} else if(auto epon = cast(const EponymousTemplateDeclaration) node) {
			epon.accept(this);
		} else if(auto equalExp =  cast(const EqualExpression) node) {
			mark_used_variables(equalExp);

			equalExp.accept(this);
		} else if(auto expr = cast(const Expression) node) {
			expr.accept(this);
		} else if(auto exprSta = cast(const ExpressionStatement) node) {
			exprSta.accept(this);
		} else if(auto finalSwitchStatement = cast(const FinalSwitchStatement) node) {
			finalSwitchStatement.accept(this);
		} else if(auto finally_ = cast(const Finally) node) {
			finally_.accept(this);
		} else if(auto forEach = cast(const ForeachStatement) node) {
			forEach.accept(this);
		} else if(auto forEachType = cast(const ForeachType) node) {
			forEachType.accept(this);
		} else if(auto forEachTypeList = cast(const ForeachTypeList) node) {
			forEachTypeList.accept(this);
		} else if(auto forSta = cast(const ForStatement) node) {
			forSta.accept(this);
		} else if(auto functionAttribute = cast(const FunctionAttribute) node) {
			functionAttribute.accept(this);
		} else if(auto funcBody = cast(const FunctionBody) node) {
			funcBody.accept(this);
		} else if(auto fncExp = cast(const FunctionCallExpression) node) {
			fncExp.accept(this);
		} else if(auto functionCallStatement = cast(const FunctionCallStatement) node) {
			functionCallStatement.accept(this);
		} else if(auto funcDec = cast(const FunctionDeclaration) node) {
			// Only declare if NOT a struct/class method
			if(parents.peak != IdentifierType.struct_ && parents.peak != IdentifierType.class_) {
				declare_function(funcDec);
			}

			parents.push(IdentifierType.function_);
			stack_frame_start();
			this.call_visit(node);

			funcDec.accept(this);

			this.call_visit(node);
			stack_frame_exit();
			parents.pop();
		} else if(auto funcLitExp = cast(const FunctionLiteralExpression) node) {
			funcLitExp.accept(this);
		} else if(auto gotoSta = cast(const GotoStatement) node) {
			gotoSta.accept(this);
		} else if(auto idenChain = cast(const IdentifierChain) node) {
			idenChain.accept(this);
		} else if(auto idenList = cast(const IdentifierList) node) {
			idenList.accept(this);
		} else if(auto idenOrTempChain = cast(const IdentifierOrTemplateChain) node) {
			idenOrTempChain.accept(this);
		} else if(auto idenTemp = cast(const IdentifierOrTemplateInstance) node) {
			idenTemp.accept(this);
		} else if(auto idenExp =  cast(const IdentityExpression) node) {
			mark_used_variables(idenExp);

			idenExp.accept(this);
		} else if(auto ifStatement = cast(const IfStatement) node) {
			ifStatement.accept(this);
		} else if(auto imptBind = cast(const ImportBind) node) {
			imptBind.accept(this);
		} else if(auto imptBinds = cast(const ImportBindings) node) {
			imptBinds.accept(this);
		} else if(auto imptDec = cast(const ImportDeclaration) node) {
			imptDec.accept(this);
		} else if(auto imptExpr = cast(const ImportExpression) node) {
			imptExpr.accept(this);
		} else if(auto indexExp = cast(const IndexExpression) node) {
			indexExp.accept(this);
		} else if(auto inExp =  cast(const InExpression) node) {
			mark_used_variables(inExp);

			inExp.accept(this);
		} else if(auto inSta =  cast(const InStatement) node) {
			inSta.accept(this);
		} else if(auto init = cast(const Initialize) node) {
			init.accept(this);
		} else if(auto init = cast(const Initializer) node) {
			init.accept(this);
		} else if(auto interDec = cast(const InterfaceDeclaration) node) {
			interDec.accept(this);
		} else if(auto invar = cast(const Invariant) node) {
			invar.accept(this);
		} else if(auto isExpr = cast(const IsExpression) node) {
			isExpr.accept(this);
		} else if(auto linkageAttr = cast(const LinkageAttribute) node) {
			linkageAttr.accept(this);
		} else if(auto mod = cast(const Module) node) {
			parents.push(IdentifierType.module_);
			declare_module(mod);

			visit(mod.moduleDeclaration);

			// Declare the global functions and variables
			// This is a special case, because global functions do NOT have to 
			// be declared in order.
			foreach(decl; mod.declarations) {
				if(!decl) continue;

				// Add decorations such as properties, auto, ref, et cetera
				Decoration decoration = get_declaration_decorations(decl);
				decorations.push(decoration);

				// Add the import
				if(decl.importDeclaration && decl.importDeclaration.singleImports) {
					foreach(singleImport; decl.importDeclaration.singleImports) {
						declare_import(singleImport);
					}
				// Declare the function
				} else if(decl.functionDeclaration) {
					declare_function(decl.functionDeclaration);
				// Declare the variable
				} else if(decl.variableDeclaration) {
					declare_variable(decl.variableDeclaration);
				}

				// Remove decorations
				decorations.pop();
			}

			// Walk through the declarations normally
			foreach(decl; mod.declarations) {
				visit(decl);
			}

			parents.pop();
		} else if(auto modDec = cast(const ModuleDeclaration) node) {
			modDec.accept(this);
		} else if(auto mulExp =  cast(const MulExpression) node) {
			mark_used_variables(mulExp);

			mulExp.accept(this);
		} else if(auto newExpr = cast(const NewExpression) node) {
			newExpr.accept(this);
		} else if(auto nonVoidInit = cast(const NonVoidInitializer) node) {
			nonVoidInit.accept(this);
		} else if(auto orOrExp =  cast(const OrOrExpression) node) {
			mark_used_variables(orOrExp);

			orOrExp.accept(this);
		} else if(auto orExp =  cast(const OrExpression) node) {
			mark_used_variables(orExp);

			orExp.accept(this);
		} else if(auto param = cast(const Parameter) node) {
			// Declare parametr
			declare_parameter(param);

			param.accept(this);
		} else if(auto params = cast(const Parameters) node) {
			params.accept(this);
		} else if(auto powExp =  cast(const PowExpression) node) {
			mark_used_variables(powExp);

			powExp.accept(this);
		} else if(auto priExp = cast(const PrimaryExpression) node) {
			priExp.accept(this);
		} else if(auto relExp =  cast(const RelExpression) node) {
			mark_used_variables(relExp);

			relExp.accept(this);
		} else if(auto retSta = cast(const ReturnStatement) node) {
			retSta.accept(this);
		} else if(auto shiftExp =  cast(const ShiftExpression) node) {
			mark_used_variables(shiftExp);

			shiftExp.accept(this);
		} else if(auto singImpo = cast(const SingleImport) node) {
			declare_import(singImpo);

			singImpo.accept(this);
		} else if(auto sliceExpt = cast(const SliceExpression) node) {
			sliceExpt.accept(this);
		} else if(auto stat = cast(const Statement) node) {
			stat.accept(this);
		} else if(auto stateNoCaseNoDef = cast(const StatementNoCaseNoDefault) node) {
			stateNoCaseNoDef.accept(this);
		} else if(auto storageClass = cast(const StorageClass) node) {
			storageClass.accept(this);
		} else if(auto structBody = cast(const StructBody) node) {
			structBody.accept(this);
		} else if(auto strucDec = cast(const StructDeclaration) node) {
			parents.push(IdentifierType.struct_);
			this_pointers.push(strucDec.name.text);

			declare_struct(strucDec);

			strucDec.accept(this);

			this_pointers.pop();
			parents.pop();
		} else if(auto switchSta = cast(const SwitchStatement) node) {
			switchSta.accept(this);
		} else if(auto symbol = cast(const Symbol) node) {
			symbol.accept(this);
		} else if(auto terExp = cast(const TernaryExpression) node) {
			terExp.accept(this);
		} else if(auto templArgList = cast(const TemplateArgumentList) node) {
			templArgList.accept(this);
		} else if(auto templArgs = cast(const TemplateArguments) node) {
			templArgs.accept(this);
		} else if(auto templArg = cast(const TemplateArgument) node) {
			templArg.accept(this);
		} else if(auto tempDec = cast(const TemplateDeclaration) node) {
			tempDec.accept(this);
		} else if(auto templInst = cast(const TemplateInstance) node) {
			templInst.accept(this);
		} else if(auto templSingArg = cast(const TemplateSingleArgument) node) {
			templSingArg.accept(this);
		} else if(auto tempParams = cast(const TemplateParameters) node) {
			declare_templates(tempParams);

			tempParams.accept(this);
		} else if(auto templParamList = cast(const TemplateParameterList) node) {
			templParamList.accept(this);
		} else if(auto tempParam = cast(const TemplateParameter) node) {
			tempParam.accept(this);
		} else if(auto tempTypeParam = cast(const TemplateTypeParameter) node) {
			tempTypeParam.accept(this);
		} else if(auto tempValParam = cast(const TemplateValueParameter) node) {
			tempValParam.accept(this);
		} else if(auto throwSta = cast(const ThrowStatement) node) {
			throwSta.accept(this);
		} else if(auto astType = cast(const Type) node) {
			astType.accept(this);
		} else if(auto astType2 = cast(const Type2) node) {
			astType2.accept(this);
		} else if(auto typeidExp = cast(const TypeidExpression) node) {
			typeidExp.accept(this);
		} else if(auto typeOfExp = cast(const TypeofExpression) node) {
			typeOfExp.accept(this);
		} else if(auto typeSuffix = cast(const TypeSuffix) node) {
			typeSuffix.accept(this);
		} else if(auto unrExp = cast(const UnaryExpression) node) {
			unrExp.accept(this);
		} else if(auto unitTest = cast(const Unittest) node) {
			unitTest.accept(this);
		} else if(auto varDec = cast(const VariableDeclaration) node) {
			varDec.accept(this);

			// Only declare if NOT a struct/class field
			if(parents.peak != IdentifierType.struct_ && parents.peak != IdentifierType.class_) {
				declare_variable(varDec);
			}
		} else if(auto verCond = cast(const VersionCondition) node) {
			verCond.accept(this);
		} else if(auto trySta = cast(const TryStatement) node) {
			trySta.accept(this);
		} else if(auto whileSta = cast(const WhileStatement) node) {
			whileSta.accept(this);
		} else if(auto xorExp =  cast(const XorExpression) node) {
			mark_used_variables(xorExp);

			xorExp.accept(this);
		} else if(node) {
//			stderr.writefln("%s!!! accept() override needed for: %s", pad(indent), typeid(node));
		} else {
//			stderr.writefln("%s!!! accept() override needed for unknown node type", pad(indent));
		}

		// Call the end events for this type of node
		if(!(cast(const FunctionDeclaration) node)) {
			this.call_visit(node);
		}

		// Move to the previous stack frame if needed
		if(cast(const BlockStatement) node || cast(const Module) node) {
			info("exit frame by %s", typeid(node));
			stack_frame_exit();
		}
	}
/*
	void call_visit(const Token token) {
		this.visit(token);
	}

	void call_visit(const IdType idType) {
		this.visit(idType);
	}
*/
	void call_visit(const ASTNode unknown) {
		mixin(std.d.codegen.callOnActualType!(
			"this.visit(actual)", 
			"throw new Exception(std.string.format(\"Unexpected ast node type: %s\", typeid(unknown)))", 
			NODE_TYPE_NAMES));
	}
}

