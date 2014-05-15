// Written in the D programming language.

/**
 * This module defines an Abstract Syntax Tree for the D language
 *
 * Examples:
 * ---
 * // TODO
 * ---
 *
 * Copyright: Brian Schott 2013
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt Boost, License 1.0)
 * Authors: Brian Schott
 * Source: $(PHOBOSSRC std/d/_ast.d)
 */

module std.d.ast;

import std.d.lexer;
import std.traits;
import std.algorithm;
import std.array;
import std.string;
import std.stdio;

// TODO: Many of these classes can be simplified by using std.variant.Algebraic

/**
 * Implements the $(LINK2 http://en.wikipedia.org/wiki/Visitor_pattern, Visitor Pattern)
 * for the various AST classes
 */
abstract class ASTVisitor
{
public:

    /** */ void visit(const AddExpression addExpression) { addExpression.accept(this); }
    /** */ void visit(const AliasDeclaration aliasDeclaration) { aliasDeclaration.accept(this); }
    /** */ void visit(const AliasInitializer aliasInitializer) { aliasInitializer.accept(this); }
    /** */ void visit(const AliasThisDeclaration aliasThisDeclaration) { aliasThisDeclaration.accept(this); }
    /** */ void visit(const AlignAttribute alignAttribute) { alignAttribute.accept(this); }
    /** */ void visit(const AndAndExpression andAndExpression) { andAndExpression.accept(this); }
    /** */ void visit(const AndExpression andExpression) { andExpression.accept(this); }
    /** */ void visit(const ArgumentList argumentList) { argumentList.accept(this); }
    /** */ void visit(const Arguments arguments) { arguments.accept(this); }
    /** */ void visit(const ArrayInitializer arrayInitializer) { arrayInitializer.accept(this); }
    /** */ void visit(const ArrayLiteral arrayLiteral) { arrayLiteral.accept(this); }
    /** */ void visit(const ArrayMemberInitialization arrayMemberInitialization) { arrayMemberInitialization.accept(this); }
    /** */ void visit(const AsmAddExp asmAddExp) { asmAddExp.accept(this); }
    /** */ void visit(const AsmAndExp asmAndExp) { asmAndExp.accept(this); }
    /** */ void visit(const AsmBrExp asmBrExp) { asmBrExp.accept(this); }
    /** */ void visit(const AsmEqualExp asmEqualExp) { asmEqualExp.accept(this); }
    /** */ void visit(const AsmExp asmExp) { asmExp.accept(this); }
    /** */ void visit(const AsmInstruction asmInstruction) { asmInstruction.accept(this); }
    /** */ void visit(const AsmLogAndExp asmLogAndExp) { asmLogAndExp.accept(this); }
    /** */ void visit(const AsmLogOrExp asmLogOrExp) { asmLogOrExp.accept(this); }
    /** */ void visit(const AsmMulExp asmMulExp) { asmMulExp.accept(this); }
    /** */ void visit(const AsmOrExp asmOrExp) { asmOrExp.accept(this); }
    /** */ void visit(const AsmPrimaryExp asmPrimaryExp) { asmPrimaryExp.accept(this); }
    /** */ void visit(const AsmRelExp asmRelExp) { asmRelExp.accept(this); }
    /** */ void visit(const AsmShiftExp asmShiftExp) { asmShiftExp.accept(this); }
    /** */ void visit(const AsmStatement asmStatement) { asmStatement.accept(this); }
    /** */ void visit(const AsmTypePrefix asmTypePrefix) { asmTypePrefix.accept(this); }
    /** */ void visit(const AsmUnaExp asmUnaExp) { asmUnaExp.accept(this); }
    /** */ void visit(const AsmXorExp asmXorExp) { asmXorExp.accept(this); }
    /** */ void visit(const AssertExpression assertExpression) { assertExpression.accept(this); }
    /** */ void visit(const AssignExpression assignExpression) { assignExpression.accept(this); }
    /** */ void visit(const AssocArrayLiteral assocArrayLiteral) { assocArrayLiteral.accept(this); }
    /** */ void visit(const AtAttribute atAttribute) { atAttribute.accept(this); }
    /** */ void visit(const Attribute attribute) { attribute.accept(this); }
    /** */ void visit(const AttributeDeclaration attributeDeclaration) { attributeDeclaration.accept(this); }
    /** */ void visit(const AutoDeclaration autoDeclaration) { autoDeclaration.accept(this); }
    /** */ void visit(const BlockStatement blockStatement) { blockStatement.accept(this); }
    /** */ void visit(const BodyStatement bodyStatement) { bodyStatement.accept(this); }
    /** */ void visit(const BreakStatement breakStatement) { breakStatement.accept(this); }
    /** */ void visit(const BaseClass baseClass) { baseClass.accept(this); }
    /** */ void visit(const BaseClassList baseClassList) { baseClassList.accept(this); }
    /** */ void visit(const CaseRangeStatement caseRangeStatement) { caseRangeStatement.accept(this); }
    /** */ void visit(const CaseStatement caseStatement) { caseStatement.accept(this); }
    /** */ void visit(const CastExpression castExpression) { castExpression.accept(this); }
    /** */ void visit(const CastQualifier castQualifier) { castQualifier.accept(this); }
    /** */ void visit(const Catch catch_) { catch_.accept(this); }
    /** */ void visit(const Catches catches) { catches.accept(this); }
    /** */ void visit(const ClassDeclaration classDeclaration) { classDeclaration.accept(this); }
    /** */ void visit(const CmpExpression cmpExpression) { cmpExpression.accept(this); }
    /** */ void visit(const CompileCondition compileCondition) { compileCondition.accept(this); }
    /** */ void visit(const ConditionalDeclaration conditionalDeclaration) { conditionalDeclaration.accept(this); }
    /** */ void visit(const ConditionalStatement conditionalStatement) { conditionalStatement.accept(this); }
    /** */ void visit(const Constraint constraint) { constraint.accept(this); }
    /** */ void visit(const Constructor constructor) { constructor.accept(this); }
    /** */ void visit(const ContinueStatement continueStatement) { continueStatement.accept(this); }
    /** */ void visit(const DebugCondition debugCondition) { debugCondition.accept(this); }
    /** */ void visit(const DebugSpecification debugSpecification) { debugSpecification.accept(this); }
    /** */ void visit(const Declaration declaration) { declaration.accept(this); }
    /** */ void visit(const DeclarationOrStatement declarationsOrStatement) { declarationsOrStatement.accept(this); }
    /** */ void visit(const DeclarationsAndStatements declarationsAndStatements) { declarationsAndStatements.accept(this); }
    /** */ void visit(const Declarator declarator) { declarator.accept(this); }
    /** */ void visit(const DefaultStatement defaultStatement) { defaultStatement.accept(this); }
    /** */ void visit(const DeleteExpression deleteExpression) { deleteExpression.accept(this); }
    /** */ void visit(const DeleteStatement deleteStatement) { deleteStatement.accept(this); }
    /** */ void visit(const Deprecated deprecated_) { deprecated_.accept(this); }
    /** */ void visit(const Destructor destructor) { destructor.accept(this); }
    /** */ void visit(const DoStatement doStatement) { doStatement.accept(this); }
    /** */ void visit(const EnumBody enumBody) { enumBody.accept(this); }
    /** */ void visit(const EnumDeclaration enumDeclaration) { enumDeclaration.accept(this); }
    /** */ void visit(const EnumMember enumMember) { enumMember.accept(this); }
    /** */ void visit(const EponymousTemplateDeclaration eponymousTemplateDeclaration) { eponymousTemplateDeclaration.accept(this); }
    /** */ void visit(const EqualExpression equalExpression) { equalExpression.accept(this); }
    /** */ void visit(const Expression expression) { expression.accept(this); }
    /** */ void visit(const ExpressionStatement expressionStatement) { expressionStatement.accept(this); }
    /** */ void visit(const FinalSwitchStatement finalSwitchStatement) { finalSwitchStatement.accept(this); }
    /** */ void visit(const Finally finally_) { finally_.accept(this); }
    /** */ void visit(const ForStatement forStatement) { forStatement.accept(this); }
    /** */ void visit(const ForeachStatement foreachStatement) { foreachStatement.accept(this); }
    /** */ void visit(const ForeachType foreachType) { foreachType.accept(this); }
    /** */ void visit(const ForeachTypeList foreachTypeList) { foreachTypeList.accept(this); }
    /** */ void visit(const FunctionAttribute functionAttribute) { functionAttribute.accept(this); }
    /** */ void visit(const FunctionBody functionBody) { functionBody.accept(this); }
    /** */ void visit(const FunctionCallExpression functionCallExpression) { functionCallExpression.accept(this); }
    /** */ void visit(const FunctionCallStatement functionCallStatement) { functionCallStatement.accept(this); }
    /** */ void visit(const FunctionDeclaration functionDeclaration) { functionDeclaration.accept(this); }
    /** */ void visit(const FunctionLiteralExpression functionLiteralExpression) { functionLiteralExpression.accept(this); }
    /** */ void visit(const GotoStatement gotoStatement) { gotoStatement.accept(this); }
    /** */ void visit(const IdentifierChain identifierChain) { identifierChain.accept(this); }
    /** */ void visit(const IdentifierList identifierList) { identifierList.accept(this); }
    /** */ void visit(const IdentifierOrTemplateChain identifierOrTemplateChain) { identifierOrTemplateChain.accept(this); }
    /** */ void visit(const IdentifierOrTemplateInstance identifierOrTemplateInstance) { identifierOrTemplateInstance.accept(this); }
    /** */ void visit(const IdentityExpression identityExpression) { identityExpression.accept(this); }
    /** */ void visit(const IdType idType) { }
    /** */ void visit(const IfStatement ifStatement) { ifStatement.accept(this); }
    /** */ void visit(const ImportBind importBind) { importBind.accept(this); }
    /** */ void visit(const ImportBindings importBindings) { importBindings.accept(this); }
    /** */ void visit(const ImportDeclaration importDeclaration) { importDeclaration.accept(this); }
    /** */ void visit(const ImportExpression importExpression) { importExpression.accept(this); }
    /** */ void visit(const IndexExpression indexExpression) { indexExpression.accept(this); }
    /** */ void visit(const InExpression inExpression) { inExpression.accept(this); }
    /** */ void visit(const InStatement inStatement) { inStatement.accept(this); }
    /** */ void visit(const Initialize initialize) { initialize.accept(this); }
    /** */ void visit(const Initializer initializer) { initializer.accept(this); }
    /** */ void visit(const InterfaceDeclaration interfaceDeclaration) { interfaceDeclaration.accept(this); }
    /** */ void visit(const Invariant invariant_) { invariant_.accept(this); }
    /** */ void visit(const IsExpression isExpression) { isExpression.accept(this); }
    /** */ void visit(const KeyValuePair keyValuePair) { keyValuePair.accept(this); }
    /** */ void visit(const KeyValuePairs keyValuePairs) { keyValuePairs.accept(this); }
    /** */ void visit(const LabeledStatement labeledStatement) { labeledStatement.accept(this); }
    /** */ void visit(const LambdaExpression lambdaExpression) { lambdaExpression.accept(this); }
    /** */ void visit(const LastCatch lastCatch) { lastCatch.accept(this); }
    /** */ void visit(const LinkageAttribute linkageAttribute) { linkageAttribute.accept(this); }
    /** */ void visit(const MemberFunctionAttribute memberFunctionAttribute) { memberFunctionAttribute.accept(this); }
    /** */ void visit(const MixinDeclaration mixinDeclaration) { mixinDeclaration.accept(this); }
    /** */ void visit(const MixinExpression mixinExpression) { mixinExpression.accept(this); }
    /** */ void visit(const MixinTemplateDeclaration mixinTemplateDeclaration) { mixinTemplateDeclaration.accept(this); }
    /** */ void visit(const MixinTemplateName mixinTemplateName) { mixinTemplateName.accept(this); }
    /** */ void visit(const Module module_) { module_.accept(this); }
    /** */ void visit(const ModuleDeclaration moduleDeclaration) { moduleDeclaration.accept(this); }
    /** */ void visit(const MulExpression mulExpression) { mulExpression.accept(this); }
    /** */ void visit(const NewAnonClassExpression newAnonClassExpression) { newAnonClassExpression.accept(this); }
    /** */ void visit(const NewExpression newExpression) { newExpression.accept(this); }
    /** */ void visit(const NonVoidInitializer nonVoidInitializer) { nonVoidInitializer.accept(this); }
    /** */ void visit(const Operand operand) { operand.accept(this); }
    /** */ void visit(const Operands operands) { operands.accept(this); }
    /** */ void visit(const OrExpression orExpression) { orExpression.accept(this); }
    /** */ void visit(const OrOrExpression orOrExpression) { orOrExpression.accept(this); }
    /** */ void visit(const OutStatement outStatement) { outStatement.accept(this); }
    /** */ void visit(const Parameter parameter) { parameter.accept(this); }
    /** */ void visit(const Parameters parameters) { parameters.accept(this); }
    /** */ void visit(const Postblit postblit) { postblit.accept(this); }
    /** */ void visit(const PostIncDecExpression postIncDecExpression) { postIncDecExpression.accept(this); }
    /** */ void visit(const PowExpression powExpression) { powExpression.accept(this); }
    /** */ void visit(const PragmaDeclaration pragmaDeclaration) { pragmaDeclaration.accept(this); }
    /** */ void visit(const PragmaExpression pragmaExpression) { pragmaExpression.accept(this); }
    /** */ void visit(const PreIncDecExpression preIncDecExpression) { preIncDecExpression.accept(this); }
    /** */ void visit(const PrimaryExpression primaryExpression) { primaryExpression.accept(this); }
    /** */ void visit(const Register register) { register.accept(this); }
    /** */ void visit(const RelExpression relExpression) { relExpression.accept(this); }
    /** */ void visit(const ReturnStatement returnStatement) { returnStatement.accept(this); }
    /** */ void visit(const ScopeGuardStatement scopeGuardStatement) { scopeGuardStatement.accept(this); }
    /** */ void visit(const SharedStaticConstructor sharedStaticConstructor) { sharedStaticConstructor.accept(this); }
    /** */ void visit(const SharedStaticDestructor sharedStaticDestructor) { sharedStaticDestructor.accept(this); }
    /** */ void visit(const ShiftExpression shiftExpression) { shiftExpression.accept(this); }
    /** */ void visit(const SingleImport singleImport) { singleImport.accept(this); }
    /** */ void visit(const SliceExpression sliceExpression) { sliceExpression.accept(this); }
    /** */ void visit(const Statement statement) { statement.accept(this); }
    /** */ void visit(const StatementNoCaseNoDefault statementNoCaseNoDefault) { statementNoCaseNoDefault.accept(this); }
    /** */ void visit(const StaticAssertDeclaration staticAssertDeclaration) { staticAssertDeclaration.accept(this); }
    /** */ void visit(const StaticAssertStatement staticAssertStatement) { staticAssertStatement.accept(this); }
    /** */ void visit(const StaticConstructor staticConstructor) { staticConstructor.accept(this); }
    /** */ void visit(const StaticDestructor staticDestructor) { staticDestructor.accept(this); }
    /** */ void visit(const StaticIfCondition staticIfCondition) { staticIfCondition.accept(this); }
    /** */ void visit(const StorageClass storageClass) { storageClass.accept(this); }
    /** */ void visit(const StructBody structBody) { structBody.accept(this); }
    /** */ void visit(const StructDeclaration structDeclaration) { structDeclaration.accept(this); }
    /** */ void visit(const StructInitializer structInitializer) { structInitializer.accept(this); }
    /** */ void visit(const StructMemberInitializer structMemberInitializer) { structMemberInitializer.accept(this); }
    /** */ void visit(const StructMemberInitializers structMemberInitializers) { structMemberInitializers.accept(this); }
    /** */ void visit(const SwitchStatement switchStatement) { switchStatement.accept(this); }
    /** */ void visit(const Symbol symbol) { symbol.accept(this); }
    /** */ void visit(const SynchronizedStatement synchronizedStatement) { synchronizedStatement.accept(this); }
    /** */ void visit(const TemplateAliasParameter templateAliasParameter) { templateAliasParameter.accept(this); }
    /** */ void visit(const TemplateArgument templateArgument) { templateArgument.accept(this); }
    /** */ void visit(const TemplateArgumentList templateArgumentList) { templateArgumentList.accept(this); }
    /** */ void visit(const TemplateArguments templateArguments) { templateArguments.accept(this); }
    /** */ void visit(const TemplateDeclaration templateDeclaration) { templateDeclaration.accept(this); }
    /** */ void visit(const TemplateInstance templateInstance) { templateInstance.accept(this); }
    /** */ void visit(const TemplateMixinExpression templateMixinExpression) { templateMixinExpression.accept(this); }
    /** */ void visit(const TemplateParameter templateParameter) { templateParameter.accept(this); }
    /** */ void visit(const TemplateParameterList templateParameterList) { templateParameterList.accept(this); }
    /** */ void visit(const TemplateParameters templateParameters) { templateParameters.accept(this); }
    /** */ void visit(const TemplateSingleArgument templateSingleArgument) { templateSingleArgument.accept(this); }
    /** */ void visit(const TemplateThisParameter templateThisParameter) { templateThisParameter.accept(this); }
    /** */ void visit(const TemplateTupleParameter templateTupleParameter) { templateTupleParameter.accept(this); }
    /** */ void visit(const TemplateTypeParameter templateTypeParameter) { templateTypeParameter.accept(this); }
    /** */ void visit(const TemplateValueParameter templateValueParameter) { templateValueParameter.accept(this); }
    /** */ void visit(const TemplateValueParameterDefault templateValueParameterDefault) { templateValueParameterDefault.accept(this); }
    /** */ void visit(const TernaryExpression ternaryExpression) { ternaryExpression.accept(this); }
    /** */ void visit(const ThrowStatement throwStatement) { throwStatement.accept(this); }
    /** */ void visit(const Token token) { }
    /** */ void visit(const TraitsExpression traitsExpression) { traitsExpression.accept(this); }
    /** */ void visit(const TryStatement tryStatement) { tryStatement.accept(this); }
    /** */ void visit(const Type type) { type.accept(this); }
    /** */ void visit(const Type2 type2) { type2.accept(this); }
    /** */ void visit(const TypeSpecialization typeSpecialization) { typeSpecialization.accept(this); }
    /** */ void visit(const TypeSuffix typeSuffix) { typeSuffix.accept(this); }
    /** */ void visit(const TypeidExpression typeidExpression) { typeidExpression.accept(this); }
    /** */ void visit(const TypeofExpression typeofExpression) { typeofExpression.accept(this); }
    /** */ void visit(const UnaryExpression unaryExpression) { unaryExpression.accept(this); }
    /** */ void visit(const UnionDeclaration unionDeclaration) { unionDeclaration.accept(this); }
    /** */ void visit(const Unittest unittest_) { unittest_.accept(this); }
    /** */ void visit(const VariableDeclaration variableDeclaration) { variableDeclaration.accept(this); }
    /** */ void visit(const Vector vector) { vector.accept(this); }
    /** */ void visit(const VersionCondition versionCondition) { versionCondition.accept(this); }
    /** */ void visit(const VersionSpecification versionSpecification) { versionSpecification.accept(this); }
    /** */ void visit(const WhileStatement whileStatement) { whileStatement.accept(this); }
    /** */ void visit(const WithStatement withStatement) { withStatement.accept(this); }
    /** */ void visit(const XorExpression xorExpression) { xorExpression.accept(this); }

    /** */ void visitStart(const AddExpression node) { }
    /** */ void visitStart(const AliasDeclaration node) { }
    /** */ void visitStart(const AliasInitializer node) { }
    /** */ void visitStart(const AliasThisDeclaration node) { }
    /** */ void visitStart(const AlignAttribute node) { }
    /** */ void visitStart(const AndAndExpression node) { }
    /** */ void visitStart(const AndExpression node) { }
    /** */ void visitStart(const ArgumentList node) { }
    /** */ void visitStart(const Arguments node) { }
    /** */ void visitStart(const ArrayInitializer node) { }
    /** */ void visitStart(const ArrayLiteral node) { }
    /** */ void visitStart(const ArrayMemberInitialization node) { }
    /** */ void visitStart(const AsmAddExp node) { }
    /** */ void visitStart(const AsmAndExp node) { }
    /** */ void visitStart(const AsmBrExp node) { }
    /** */ void visitStart(const AsmEqualExp node) { }
    /** */ void visitStart(const AsmExp node) { }
    /** */ void visitStart(const AsmInstruction node) { }
    /** */ void visitStart(const AsmLogAndExp node) { }
    /** */ void visitStart(const AsmLogOrExp node) { }
    /** */ void visitStart(const AsmMulExp node) { }
    /** */ void visitStart(const AsmOrExp node) { }
    /** */ void visitStart(const AsmPrimaryExp node) { }
    /** */ void visitStart(const AsmRelExp node) { }
    /** */ void visitStart(const AsmShiftExp node) { }
    /** */ void visitStart(const AsmStatement node) { }
    /** */ void visitStart(const AsmTypePrefix node) { }
    /** */ void visitStart(const AsmUnaExp node) { }
    /** */ void visitStart(const AsmXorExp node) { }
    /** */ void visitStart(const AssertExpression node) { }
    /** */ void visitStart(const AssignExpression node) { }
    /** */ void visitStart(const AssocArrayLiteral node) { }
    /** */ void visitStart(const AtAttribute node) { }
    /** */ void visitStart(const Attribute node) { }
    /** */ void visitStart(const AttributeDeclaration node) { }
    /** */ void visitStart(const AutoDeclaration node) { }
    /** */ void visitStart(const BlockStatement node) { }
    /** */ void visitStart(const BodyStatement node) { }
    /** */ void visitStart(const BreakStatement node) { }
    /** */ void visitStart(const BaseClass node) { }
    /** */ void visitStart(const BaseClassList node) { }
    /** */ void visitStart(const CaseRangeStatement node) { }
    /** */ void visitStart(const CaseStatement node) { }
    /** */ void visitStart(const CastExpression node) { }
    /** */ void visitStart(const CastQualifier node) { }
    /** */ void visitStart(const Catch node) { }
    /** */ void visitStart(const Catches node) { }
    /** */ void visitStart(const ClassDeclaration node) { }
    /** */ void visitStart(const CmpExpression node) { }
    /** */ void visitStart(const CompileCondition node) { }
    /** */ void visitStart(const ConditionalDeclaration node) { }
    /** */ void visitStart(const ConditionalStatement node) { }
    /** */ void visitStart(const Constraint node) { }
    /** */ void visitStart(const Constructor node) { }
    /** */ void visitStart(const ContinueStatement node) { }
    /** */ void visitStart(const DebugCondition node) { }
    /** */ void visitStart(const DebugSpecification node) { }
    /** */ void visitStart(const Declaration node) { }
    /** */ void visitStart(const DeclarationOrStatement node) { }
    /** */ void visitStart(const DeclarationsAndStatements node) { }
    /** */ void visitStart(const Declarator node) { }
    /** */ void visitStart(const DefaultStatement node) { }
    /** */ void visitStart(const DeleteExpression node) { }
    /** */ void visitStart(const DeleteStatement node) { }
    /** */ void visitStart(const Deprecated node) { }
    /** */ void visitStart(const Destructor node) { }
    /** */ void visitStart(const DoStatement node) { }
    /** */ void visitStart(const EnumBody node) { }
    /** */ void visitStart(const EnumDeclaration node) { }
    /** */ void visitStart(const EnumMember node) { }
    /** */ void visitStart(const EponymousTemplateDeclaration node) { }
    /** */ void visitStart(const EqualExpression node) { }
    /** */ void visitStart(const Expression node) { }
    /** */ void visitStart(const ExpressionStatement node) { }
    /** */ void visitStart(const FinalSwitchStatement node) { }
    /** */ void visitStart(const Finally node) { }
    /** */ void visitStart(const ForStatement node) { }
    /** */ void visitStart(const ForeachStatement node) { }
    /** */ void visitStart(const ForeachType node) { }
    /** */ void visitStart(const ForeachTypeList node) { }
    /** */ void visitStart(const FunctionAttribute node) { }
    /** */ void visitStart(const FunctionBody node) { }
    /** */ void visitStart(const FunctionCallExpression node) { }
    /** */ void visitStart(const FunctionCallStatement node) { }
    /** */ void visitStart(const FunctionDeclaration node) { }
    /** */ void visitStart(const FunctionLiteralExpression node) { }
    /** */ void visitStart(const GotoStatement node) { }
    /** */ void visitStart(const IdentifierChain node) { }
    /** */ void visitStart(const IdentifierList node) { }
    /** */ void visitStart(const IdentifierOrTemplateChain node) { }
    /** */ void visitStart(const IdentifierOrTemplateInstance node) { }
    /** */ void visitStart(const IdentityExpression node) { }
    /** */ void visitStart(const IdType node) { }
    /** */ void visitStart(const IfStatement node) { }
    /** */ void visitStart(const ImportBind node) { }
    /** */ void visitStart(const ImportBindings node) { }
    /** */ void visitStart(const ImportDeclaration node) { }
    /** */ void visitStart(const ImportExpression node) { }
    /** */ void visitStart(const IndexExpression node) { }
    /** */ void visitStart(const InExpression node) { }
    /** */ void visitStart(const InStatement node) { }
    /** */ void visitStart(const Initialize node) { }
    /** */ void visitStart(const Initializer node) { }
    /** */ void visitStart(const InterfaceDeclaration node) { }
    /** */ void visitStart(const Invariant  node) { }
    /** */ void visitStart(const IsExpression node) { }
    /** */ void visitStart(const KeyValuePair node) { }
    /** */ void visitStart(const KeyValuePairs node) { }
    /** */ void visitStart(const LabeledStatement node) { }
    /** */ void visitStart(const LambdaExpression node) { }
    /** */ void visitStart(const LastCatch node) { }
    /** */ void visitStart(const LinkageAttribute node) { }
    /** */ void visitStart(const MemberFunctionAttribute node) { }
    /** */ void visitStart(const MixinDeclaration node) { }
    /** */ void visitStart(const MixinExpression node) { }
    /** */ void visitStart(const MixinTemplateDeclaration node) { }
    /** */ void visitStart(const MixinTemplateName node) { }
    /** */ void visitStart(const Module node) { }
    /** */ void visitStart(const ModuleDeclaration node) { }
    /** */ void visitStart(const MulExpression node) { }
    /** */ void visitStart(const NewAnonClassExpression node) { }
    /** */ void visitStart(const NewExpression node) { }
    /** */ void visitStart(const NonVoidInitializer node) { }
    /** */ void visitStart(const Operand node) { }
    /** */ void visitStart(const Operands node) { }
    /** */ void visitStart(const OrExpression node) { }
    /** */ void visitStart(const OrOrExpression node) { }
    /** */ void visitStart(const OutStatement node) { }
    /** */ void visitStart(const Parameter node) { }
    /** */ void visitStart(const Parameters node) { }
    /** */ void visitStart(const Postblit node) { }
    /** */ void visitStart(const PostIncDecExpression node) { }
    /** */ void visitStart(const PowExpression node) { }
    /** */ void visitStart(const PragmaDeclaration node) { }
    /** */ void visitStart(const PragmaExpression node) { }
    /** */ void visitStart(const PreIncDecExpression node) { }
    /** */ void visitStart(const PrimaryExpression node) { }
    /** */ void visitStart(const Register node) { }
    /** */ void visitStart(const RelExpression node) { }
    /** */ void visitStart(const ReturnStatement node) { }
    /** */ void visitStart(const ScopeGuardStatement node) { }
    /** */ void visitStart(const SharedStaticConstructor node) { }
    /** */ void visitStart(const SharedStaticDestructor node) { }
    /** */ void visitStart(const ShiftExpression node) { }
    /** */ void visitStart(const SingleImport node) { }
    /** */ void visitStart(const SliceExpression node) { }
    /** */ void visitStart(const Statement node) { }
    /** */ void visitStart(const StatementNoCaseNoDefault node) { }
    /** */ void visitStart(const StaticAssertDeclaration node) { }
    /** */ void visitStart(const StaticAssertStatement node) { }
    /** */ void visitStart(const StaticConstructor node) { }
    /** */ void visitStart(const StaticDestructor node) { }
    /** */ void visitStart(const StaticIfCondition node) { }
    /** */ void visitStart(const StorageClass node) { }
    /** */ void visitStart(const StructBody node) { }
    /** */ void visitStart(const StructDeclaration node) { }
    /** */ void visitStart(const StructInitializer node) { }
    /** */ void visitStart(const StructMemberInitializer node) { }
    /** */ void visitStart(const StructMemberInitializers node) { }
    /** */ void visitStart(const SwitchStatement node) { }
    /** */ void visitStart(const Symbol node) { }
    /** */ void visitStart(const SynchronizedStatement node) { }
    /** */ void visitStart(const TemplateAliasParameter node) { }
    /** */ void visitStart(const TemplateArgument node) { }
    /** */ void visitStart(const TemplateArgumentList node) { }
    /** */ void visitStart(const TemplateArguments node) { }
    /** */ void visitStart(const TemplateDeclaration node) { }
    /** */ void visitStart(const TemplateInstance node) { }
    /** */ void visitStart(const TemplateMixinExpression node) { }
    /** */ void visitStart(const TemplateParameter node) { }
    /** */ void visitStart(const TemplateParameterList node) { }
    /** */ void visitStart(const TemplateParameters node) { }
    /** */ void visitStart(const TemplateSingleArgument node) { }
    /** */ void visitStart(const TemplateThisParameter node) { }
    /** */ void visitStart(const TemplateTupleParameter node) { }
    /** */ void visitStart(const TemplateTypeParameter node) { }
    /** */ void visitStart(const TemplateValueParameter node) { }
    /** */ void visitStart(const TemplateValueParameterDefault node) { }
    /** */ void visitStart(const TernaryExpression node) { }
    /** */ void visitStart(const ThrowStatement node) { }
    /** */ void visitStart(const Token node) { }
    /** */ void visitStart(const TraitsExpression node) { }
    /** */ void visitStart(const TryStatement node) { }
    /** */ void visitStart(const Type node) { }
    /** */ void visitStart(const Type2 node) { }
    /** */ void visitStart(const TypeSpecialization node) { }
    /** */ void visitStart(const TypeSuffix node) { }
    /** */ void visitStart(const TypeidExpression node) { }
    /** */ void visitStart(const TypeofExpression node) { }
    /** */ void visitStart(const UnaryExpression node) { }
    /** */ void visitStart(const UnionDeclaration node) { }
    /** */ void visitStart(const Unittest node) { }
    /** */ void visitStart(const VariableDeclaration node) { }
    /** */ void visitStart(const Vector node) { }
    /** */ void visitStart(const VersionCondition node) { }
    /** */ void visitStart(const VersionSpecification node) { }
    /** */ void visitStart(const WhileStatement node) { }
    /** */ void visitStart(const WithStatement node) { }
    /** */ void visitStart(const XorExpression node) { }

    /** */ void visitEnd(const AddExpression node) { }
    /** */ void visitEnd(const AliasDeclaration node) { }
    /** */ void visitEnd(const AliasInitializer node) { }
    /** */ void visitEnd(const AliasThisDeclaration node) { }
    /** */ void visitEnd(const AlignAttribute node) { }
    /** */ void visitEnd(const AndAndExpression node) { }
    /** */ void visitEnd(const AndExpression node) { }
    /** */ void visitEnd(const ArgumentList node) { }
    /** */ void visitEnd(const Arguments node) { }
    /** */ void visitEnd(const ArrayInitializer node) { }
    /** */ void visitEnd(const ArrayLiteral node) { }
    /** */ void visitEnd(const ArrayMemberInitialization node) { }
    /** */ void visitEnd(const AsmAddExp node) { }
    /** */ void visitEnd(const AsmAndExp node) { }
    /** */ void visitEnd(const AsmBrExp node) { }
    /** */ void visitEnd(const AsmEqualExp node) { }
    /** */ void visitEnd(const AsmExp node) { }
    /** */ void visitEnd(const AsmInstruction node) { }
    /** */ void visitEnd(const AsmLogAndExp node) { }
    /** */ void visitEnd(const AsmLogOrExp node) { }
    /** */ void visitEnd(const AsmMulExp node) { }
    /** */ void visitEnd(const AsmOrExp node) { }
    /** */ void visitEnd(const AsmPrimaryExp node) { }
    /** */ void visitEnd(const AsmRelExp node) { }
    /** */ void visitEnd(const AsmShiftExp node) { }
    /** */ void visitEnd(const AsmStatement node) { }
    /** */ void visitEnd(const AsmTypePrefix node) { }
    /** */ void visitEnd(const AsmUnaExp node) { }
    /** */ void visitEnd(const AsmXorExp node) { }
    /** */ void visitEnd(const AssertExpression node) { }
    /** */ void visitEnd(const AssignExpression node) { }
    /** */ void visitEnd(const AssocArrayLiteral node) { }
    /** */ void visitEnd(const AtAttribute node) { }
    /** */ void visitEnd(const Attribute node) { }
    /** */ void visitEnd(const AttributeDeclaration node) { }
    /** */ void visitEnd(const AutoDeclaration node) { }
    /** */ void visitEnd(const BlockStatement node) { }
    /** */ void visitEnd(const BodyStatement node) { }
    /** */ void visitEnd(const BreakStatement node) { }
    /** */ void visitEnd(const BaseClass node) { }
    /** */ void visitEnd(const BaseClassList node) { }
    /** */ void visitEnd(const CaseRangeStatement node) { }
    /** */ void visitEnd(const CaseStatement node) { }
    /** */ void visitEnd(const CastExpression node) { }
    /** */ void visitEnd(const CastQualifier node) { }
    /** */ void visitEnd(const Catch node) { }
    /** */ void visitEnd(const Catches node) { }
    /** */ void visitEnd(const ClassDeclaration node) { }
    /** */ void visitEnd(const CmpExpression node) { }
    /** */ void visitEnd(const CompileCondition node) { }
    /** */ void visitEnd(const ConditionalDeclaration node) { }
    /** */ void visitEnd(const ConditionalStatement node) { }
    /** */ void visitEnd(const Constraint node) { }
    /** */ void visitEnd(const Constructor node) { }
    /** */ void visitEnd(const ContinueStatement node) { }
    /** */ void visitEnd(const DebugCondition node) { }
    /** */ void visitEnd(const DebugSpecification node) { }
    /** */ void visitEnd(const Declaration node) { }
    /** */ void visitEnd(const DeclarationOrStatement node) { }
    /** */ void visitEnd(const DeclarationsAndStatements node) { }
    /** */ void visitEnd(const Declarator node) { }
    /** */ void visitEnd(const DefaultStatement node) { }
    /** */ void visitEnd(const DeleteExpression node) { }
    /** */ void visitEnd(const DeleteStatement node) { }
    /** */ void visitEnd(const Deprecated node) { }
    /** */ void visitEnd(const Destructor node) { }
    /** */ void visitEnd(const DoStatement node) { }
    /** */ void visitEnd(const EnumBody node) { }
    /** */ void visitEnd(const EnumDeclaration node) { }
    /** */ void visitEnd(const EnumMember node) { }
    /** */ void visitEnd(const EponymousTemplateDeclaration node) { }
    /** */ void visitEnd(const EqualExpression node) { }
    /** */ void visitEnd(const Expression node) { }
    /** */ void visitEnd(const ExpressionStatement node) { }
    /** */ void visitEnd(const FinalSwitchStatement node) { }
    /** */ void visitEnd(const Finally node) { }
    /** */ void visitEnd(const ForStatement node) { }
    /** */ void visitEnd(const ForeachStatement node) { }
    /** */ void visitEnd(const ForeachType node) { }
    /** */ void visitEnd(const ForeachTypeList node) { }
    /** */ void visitEnd(const FunctionAttribute node) { }
    /** */ void visitEnd(const FunctionBody node) { }
    /** */ void visitEnd(const FunctionCallExpression node) { }
    /** */ void visitEnd(const FunctionCallStatement node) { }
    /** */ void visitEnd(const FunctionDeclaration node) { }
    /** */ void visitEnd(const FunctionLiteralExpression node) { }
    /** */ void visitEnd(const GotoStatement node) { }
    /** */ void visitEnd(const IdentifierChain node) { }
    /** */ void visitEnd(const IdentifierList node) { }
    /** */ void visitEnd(const IdentifierOrTemplateChain node) { }
    /** */ void visitEnd(const IdentifierOrTemplateInstance node) { }
    /** */ void visitEnd(const IdentityExpression node) { }
    /** */ void visitEnd(const IdType node) { }
    /** */ void visitEnd(const IfStatement node) { }
    /** */ void visitEnd(const ImportBind node) { }
    /** */ void visitEnd(const ImportBindings node) { }
    /** */ void visitEnd(const ImportDeclaration node) { }
    /** */ void visitEnd(const ImportExpression node) { }
    /** */ void visitEnd(const IndexExpression node) { }
    /** */ void visitEnd(const InExpression node) { }
    /** */ void visitEnd(const InStatement node) { }
    /** */ void visitEnd(const Initialize node) { }
    /** */ void visitEnd(const Initializer node) { }
    /** */ void visitEnd(const InterfaceDeclaration node) { }
    /** */ void visitEnd(const Invariant  node) { }
    /** */ void visitEnd(const IsExpression node) { }
    /** */ void visitEnd(const KeyValuePair node) { }
    /** */ void visitEnd(const KeyValuePairs node) { }
    /** */ void visitEnd(const LabeledStatement node) { }
    /** */ void visitEnd(const LambdaExpression node) { }
    /** */ void visitEnd(const LastCatch node) { }
    /** */ void visitEnd(const LinkageAttribute node) { }
    /** */ void visitEnd(const MemberFunctionAttribute node) { }
    /** */ void visitEnd(const MixinDeclaration node) { }
    /** */ void visitEnd(const MixinExpression node) { }
    /** */ void visitEnd(const MixinTemplateDeclaration node) { }
    /** */ void visitEnd(const MixinTemplateName node) { }
    /** */ void visitEnd(const Module node) { }
    /** */ void visitEnd(const ModuleDeclaration node) { }
    /** */ void visitEnd(const MulExpression node) { }
    /** */ void visitEnd(const NewAnonClassExpression node) { }
    /** */ void visitEnd(const NewExpression node) { }
    /** */ void visitEnd(const NonVoidInitializer node) { }
    /** */ void visitEnd(const Operand node) { }
    /** */ void visitEnd(const Operands node) { }
    /** */ void visitEnd(const OrExpression node) { }
    /** */ void visitEnd(const OrOrExpression node) { }
    /** */ void visitEnd(const OutStatement node) { }
    /** */ void visitEnd(const Parameter node) { }
    /** */ void visitEnd(const Parameters node) { }
    /** */ void visitEnd(const Postblit node) { }
    /** */ void visitEnd(const PostIncDecExpression node) { }
    /** */ void visitEnd(const PowExpression node) { }
    /** */ void visitEnd(const PragmaDeclaration node) { }
    /** */ void visitEnd(const PragmaExpression node) { }
    /** */ void visitEnd(const PreIncDecExpression node) { }
    /** */ void visitEnd(const PrimaryExpression node) { }
    /** */ void visitEnd(const Register node) { }
    /** */ void visitEnd(const RelExpression node) { }
    /** */ void visitEnd(const ReturnStatement node) { }
    /** */ void visitEnd(const ScopeGuardStatement node) { }
    /** */ void visitEnd(const SharedStaticConstructor node) { }
    /** */ void visitEnd(const SharedStaticDestructor node) { }
    /** */ void visitEnd(const ShiftExpression node) { }
    /** */ void visitEnd(const SingleImport node) { }
    /** */ void visitEnd(const SliceExpression node) { }
    /** */ void visitEnd(const Statement node) { }
    /** */ void visitEnd(const StatementNoCaseNoDefault node) { }
    /** */ void visitEnd(const StaticAssertDeclaration node) { }
    /** */ void visitEnd(const StaticAssertStatement node) { }
    /** */ void visitEnd(const StaticConstructor node) { }
    /** */ void visitEnd(const StaticDestructor node) { }
    /** */ void visitEnd(const StaticIfCondition node) { }
    /** */ void visitEnd(const StorageClass node) { }
    /** */ void visitEnd(const StructBody node) { }
    /** */ void visitEnd(const StructDeclaration node) { }
    /** */ void visitEnd(const StructInitializer node) { }
    /** */ void visitEnd(const StructMemberInitializer node) { }
    /** */ void visitEnd(const StructMemberInitializers node) { }
    /** */ void visitEnd(const SwitchStatement node) { }
    /** */ void visitEnd(const Symbol node) { }
    /** */ void visitEnd(const SynchronizedStatement node) { }
    /** */ void visitEnd(const TemplateAliasParameter node) { }
    /** */ void visitEnd(const TemplateArgument node) { }
    /** */ void visitEnd(const TemplateArgumentList node) { }
    /** */ void visitEnd(const TemplateArguments node) { }
    /** */ void visitEnd(const TemplateDeclaration node) { }
    /** */ void visitEnd(const TemplateInstance node) { }
    /** */ void visitEnd(const TemplateMixinExpression node) { }
    /** */ void visitEnd(const TemplateParameter node) { }
    /** */ void visitEnd(const TemplateParameterList node) { }
    /** */ void visitEnd(const TemplateParameters node) { }
    /** */ void visitEnd(const TemplateSingleArgument node) { }
    /** */ void visitEnd(const TemplateThisParameter node) { }
    /** */ void visitEnd(const TemplateTupleParameter node) { }
    /** */ void visitEnd(const TemplateTypeParameter node) { }
    /** */ void visitEnd(const TemplateValueParameter node) { }
    /** */ void visitEnd(const TemplateValueParameterDefault node) { }
    /** */ void visitEnd(const TernaryExpression node) { }
    /** */ void visitEnd(const Token node) { }
    /** */ void visitEnd(const ThrowStatement node) { }
    /** */ void visitEnd(const TraitsExpression node) { }
    /** */ void visitEnd(const TryStatement node) { }
    /** */ void visitEnd(const Type node) { }
    /** */ void visitEnd(const Type2 node) { }
    /** */ void visitEnd(const TypeSpecialization node) { }
    /** */ void visitEnd(const TypeSuffix node) { }
    /** */ void visitEnd(const TypeidExpression node) { }
    /** */ void visitEnd(const TypeofExpression node) { }
    /** */ void visitEnd(const UnaryExpression node) { }
    /** */ void visitEnd(const UnionDeclaration node) { }
    /** */ void visitEnd(const Unittest node) { }
    /** */ void visitEnd(const VariableDeclaration node) { }
    /** */ void visitEnd(const Vector node) { }
    /** */ void visitEnd(const VersionCondition node) { }
    /** */ void visitEnd(const VersionSpecification node) { }
    /** */ void visitEnd(const WhileStatement node) { }
    /** */ void visitEnd(const WithStatement node) { }
    /** */ void visitEnd(const XorExpression node) { }

    void callVisit(T)(const T thing)
    {
        // Visit Token and IdType
        static if(is(T == Token) || is(T == IdType))
        {
            if(thing !is T.init)
            {
                this.visitStart(thing);
                this.visit(thing);
                this.visitEnd(thing);
            }
        }
        // Visit anything else that is not null
        else static if(!is(T == typeof(null)))
        {
            //stderr.writefln("!!! callVisit is ignoring the type %s.", typeid(T));
        }
    }

    void callVisit(const ASTNode[] unknowns)
    {
        if(unknowns is null) return;

        foreach(unknown; unknowns)
            callVisit(unknown);
    }

    void callVisit(const ASTNode unknown)
    {
        if(unknown is null) return;

        mixin(std.d.codegen.callOnActualType!(
            "this.visitStart(actual);\nthis.visit(actual);\nthis.visitEnd(actual);", 
            "throw new Exception(std.string.format(\"Unexpected ast node type: %s\", typeid(unknown)))", 
            NODE_TYPE_NAMES));
    }
}

abstract class ASTNode
{
public:
    /** */ void accept(ASTVisitor visitor) const;
}

mixin template acceptMembersIfNotNull()
{
    override void accept(ASTVisitor visitor) const
    {
        foreach (member_string; __traits(allMembers, typeof(this)))
        {
            mixin("alias member = " ~ __traits(identifier, typeof(this)) ~ "." ~ member_string ~ ";");
            static if (!is(typeof(member) == function)
                    && !is(typeof(member) == delegate)
                    && !is(member == function)
                    && !is(member == delegate)
                    && !is(member == class)
                    && !is(member == struct)
                    && !is(member == enum)
                    && __traits(identifier, member) != "Monitor")
            {
                mixin("visitor.callVisit(this." ~ member_string ~ ");\n");
            }
        }
    }
}

mixin template OpEquals()
{
    override bool opEquals(Object other) const
    {
        mixin (generateOpEquals!(typeof(this)));
    }
}

template generateOpEquals(T)
{
    template opEqualsPart(p ...)
    {
        import std.traits;
        static if (p.length == 0)
            enum opEqualsPart = "";
        else static if (!isSomeFunction!(__traits(getMember, T, p[0]))
            && p[0] != "line" && p[0] != "column" && p[0] != "startLocation"
            && p[0] != "endLocation")
        {
            static if (typeof(__traits(getMember, T, p[0])).stringof[$ - 2 .. $] == "[]")
            {
                enum opEqualsPart = "\nif (obj." ~ p[0] ~ ".length != this." ~ p[0] ~ ".length) return false;\n"
                    ~ "foreach (i; 0 .. this." ~ p[0] ~ ".length)\n"
                    ~ "\tif (this." ~ p[0] ~ "[i] != obj." ~ p[0] ~ "[i]) return false;";
            }
            else
                enum opEqualsPart = "\nif (obj." ~ p[0] ~ " != this." ~ p[0] ~ ") return false;" ~ opEqualsPart!(p[1 .. $]);
        }
        else
            enum opEqualsPart = opEqualsPart!(p[1 .. $]);
    }
    enum generateOpEquals = T.stringof ~ " obj = cast(" ~ T.stringof ~ ") other;\n"
        ~ "if (obj is null) return false;"
        ~ opEqualsPart!(__traits(derivedMembers, T)) ~ "\n"
        ~ "return true;";
}

abstract class ExpressionNode : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        assert (false);
    }
}

mixin template BinaryExpressionBody()
{
    ExpressionNode left;
    ExpressionNode right;
    size_t line;
    size_t column;
}

///
final class AddExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    mixin OpEquals;
    /** */ IdType operator;
    mixin BinaryExpressionBody;
}

///
final class AliasDeclaration : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    mixin OpEquals;
    /** */ LinkageAttribute linkageAttribute;
    /** */ Type type;
    /** */ Token name;
    /** */ AliasInitializer[] initializers;
    /** */ string comment;
}

///
final class AliasInitializer : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    mixin OpEquals;
    /** */ Token name;
    /** */ Type type;
}

///
final class AliasThisDeclaration : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    mixin OpEquals;
    /** */ Token identifier;
}

///
final class AlignAttribute : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    mixin OpEquals;
    /** */ Token intLiteral;
}

///
final class AndAndExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    mixin OpEquals;
    mixin BinaryExpressionBody;
}

///
final class AndExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    mixin OpEquals;
    mixin BinaryExpressionBody;
}

///
final class ArgumentList : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    mixin OpEquals;
    /** */ AssignExpression[] items;
}

///
final class Arguments : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    mixin OpEquals;
    /** */ ArgumentList argumentList;
}

///
final class ArrayInitializer : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    mixin OpEquals;
    /** */ ArrayMemberInitialization[] arrayMemberInitializations;
}

///
final class ArrayLiteral : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    mixin OpEquals;
    /** */ ArgumentList argumentList;
}

///
final class ArrayMemberInitialization : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    mixin OpEquals;
    /** */ AssignExpression assignExpression;
    /** */ NonVoidInitializer nonVoidInitializer;
}

///
final class AsmAddExp : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    mixin OpEquals;
    /** */ IdType operator;
    mixin BinaryExpressionBody;
}

///
final class AsmAndExp : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    mixin OpEquals;
    mixin BinaryExpressionBody;
}

///
final class AsmBrExp : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    mixin OpEquals;
    /** */ AsmBrExp asmBrExp;
    /** */ AsmEqualExp asmEqualExp;
    /** */ AsmUnaExp asmUnaExp;
}

///
final class AsmEqualExp : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    mixin BinaryExpressionBody;
    mixin OpEquals;
    /** */ Token operator;
}

///
final class AsmExp : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    mixin OpEquals;
    /** */ AsmLogOrExp left;
    /** */ AsmExp middle;
    /** */ AsmExp right;
}

///
final class AsmInstruction : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    mixin OpEquals;
    /** */ Token identifierOrIntegerOrOpcode;
    /** */ bool hasAlign;
    /** */ AsmExp asmExp;
    /** */ Operands operands;
}

///
final class AsmLogAndExp : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    mixin BinaryExpressionBody;
    mixin OpEquals;
}

///
final class AsmLogOrExp : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    mixin BinaryExpressionBody;
    mixin OpEquals;
}

///
final class AsmMulExp : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ IdType operator;
    mixin BinaryExpressionBody;
    mixin OpEquals;

}

///
final class AsmOrExp : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    mixin BinaryExpressionBody;
    mixin OpEquals;
}

///
final class AsmPrimaryExp : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ IdentifierChain identifierChain;
    /** */ Register register;
    /** */ Token token;
    mixin OpEquals;
}

///
final class AsmRelExp : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    mixin BinaryExpressionBody;
    /** */ Token operator;
    mixin OpEquals;
}

///
final class AsmShiftExp : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    mixin BinaryExpressionBody;
    /** */ Token operator;
    mixin OpEquals;
}

///
final class AsmStatement : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ AsmInstruction[] asmInstructions;
    mixin OpEquals;
}

///
final class AsmTypePrefix : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token left;
    /** */ Token right;
    mixin OpEquals;
}

///
final class AsmUnaExp : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ AsmTypePrefix asmTypePrefix;
    /** */ AsmExp asmExp;
    /** */ Token prefix;
    /** */ AsmPrimaryExp asmPrimaryExp;
    /** */ AsmUnaExp asmUnaExp;
    mixin OpEquals;
}

///
final class AsmXorExp : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    mixin BinaryExpressionBody;
    mixin OpEquals;
}

///
final class AssertExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ AssignExpression assertion;
    /** */ AssignExpression message;
    mixin OpEquals;
}

///
final class AssignExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ ExpressionNode ternaryExpression;
    /** */ ExpressionNode assignExpression;
    /** */ IdType operator;
    size_t line;
    size_t column;
    mixin OpEquals;
}

///
final class AssocArrayLiteral : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ KeyValuePairs keyValuePairs;
    mixin OpEquals;
}

///
final class AtAttribute : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ FunctionCallExpression functionCallExpression;
    /** */ ArgumentList argumentList;
    /** */ Token identifier;
    mixin OpEquals;
}

///
final class Attribute : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ LinkageAttribute linkageAttribute;
    /** */ AlignAttribute alignAttribute;
    /** */ PragmaExpression pragmaExpression;
    /** */ StorageClass storageClass;
    /** */ IdType attribute;
    mixin OpEquals;
}

///
final class AttributeDeclaration : ASTNode
{
    mixin acceptMembersIfNotNull;
    /** */ Attribute attribute;
    mixin OpEquals;
}

///
final class AutoDeclaration : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token[] identifiers;
    /** */ Initializer[] initializers;
    mixin OpEquals;
}

///
final class BlockStatement : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /**
     * Byte position of the opening brace
     */
    size_t startLocation;

    /**
     * Byte position of the closing brace
     */
    size_t endLocation;

    /** */ DeclarationsAndStatements declarationsAndStatements;
    mixin OpEquals;
}

///
final class BodyStatement : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ BlockStatement blockStatement;
    mixin OpEquals;
}

///
final class BreakStatement : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token label;
    mixin OpEquals;
}

///
final class BaseClass : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ IdentifierOrTemplateChain identifierOrTemplateChain;
    /** */ TypeofExpression typeofExpression;
    mixin OpEquals;
}

///
final class BaseClassList : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ BaseClass[] items;
    mixin OpEquals;
}

///
final class CaseRangeStatement : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ AssignExpression low;
    /** */ AssignExpression high;
    /** */ DeclarationsAndStatements declarationsAndStatements;
    mixin OpEquals;
}

///
final class CaseStatement: ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ ArgumentList argumentList;
    /** */ DeclarationsAndStatements declarationsAndStatements;
    mixin OpEquals;
}

///
final class CastExpression: ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Type type;
    /** */ CastQualifier castQualifier;
    /** */ UnaryExpression unaryExpression;
    mixin OpEquals;
}

///
final class CastQualifier: ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token first;
    /** */ Token second;
    mixin OpEquals;
}

///
final class Catches: ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Catch[] catches;
    /** */ LastCatch lastCatch;
    mixin OpEquals;
}

///
final class Catch: ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Type type;
    /** */ Token identifier;
    /** */ DeclarationOrStatement declarationOrStatement;
    mixin OpEquals;
}

///
final class ClassDeclaration: ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token name;
    /** */ TemplateParameters templateParameters;
    /** */ Constraint constraint;
    /** */ BaseClassList baseClassList;
    /** */ StructBody structBody;
    /** */ string comment;
    mixin OpEquals;
}

///
final class CmpExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ ExpressionNode shiftExpression;
    /** */ ExpressionNode equalExpression;
    /** */ ExpressionNode identityExpression;
    /** */ ExpressionNode relExpression;
    /** */ ExpressionNode inExpression;
    mixin OpEquals;
}

///
final class CompileCondition : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ VersionCondition versionCondition;
    /** */ DebugCondition debugCondition;
    /** */ StaticIfCondition staticIfCondition;
    mixin OpEquals;
}

///
final class ConditionalDeclaration : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ CompileCondition compileCondition;
    /** */ Declaration[] trueDeclarations;
    /** */ Declaration falseDeclaration;
    mixin OpEquals;
}

///
final class ConditionalStatement : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ CompileCondition compileCondition;
    /** */ DeclarationOrStatement trueStatement;
    /** */ DeclarationOrStatement falseStatement;
    mixin OpEquals;
}

///
final class Constraint : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Expression expression;
    mixin OpEquals;
}

///
final class Constructor : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Parameters parameters;
    /** */ FunctionBody functionBody;
    /** */ Constraint constraint;
    /** */ MemberFunctionAttribute[] memberFunctionAttributes;
    /** */ TemplateParameters templateParameters;
    /** */ size_t location;
    /** */ size_t line;
    /** */ size_t column;
    /** */ string comment;
    mixin OpEquals;
}

///
final class ContinueStatement : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token label;
    mixin OpEquals;
}

///
final class DebugCondition : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token identifierOrInteger;
    mixin OpEquals;
}

///
final class DebugSpecification : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token identifierOrInteger;
    mixin OpEquals;
}

///
final class Declaration : ASTNode
{
public:

    mixin acceptMembersIfNotNull;

    /** */ Attribute[] attributes;
    /** */ AttributeDeclaration attributeDeclaration;
    /** */ ImportDeclaration importDeclaration;
    /** */ FunctionDeclaration functionDeclaration;
    /** */ VariableDeclaration variableDeclaration;
    /** */ AliasThisDeclaration aliasThisDeclaration;
    /** */ StructDeclaration structDeclaration;
    /** */ ClassDeclaration classDeclaration;
    /** */ InterfaceDeclaration interfaceDeclaration;
    /** */ UnionDeclaration unionDeclaration;
    /** */ EnumDeclaration enumDeclaration;
    /** */ AliasDeclaration aliasDeclaration;
    /** */ MixinDeclaration mixinDeclaration;
    /** */ MixinTemplateDeclaration mixinTemplateDeclaration;
    /** */ Unittest unittest_;
    /** */ StaticAssertDeclaration staticAssertDeclaration;
    /** */ TemplateDeclaration templateDeclaration;
    /** */ Constructor constructor;
    /** */ Destructor destructor;
    /** */ StaticConstructor staticConstructor;
    /** */ StaticDestructor staticDestructor;
    /** */ SharedStaticDestructor sharedStaticDestructor;
    /** */ SharedStaticConstructor sharedStaticConstructor;
    /** */ ConditionalDeclaration conditionalDeclaration;
    /** */ PragmaDeclaration pragmaDeclaration;
    /** */ VersionSpecification versionSpecification;
    /** */ Invariant invariant_;
    /** */ Postblit postblit;
    /** */ Declaration[] declarations;
    mixin OpEquals;
}

///
final class DeclarationsAndStatements : ASTNode
{
    mixin acceptMembersIfNotNull;
    /** */ DeclarationOrStatement[] declarationsAndStatements;
    mixin OpEquals;
}

///
final class DeclarationOrStatement : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Declaration declaration;
    /** */ Statement statement;
    mixin OpEquals;
}

///
final class Declarator : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token name;
    /** */ Initializer initializer;
    mixin OpEquals;
}

///
final class DefaultStatement : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ DeclarationsAndStatements declarationsAndStatements;
    mixin OpEquals;
}

///
final class DeleteExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ UnaryExpression unaryExpression;
    /** */ size_t line;
    /** */ size_t column;
    mixin OpEquals;
}

///
final class DeleteStatement : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ DeleteExpression deleteExpression;
    mixin OpEquals;
}

///
final class Deprecated : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ AssignExpression assignExpression;
    mixin OpEquals;
}

///
final class Destructor : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ MemberFunctionAttribute[] memberFunctionAttributes;
    /** */ FunctionBody functionBody;
    /** */ size_t line;
    /** */ size_t column;
    /** */ size_t index;
    /** */ string comment;
    mixin OpEquals;
}

///
final class DoStatement : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
    /** */ Expression expression;
    mixin OpEquals;
}

///
final class EnumBody : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ EnumMember[] enumMembers;

    /**
     * Byte position of the opening brace
     */
    size_t startLocation;

    /**
     * Byte position of the closing brace
     */
    size_t endLocation;
    mixin OpEquals;
}

///
final class EnumDeclaration : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token name;
    /** */ Type type;
    /** */ EnumBody enumBody;
    /** */ string comment;
    mixin OpEquals;
}

///
final class EnumMember : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token name;
    /** */ Type type;
    /** */ AssignExpression assignExpression;
    /** */ string comment;
    mixin OpEquals;
}

///
final class EponymousTemplateDeclaration : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token name;
    /** */ TemplateParameters templateParameters;
    /** */ AssignExpression assignExpression;
    mixin OpEquals;
}

///
final class EqualExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ IdType operator;
    mixin BinaryExpressionBody;
    mixin OpEquals;
}

///
final class Expression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ AssignExpression[] items;
    mixin OpEquals;
}

///
final class ExpressionStatement : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Expression expression;
    mixin OpEquals;
}

///
final class FinalSwitchStatement : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ SwitchStatement switchStatement;
    mixin OpEquals;
}

///
final class Finally : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ DeclarationOrStatement declarationOrStatement;
    mixin OpEquals;
}

///
final class ForStatement : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ DeclarationOrStatement initialization;
    /** */ ExpressionStatement test;
    /** */ Expression increment;
    /** */ DeclarationOrStatement declarationOrStatement;
    /** */ size_t startIndex;
    mixin OpEquals;
}

///
final class ForeachStatement : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ IdType type;
    /** */ ForeachTypeList foreachTypeList;
    /** */ ForeachType foreachType;
    /** */ Expression low;
    /** */ Expression high;
    /** */ DeclarationOrStatement declarationOrStatement;
    /** */ size_t startIndex;
    mixin OpEquals;
}

///
final class ForeachType : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ IdType[] typeConstructors;
    /** */ Type type;
    /** */ Token identifier;
    mixin OpEquals;
}

///
final class ForeachTypeList : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ ForeachType[] items;
    mixin OpEquals;
}

///
final class FunctionAttribute : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token token;
    /** */ AtAttribute atAttribute;
    mixin OpEquals;
}

///
final class FunctionBody : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ BlockStatement blockStatement;
    /** */ BodyStatement bodyStatement;
    /** */ OutStatement outStatement;
    /** */ InStatement inStatement;
    mixin OpEquals;
}

///
final class FunctionCallExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Type type;
    /** */ UnaryExpression unaryExpression;
    /** */ TemplateArguments templateArguments;
    /** */ Arguments arguments;
    mixin OpEquals;
}

///
final class FunctionCallStatement : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ FunctionCallExpression functionCallExpression;
    mixin OpEquals;
}

///
final class FunctionDeclaration : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ bool hasAuto;
    /** */ bool hasRef;
    /** */ Type returnType;
    /** */ Token name;
    /** */ TemplateParameters templateParameters;
    /** */ Parameters parameters;
    /** */ Constraint constraint;
    /** */ FunctionBody functionBody;
    /** */ MemberFunctionAttribute[] memberFunctionAttributes;
    /** */ string comment;
    mixin OpEquals;
}

///
final class FunctionLiteralExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ IdType functionOrDelegate;
    /** */ Type type;
    /** */ Parameters parameters;
    /** */ FunctionAttribute[] functionAttributes;
    /** */ FunctionBody functionBody;
    mixin OpEquals;
}

///
final class GotoStatement : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Expression expression;
    /** */ Token label;
    mixin OpEquals;
}

///
final class IdentifierChain : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token[] identifiers;
    mixin OpEquals;
}

///
final class IdentifierList : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token[] identifiers;
    mixin OpEquals;
}

///
final class IdentifierOrTemplateChain : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ IdentifierOrTemplateInstance[] identifiersOrTemplateInstances;
    mixin OpEquals;
}

///
final class IdentifierOrTemplateInstance : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token identifier;
    /** */ TemplateInstance templateInstance;
    mixin OpEquals;
}

///
final class IdentityExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ bool negated;
    mixin BinaryExpressionBody;
    mixin OpEquals;
}

///
final class IfStatement : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token identifier;
    /** */ Type type;
    /** */ Expression expression;
    /** */ DeclarationOrStatement thenStatement;
    /** */ DeclarationOrStatement elseStatement;
    /** */ size_t startIndex;
    /** */ size_t line;
    /** */ size_t column;
    mixin OpEquals;
}

///
final class ImportBind : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token left;
    /** */ Token right;
    mixin OpEquals;
}

///
final class ImportBindings : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ SingleImport singleImport;
    /** */ ImportBind[] importBinds;
    mixin OpEquals;
}

///
final class ImportDeclaration : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ SingleImport[] singleImports;
    /** */ ImportBindings importBindings;
    mixin OpEquals;
}

///
final class ImportExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ AssignExpression assignExpression;
    mixin OpEquals;
}

///
final class IndexExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ UnaryExpression unaryExpression;
    /** */ ArgumentList argumentList;
    mixin OpEquals;
}

///
final class InExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    mixin BinaryExpressionBody;
    bool negated;
    mixin OpEquals;
}

///
final class InStatement : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ BlockStatement blockStatement;
    mixin OpEquals;
}

///
final class Initialize : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
    mixin OpEquals;
}

///
final class Initializer : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ NonVoidInitializer nonVoidInitializer;
    mixin OpEquals;
}

///
final class InterfaceDeclaration : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token name;
    /** */ TemplateParameters templateParameters;
    /** */ Constraint constraint;
    /** */ BaseClassList baseClassList;
    /** */ StructBody structBody;
    /** */ string comment;
    mixin OpEquals;
}

///
final class Invariant : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ BlockStatement blockStatement;
    /** */ string comment;
    size_t line;
    size_t index;
    mixin OpEquals;
}

///
final class IsExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Type type;
    /** */ Token identifier;
    /** */ TypeSpecialization typeSpecialization;
    /** */ TemplateParameterList templateParameterList;
    /** */ IdType equalsOrColon;
    mixin OpEquals;
}

///
final class KeyValuePair : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ AssignExpression key;
    /** */ AssignExpression value;
    mixin OpEquals;
}

///
final class KeyValuePairs : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ KeyValuePair[] keyValuePairs;
    mixin OpEquals;
}

///
final class LabeledStatement : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    Token identifier;
    /** */ DeclarationOrStatement declarationOrStatement;
    mixin OpEquals;
}

///
final class LambdaExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ IdType functionType;
    /** */ Token identifier;
    /** */ Parameters parameters;
    /** */ FunctionAttribute[] functionAttributes;
    /** */ AssignExpression assignExpression;
    mixin OpEquals;
}

///
final class LastCatch : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
    mixin OpEquals;
}

///
final class LinkageAttribute : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token identifier;
    /** */ bool hasPlusPlus;
    version (DIP61) /** */ IdentifierChain identifierChain;
    mixin OpEquals;
}

///
final class MemberFunctionAttribute : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ IdType tokenType;
    /** */ AtAttribute atAttribute;
    mixin OpEquals;
}

///
final class MixinDeclaration : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ MixinExpression mixinExpression;
    /** */ TemplateMixinExpression templateMixinExpression;
    mixin OpEquals;
}

///
final class MixinExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ AssignExpression assignExpression;
    mixin OpEquals;
}

///
final class MixinTemplateDeclaration : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ TemplateDeclaration templateDeclaration;
    mixin OpEquals;
}

///
final class MixinTemplateName : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Symbol symbol;
    /** */ IdentifierOrTemplateChain identifierOrTemplateChain;
    /** */ TypeofExpression typeofExpression;
    mixin OpEquals;
}

///
final class Module : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
	/** */ Token scriptLine;
    /** */ ModuleDeclaration moduleDeclaration;
    /** */ Declaration[] declarations;
    mixin OpEquals;
}

///
final class ModuleDeclaration : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ IdentifierChain moduleName;
    mixin OpEquals;
}


///
final class MulExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ IdType operator;
    mixin BinaryExpressionBody;
    mixin OpEquals;
}

///
final class NewAnonClassExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Arguments allocatorArguments;
    /** */ Arguments constructorArguments;
    /** */ BaseClassList baseClassList;
    /** */ StructBody structBody;
    mixin OpEquals;
}

///
final class NewExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Type type;
    /** */ NewAnonClassExpression newAnonClassExpression;
    /** */ Arguments arguments;
    /** */ AssignExpression assignExpression;
    mixin OpEquals;
}


///
final class StatementNoCaseNoDefault : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ LabeledStatement labeledStatement;
    /** */ BlockStatement blockStatement;
    /** */ IfStatement ifStatement;
    /** */ WhileStatement whileStatement;
    /** */ DoStatement doStatement;
    /** */ ForStatement forStatement;
    /** */ ForeachStatement foreachStatement;
    /** */ SwitchStatement switchStatement;
    /** */ FinalSwitchStatement finalSwitchStatement;
    /** */ ContinueStatement continueStatement;
    /** */ BreakStatement breakStatement;
    /** */ ReturnStatement returnStatement;
    /** */ GotoStatement gotoStatement;
    /** */ WithStatement withStatement;
    /** */ SynchronizedStatement synchronizedStatement;
    /** */ TryStatement tryStatement;
    /** */ ThrowStatement throwStatement;
    /** */ ScopeGuardStatement scopeGuardStatement;
    /** */ AsmStatement asmStatement;
    /** */ ConditionalStatement conditionalStatement;
    /** */ StaticAssertStatement staticAssertStatement;
    /** */ VersionSpecification versionSpecification;
    /** */ DebugSpecification debugSpecification;
    /** */ FunctionCallStatement functionCallStatement;
    /** */ ExpressionStatement expressionStatement;
    mixin OpEquals;
}

///
final class NonVoidInitializer : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ AssignExpression assignExpression;
    /** */ ArrayInitializer arrayInitializer;
    /** */ StructInitializer structInitializer;
    /** */ FunctionBody functionBody;

    mixin OpEquals;
}

///
final class Operand : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ AsmExp asmExp;
    mixin OpEquals;
}

///
final class Operands : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Operand[] operands;
    mixin OpEquals;
}

///
final class OrExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    mixin BinaryExpressionBody;
    mixin OpEquals;
}

///
final class OrOrExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    mixin BinaryExpressionBody;
    mixin OpEquals;
}

///
final class OutStatement : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token parameter;
    /** */ BlockStatement blockStatement;
    mixin OpEquals;
}

///
final class Parameter : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ IdType[] parameterAttributes;
    /** */ Type type;
    /** */ Token name;
    /** */ bool vararg;
    /** */ AssignExpression default_;
    mixin OpEquals;
}

///
final class Parameters : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Parameter[] parameters;
    /** */ bool hasVarargs;
    mixin OpEquals;
}

///
final class Postblit : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ FunctionBody functionBody;
    /** */ MemberFunctionAttribute[] memberFunctionAttributes;
    mixin OpEquals;
}

///
final class PostIncDecExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ IdType operator;
    /** */ UnaryExpression unaryExpression;
    mixin OpEquals;
}

///
final class PowExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    mixin BinaryExpressionBody;
    mixin OpEquals;
}

///
final class PragmaDeclaration : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ PragmaExpression pragmaExpression;
    mixin OpEquals;
}

///
final class PragmaExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token identifier;
    /** */ ArgumentList argumentList;
    mixin OpEquals;
}

///
final class PreIncDecExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ IdType operator;
    /** */ UnaryExpression unaryExpression;
    mixin OpEquals;
}

///
final class PrimaryExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token dot;
    /** */ Token primary;
    /** */ IdentifierOrTemplateInstance identifierOrTemplateInstance;
    /** */ Token basicType;
    /** */ TypeofExpression typeofExpression;
    /** */ TypeidExpression typeidExpression;
    /** */ ArrayLiteral arrayLiteral;
    /** */ AssocArrayLiteral assocArrayLiteral;
    /** */ Expression expression;
    /** */ IsExpression isExpression;
    /** */ LambdaExpression lambdaExpression;
    /** */ FunctionLiteralExpression functionLiteralExpression;
    /** */ TraitsExpression traitsExpression;
    /** */ MixinExpression mixinExpression;
    /** */ ImportExpression importExpression;
    /** */ Vector vector;
    mixin OpEquals;
}

///
final class Register : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token identifier;
    /** */ Token intLiteral;
    /** */ bool hasIntegerLiteral;
    mixin OpEquals;
}

///
final class RelExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ IdType operator;
    mixin BinaryExpressionBody;
    mixin OpEquals;
}

///
final class ReturnStatement : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Expression expression;
    /** */ size_t startLocation;
    /** */ size_t endLocation;
    mixin OpEquals;
}

///
final class ScopeGuardStatement : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token identifier;
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
    mixin OpEquals;
}

///
final class SharedStaticConstructor : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ FunctionBody functionBody;
    /** */ size_t location;
    /** */ string comment;
    mixin OpEquals;
}

///
final class SharedStaticDestructor : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ FunctionBody functionBody;
    /** */ size_t location;
    /** */ string comment;
    mixin OpEquals;
}

///
final class ShiftExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ IdType operator;
    mixin BinaryExpressionBody;
    mixin OpEquals;
}

///
final class SingleImport : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token rename;
    /** */ IdentifierChain identifierChain;
    mixin OpEquals;
}

///
final class SliceExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ UnaryExpression unaryExpression;
    /** */ AssignExpression lower;
    /** */ AssignExpression upper;
    mixin OpEquals;
}

///
final class Statement : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
    /** */ CaseStatement caseStatement;
    /** */ CaseRangeStatement caseRangeStatement;
    /** */ DefaultStatement defaultStatement;
    mixin OpEquals;
}

///
final class StaticAssertDeclaration : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ StaticAssertStatement staticAssertStatement;
    mixin OpEquals;
}

///
final class StaticAssertStatement : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ AssertExpression assertExpression;
    mixin OpEquals;
}

///
final class StaticConstructor : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ FunctionBody functionBody;
    /** */ size_t location;
    /** */ string comment;
    mixin OpEquals;
}

///
final class StaticDestructor : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ FunctionBody functionBody;
    /** */ size_t location;
    /** */ string comment;
    mixin OpEquals;
}

///
final class StaticIfCondition : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ AssignExpression assignExpression;
    mixin OpEquals;
}

///
final class StorageClass : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ AtAttribute atAttribute;
    /** */ Deprecated deprecated_;
    /** */ Token token;
    mixin OpEquals;
}

///
final class StructBody : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /**
     * Byte position of the opening brace
     */
    size_t startLocation;

    /**
     * Byte position of the closing brace
     */
    size_t endLocation;
    /** */ Declaration[] declarations;
    mixin OpEquals;
}

///
final class StructDeclaration : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token name;
    /** */ TemplateParameters templateParameters;
    /** */ Constraint constraint;
    /** */ StructBody structBody;
    /** */ string comment;
    mixin OpEquals;
}

///
final class StructInitializer : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ StructMemberInitializers structMemberInitializers;

    mixin OpEquals;
}

///
final class StructMemberInitializer : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token identifier;
    /** */ NonVoidInitializer nonVoidInitializer;
    mixin OpEquals;
}

///
final class StructMemberInitializers : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ StructMemberInitializer[] structMemberInitializers;
    mixin OpEquals;
}

///
final class SwitchStatement : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Expression expression;
    /** */ Statement statement;
    mixin OpEquals;
}

///
final class Symbol : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ IdentifierOrTemplateChain identifierOrTemplateChain;
    /** */ bool dot;
    mixin OpEquals;
}

///
final class SynchronizedStatement : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Expression expression;
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
    mixin OpEquals;
}

///
final class TemplateAliasParameter : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Type type;
    /** */ Token identifier;
    /** */ Type colonType;
    /** */ AssignExpression colonExpression;
    /** */ Type assignType;
    /** */ AssignExpression assignExpression;
    mixin OpEquals;
}

///
final class TemplateArgument : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Type type;
    /** */ AssignExpression assignExpression;
    mixin OpEquals;
}

///
final class TemplateArgumentList : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ TemplateArgument[] items;
    mixin OpEquals;
}

///
final class TemplateArguments : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ TemplateArgumentList templateArgumentList;
    /** */ TemplateSingleArgument templateSingleArgument;
    mixin OpEquals;
}

///
final class TemplateDeclaration : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token name;
    /** */ TemplateParameters templateParameters;
    /** */ Constraint constraint;
    /** */ Declaration[] declarations;
    /** */ EponymousTemplateDeclaration eponymousTemplateDeclaration;
    /** */ string comment;
    mixin OpEquals;
}

///
final class TemplateInstance : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token identifier;
    /** */ TemplateArguments templateArguments;
    mixin OpEquals;
}

///
final class TemplateMixinExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token identifier;
    /** */ TemplateArguments templateArguments;
    /** */ MixinTemplateName mixinTemplateName;
    mixin OpEquals;
}

///
final class TemplateParameter : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ TemplateTypeParameter templateTypeParameter;
    /** */ TemplateValueParameter templateValueParameter;
    /** */ TemplateAliasParameter templateAliasParameter;
    /** */ TemplateTupleParameter templateTupleParameter;
    /** */ TemplateThisParameter templateThisParameter;
    mixin OpEquals;
}

///
final class TemplateParameterList : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ TemplateParameter[] items;
    mixin OpEquals;
}

///
final class TemplateParameters : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ TemplateParameterList templateParameterList;
    mixin OpEquals;
}

///
final class TemplateSingleArgument : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token token;
    mixin OpEquals;
}

///
final class TemplateThisParameter : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ TemplateTypeParameter templateTypeParameter;
    mixin OpEquals;
}

///
final class TemplateTupleParameter : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token identifier;
    mixin OpEquals;
}

///
final class TemplateTypeParameter : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token identifier;
    /** */ Type colonType;
    /** */ Type assignType;
    mixin OpEquals;
}

///
final class TemplateValueParameter : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Type type;
    /** */ Token identifier;
    /** */ Expression expression;
    /** */ TemplateValueParameterDefault templateValueParameterDefault;
    mixin OpEquals;
}

///
final class TemplateValueParameterDefault : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ AssignExpression assignExpression;
    /** */ Token token;
    mixin OpEquals;
}

///
final class TernaryExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ ExpressionNode orOrExpression;
    /** */ ExpressionNode expression;
    /** */ ExpressionNode ternaryExpression;
    mixin OpEquals;
}

///
final class ThrowStatement : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Expression expression;
    mixin OpEquals;
}

///
final class TraitsExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token identifier;
    /** */ TemplateArgumentList templateArgumentList;
    mixin OpEquals;
}

///
final class TryStatement : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ DeclarationOrStatement declarationOrStatement;
    /** */ Catches catches;
    /** */ Finally finally_;
    mixin OpEquals;
}

///
final class Type : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ IdType[] typeConstructors;
    /** */ TypeSuffix[] typeSuffixes;
    /** */ Type2 type2;
    mixin OpEquals;
}

///
final class Type2 : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ IdType builtinType;
    /** */ Symbol symbol;
    /** */ TypeofExpression typeofExpression;
    /** */ IdentifierOrTemplateChain identifierOrTemplateChain;
    /** */ IdType typeConstructor;
    /** */ Type type;
    mixin OpEquals;
}

///
final class TypeSpecialization : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token token;
    /** */ Type type;
    mixin OpEquals;
}

///
final class TypeSuffix : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token delegateOrFunction;
    /** */ bool star;
    /** */ bool array;
    /** */ Type type;
    /** */ AssignExpression low;
    /** */ AssignExpression high;
    /** */ Parameters parameters;
    /** */ MemberFunctionAttribute[] memberFunctionAttributes;
    mixin OpEquals;
}

///
final class TypeidExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Type type;
    /** */ Expression expression;
    mixin OpEquals;
}

///
final class TypeofExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Expression expression;
    /** */ Token return_;
    mixin OpEquals;
}

///
final class UnaryExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Type type;
    /** */ PrimaryExpression primaryExpression;
    /** */ Token prefix;
    /** */ Token suffix;
    /** */ UnaryExpression unaryExpression;
    /** */ NewExpression newExpression;
    /** */ DeleteExpression deleteExpression;
    /** */ CastExpression castExpression;
    /** */ FunctionCallExpression functionCallExpression;
    /** */ ArgumentList argumentList;
    /** */ IdentifierOrTemplateInstance identifierOrTemplateInstance;
    /** */ AssertExpression assertExpression;
    /** */ SliceExpression sliceExpression;
    /** */ IndexExpression indexExpression;
    mixin OpEquals;
}

///
final class UnionDeclaration : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token name;
    /** */ TemplateParameters templateParameters;
    /** */ Constraint constraint;
    /** */ StructBody structBody;
    /** */ string comment;
    mixin OpEquals;
}

///
final class Unittest : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ BlockStatement blockStatement;
    /** */ string comment;
    mixin OpEquals;
}

///
final class VariableDeclaration : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Type type;
    /** */ Declarator[] declarators;
    /** */ StorageClass storageClass;
    /** */ AutoDeclaration autoDeclaration;
    /** */ string comment;
    mixin OpEquals;
}

///
final class Vector : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Type type;
    mixin OpEquals;
}

///
final class VersionCondition : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token token;
    mixin OpEquals;
}

///
final class VersionSpecification : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Token token;
    mixin OpEquals;
}

///
final class WhileStatement : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Expression expression;
    /** */ DeclarationOrStatement declarationOrStatement;
    /** */ size_t startIndex;
    mixin OpEquals;
}

///
final class WithStatement : ASTNode
{
public:
    mixin acceptMembersIfNotNull;
    /** */ Expression expression;
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
    mixin OpEquals;
}

///
final class XorExpression : ExpressionNode
{
public:
    mixin acceptMembersIfNotNull;
    mixin BinaryExpressionBody;
    mixin OpEquals;
}

immutable string NODE_TYPE_NAMES = std.d.codegen.typeNames!(
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
);

