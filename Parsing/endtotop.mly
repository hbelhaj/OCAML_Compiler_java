%{
    open Lexing
    open AST
}%

(*les tokens*)

(* Separators *)

%token EOF

%token TPOINT (* ... *)

%token LBRACE, RBRACE (*{ , } *)

%token CROCHETG, CROCHETD (* [ , ] *)

%token PARG, PARD (*  ( , ) *)

(*delimiters*)

%token SEMI COL DOT COMM QM (* ; : . , ? *)

(*math unary*)

%token INCREMENT DECREMENT

(*math Binary*)

%token PLUS MINUS DIV MUL MOD

(*bitwise*)

%token BAND BOR XOR BNOT LSHIFT RSHIFT LOGSHIFT (* & | ^ ~ << >> >>> *)

(*logical op*)

%token EQUAL NEQUAL GTHAN LTHAN GETHAN LETHAN AND OR NOT 

(*assignment op*)

%token ASSIGN PLUSEQUAL MINUSEQUAL MULEQUAL DIVEQUAL MODEQUAL OREQUAL XOREQUAL LSHIFTEQUAL RSHIFTEQUAL LOGSHIFTEQUAL 

(*keywords*)

%token CLASS INTERFACE ENUM EXTENDS IMPLEMENTS INSTANCEOF NATIVE NEW SUPER THIS

%token TRY CATCH FINALLY CASE  CONTINUE DEFAULT DO SYNCHRONIZED 

%token PUBLIC PRIVATE PROTECTED STATIC ABSTRACT VOLATILE TRANSIENT ASSERT FINAL STRICTFP

%token VOID

%token BYTE LONG INT DOUBLE FLOAT SHORT CHAR BOOLEAN CONST

%token IMPORT PACKAGE

%token IF ELSE RETURN SWITCH

%token AT GOTO

%token THROW THROWS

%token TRUE FALSE

%token FOR WHILE BREAK

(* literals and identifiers *)

%token <string> IDENTIFIER

%token <string> STRINGLIT

%token <int> INTLIT

%token <float> FLOATLIT

%token <float> DOUBLELIT

%token <int> LONGLIT

%token <char> CHARLIT

%token <bool> BOOLEANLIT

%token NULLLIT

 

(*priorities *)

%right ASSIGN PLUSEQUAL MINUSEQUAL MULEQUAL DIVEQUAL MODEQUAL OREQUAL XOREQUAL LSHIFTEQUAL RSHIFTEQUAL LOGSHIFTEQUAL

%left OR

%left AND

%left EQUAL NEQUAL

%left GTHAN GETHAN LTHAN LETHAN

%left PLUS MINUS

%left MUL DIV MOD

%right NOT

%left DOT




%start compilationUnit

%type <AST.fileType> compilationUnit

%%

compilationUnit:
  | name=packageDec? import=importDec? liste=typeDec? EOF { FileType({packagename=packname; listImport=imp; listClass=liste; })}



classModifier:
    | STRICTFP {Strictfp}
    | PUBLIC {Public}
    | PRIVATE {Private}
    | PROTECTED {Protected}
    | ABSTRACT {Abstract}
    | STATIC {Static}
    | TRANSIENT {Transient}

block:
    | LBRACE blockStatements RBRACE {$2}

blockStatements:
    | {}
    | blockStatements blockStatement {}

blockStatement:
    | localVariableDeclarationStatement {}
    | classOrInterfaceDeclaration {}
    | statement {}
    | identifier COL statement {}

localVariableDeclarationStatement:
    | type variableDeclarators SEMI {}
    | FINAL type variableDeclarators SEMI {}

statement:
    | block {}
    | ASSERT expression SEMI {}
    | ASSERT expression COL expression SEMI {}
    | IF parExpression statement {}
    | IF parExpression statement ELSE statement {}
    | FOR PARG forControl PARD statement {}
    | WHILE parExpression statement {}
    | DO statement WHILE parExpression SEMI {}
	| TRY block catches block {} 
	| TRY block FINALLY block {}
	| TRY block catches FINALLY block {}
	| SWITCH parExpression {}
	| SWITCH parExpression switchBlockStatementGroups {}
	| SYNCHRONIZED parExpression block {}
	| RETURN SEMI {}
	| RETURN expression SEMI {}
	| THROW expression SEMI {}
	| BREAK {}
	| BREAK identifier (*identifier a verif ? IDENTIFIER*) {}
	| CONTINUE {}
	| CONTINUE identifier {}
	| SEMI {}
	| statementexpression SEMI {}
	| identifier COL statement {}


catches:
	|catchClause {}
	|catches catchClause {}

catchClause:
	|CATCH PARG formalParameter PARD block {}

switchBlockStatementGroups:
	|switchBlockStatementGroups switchBlockStatementGroup {}
	|{}

switchBlockStatementGroup:
	|switchLabel blockStatements {}

switchLabel:
	|CASE constantExpression COL {}
	|CASE enumConstantName COL {}
	|DEFAULT COL {}

moreStatementExpressions:
	|{}
	|moreStatementExpressions COMM statementExpression  {}

forControl:
	|forVarControl {}
	|forInit SEMI expression SEMI forUpdate {}
	|forInit SEMI expression SEMI {}
	|forInit SEMI  SEMI forUpdate {}
	|forInit SEMI  SEMI {}

forVarControl:
	|type identifier forVarControlRest {}
	|FINAL type identifier forVarControlRest {}
	|FINAL annotations type identifier forVarControlRest {}
	|annotations type identifier forVarControlRest {}

annotations:
	|annotation {}
	|annotation annotations {}

annotation: 
	|AT typeName {}
	|AT typeName PARG identifier EQUAL elementValue PARD{}
	|AT typeName PARG elementValue PARD{}

elementValue:
	|conditionalExpression {}
	|annotation {}
	|elementValueArrayInitializer {}

conditionalExpression:
	|expression2 expression1Rest {}

elementValueArrayInitializer:
	| {}
	|elementValueArrayInitialize elementValues COMMA {}
	|elementValueArrayInitialize elementValues {}
	|elementValueArrayInitialize COMMA {}


elementValues:
	|elementValue {}
	|elementValue elementValues  {}	

forVarControlRest:
	|variableDeclaratorsRest SEMI  SEMI {}
	|variableDeclaratorsRest SEMI expression SEMI forUpdate {}
	|variableDeclaratorsRest SEMI  SEMI forUpdate {}
	|COL expression  {}

forInit:
	|statementExpression expressions  {}

modifier:
	|annotation  {}
	|PUBLIC {Public}
	|PROTECTED {Protected}
	|PRIVATE {Private}
	|STATIC {Static}
	|ABSTRACT {Abstract}
	|FINAL {Final}
	|NATIVE {Native}
	|SYNCHRONIZED {Synchronized}
	|TRANSIENT {Transient}
	|VOLATILE {Volatile}
	|STRICTFP {Strictfp}

(********************** I START HERE ***********)
enumConstantName:
	|identifier {}

methodBody:
	|block {}

formalParameterDeclsRest:
	|variableDeclaratorId COMMA formalParameterDecls {}
	|variableDeclaratorId {}
	|TPOINT variableDeclaratorID {}

formalParameterDecls: (* weird a voir "]"? *)
	|FINAL ttype formalParameterDeclsRest {}
	|FINAL annotations formalParameterDeclsRest {}
	|anotations formalParameterDeclsRest {}
	|formalParameterDeclsRest {}

formalParameters:
	| PARG PARD {} 
	| PARG formalParameterDecls PARD {}

qualifiedIdentifierList:
	|qualifiedIdentifier {}
	|qualifiedIdentifierList COMMA qualifiedIdentifier {}

constructorDeclaratorRest:
	|formalParameters methodBody {}
	|formalParameters THROWS qualifiedIdentifierList methodBody {}

voidInterfaceMethodDeclaratorRest:
	|formalParameters SEMI {}
	|formalParameters THROWS qualifiedIdentifierList {}

interfaceGenericMethodDecl:
	|typeParameters ttype identifier interfaceMethodDeclaratorRest {}
	|typeParameters VOID identifier interfaceMethodDeclaratorRest {}

interfaceMethodDeclaratorRest:
	|formalParameters bracketGen SEMI {}
	|formalParameters bracketGen THROWS qualifiedIdentifierList SEMI {}

bracketGen:
	| {}
	|bracketGen CROCHETG CROCHETD {}

voidMethodDeclaratorRest:
	|formalParameters SEMI {}
	|formalParameters methodBody {}
	|formalParameters THROWS qualifiedIdentifierList methodBody {}
	|formalParameters THROWS qualifiedIdentifierList SEMI {}

methodDeclaratorRest:
	|formalParameters bracketGen methodBody {}
	|formalParameters bracketGen SEMI {}
	|formalParameters bracketGen THROWS qualifiedIdentifierList methodBody {}
	|formalParameters bracketGen THROWS qualifiedIdentifierList SEMI {}


interfaceMethodOrFieldRest:
	|constantDeclaratorsRest SEMI {}
	|interfaceMethodDeclaratorRest {}

interfaceMethodOrFieldDecl:
	|ttype identifier interfaceMethodOrFieldRest {}

interfaceMemberDecl:
	|interfaceMethodOrFieldDecl {}
	|interfaceGenericMethodDecl {}
	|VOID identifier voidInterfaceMethodDeclaratorRest {}
	|interfaceDeclaration {}
	|classDeclaration {}

interfaceBodyDeclaration:
	|SEMI {}
	|modifierGen interfaceMemberDecl {}

modifierGen:
	|{}
	|modifierGen modifier {}


methodOrFieldRest:
	|variableDeclaratorRest {}
	|methodDeclaratorRest {}

methodOrFieldDecl:
	|ttype identifier methodOrFieldRest {}

genericMethodOrConstructorRest:
	|ttype identifier methodDeclaratorRest {}
	|VOID identifier methodDeclaratorRest {}
	|identifier constructorDeclaratorRest {}

genericMethodOrConstructorDecl:
	|typeParameters genericMethodOrConstructorRest {}




memberDecl:
	|genericMethodOrConstructorDecl {}
	|methodOrFieldDecl {}
	|VOID identifier voidMethodDeclaratorRest {}
	|identifier constructorDeclaratorRest {}
	|interfaceDeclaration {}
	|classDeclaration {}

classBodyDeclaration:
	|SEMI {}
	|block {}
	|STATIC block {}
	|memberDecl {}
	|modifier memberDecl {}

interfaceBody:
	|LBRACE interfaceBodyDeclarationGen RBRACE {}

interfaceBodyDeclarationGen:
	|{}
	|interfaceBodyDeclarationGen interfarceBodyDeclaration {}


classBody:
	|LBRACE classBodyDeclarationGen RBRACE {}

classBodyDeclarationGen:
	|{}
	|classBodyDeclarationGen classBodyDeclaration {}

defaultValue:
	|DEFAULT elementValue {}

annotationConstantRest:
	|variableDeclarators {}

annotationMethodRest:
	|PARG PARD {}
	|PARG PARD defaultValue {}

annotationMethodOrConstantRest:
	|annotationMethodRest {}
	|annotationConstantRest {}

annotationTypeElementRest:
	|ttype identifier annotationMethodOrConstantRest SEMI {}
	|classDeclaration {}
	|interfaceDeclaration{}
	|enumDeclaration{}
	|annotationTypeDeclaration{}
	
annotationTypeElementDeclaration:
	|modifierGen annotationTypeElementRest {}

annotationTypeElementDeclarations:
	|annotationTypeElementDeclaration {}
	|annotationTypeElementDeclarations annotationTypeElementDeclaration {} 

importDeclaration:
    | IMPORT str SEMI {{ $2 ; static=false }} 
    | IMPORT STATIC str SEMI {{ $3; static=true }}





