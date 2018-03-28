(*
    open Lexing
    open AST
*)

(*les tokens*)

(* Separators *)

%token EOF

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

%token CAND

(*assignment op*)

%token ASSIGN PLUSEQUAL MINUSEQUAL MULEQUAL DIVEQUAL MODEQUAL OREQUAL XOREQUAL LSHIFTEQUAL RSHIFTEQUAL LOGSHIFTEQUAL INC DEC

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



%token SHRR IDENT TPOINT
 

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




%%




expression:
    |expression1 {}
    |expression1 assignmentOperator expression1 {}

assignmentOperator: (*a completer *)
    |EQUAL {Equal}
    |PLUSEQUAL {Plusequal}
    |MINUSEQUAL {Minusequal}
    |MULEQUAL {Mulequal}
    |DIVEQUAL {Divequal}


ttypeGenOneElement:
    |{}
    |DOT identifier {}
    |DOT identifier typeArguments {}

ttypeGen:
    |{}
    |ttypeGen ttypeGenOneElement {}


ttype:
    |identifier {}
    |identifier typeArguments {}
    |identifier typeArguments ttypeGen {}
    | identifier ttypeGen {}
    | identifier bracketGen {}
    |basicType {}

typeArgumentsGen:
    | typeArgument {}
    | typeArgumentsGen COMM typeArgument {}


typeArguments:
    |LTHAN typeArgumentsGen GTHAN {}

typeArgument:
    |ttype {}
    | QM {}
    | QM EXTENDS ttype {}
    | QM SUPER ttype {}

statementExpression:
    |expression {}

constantExpression:
    |expression {}

expression1:
    |expression2 {}
    |expression2 expression1Rest {}

expression1Rest:
    |QM expression COL expression1{}

expression2:
    | expression3 {}
    | expression3 expression2Rest {}

expression2RestGen:
    |infixOp expression3 {}
    |expression2RestGen infixOp expression3 {}

expression2Rest:
    |expression2RestGen {}
    |expression3 INSTANCEOF ttype {}
 
infixOp:
    |COR {Cor}
    |CAND {Cand}
    |OR {Or}
    |XOR {Xor}
    |AND {And}
    |EQUAL {Equal}
    |NEQUAL {Nequal}
    |LTHAN {Lthan}
    |GTHAN {Gthan}
    |LETHAN {Lethan}
    |GETHAN {Gethan}
    |LSHIFT {Lshift}
    |RSHIFT {Rshift}
    |SHRR {Shrr}
    |PLUS {Plus}
    |MINUS {Minus}
    |MUL {Mul}
    |DIV {Div}
    |MOD {Mod}
    



expression3:
    |prefixOp expression3  {}
    |expression expression3 {}
    |ttype expression3 {}
    |primary list(selector) list(postfixOp) {}

primary:
    |parExpression {}
    |nonWildcardTypeArguments explicitGenericInvocationSuffix {}
    |nonWildcardTypeArguments THIS arguments {}
    |THIS arguments? {}
    |SUPER superSuffix {}
  (*  |literal {}  *)
    |NEW creator {}
    |identifier separated_list(DOT, identifier) identifierSuffix? {}
    |basicType separated_list(CROCHETG, CROCHETD) DOT CLASS {}
    | VOID DOT CLASS {}

identifierSuffix:
    |CROCHETG CROCHETD separated_list(CROCHETG, CROCHETD) DOT CLASS  {}
    |CROCHETG CROCHETD expression  {}
    |arguments  {}
    |DOT CLASS {}
    |DOT explicitGenericInvocation {}
    |DOT THIS SUPER arguments {}
    |DOT NEW  nonWildcardTypeArguments? innerCreator {}

explicitGenericInvocation:
    |nonWildcardTypeArguments explicitGenericInvocationSuffix {}

nonWildcardTypeArguments:
    |LTHAN typeList GTHAN {}

explicitGenericInvocationSuffix:
    |SUPER superSuffix {}
    |identifier arguments {}

prefixOp:
    |INC {Inc}
    |DEC {Dec}
    |NOT {Not}
    |BNOT {Bnot}
    |PLUS {Plus}
    |MINUS {Minus}

postfixOp:
    |INC {Inc}
    |DEC {Dec}

selector:
    |DOT identifier arguments? {}
    | DOT explicitGenericInvocation {}
    | DOT THIS {}
    | DOT SUPER superSuffix {}
    |DOT NEW nonWildcardTypeArguments? innerCreator {}

superSuffix:
    |arguments {}
    |DOT identifier arguments? {}

basicType:
    |BYTE {Byte}
    |SHORT {Short}
    |CHAR {Char}
    |INT {Int}
    |LONG {Long}
    |FLOAT {Float}
    |DOUBLE {Double}
    |BOOLEAN {Boolean}


classModifier:
    | STRICTFP {Strictfp}
    | PUBLIC {Public}
    | PRIVATE {Private}
    | PROTECTED {Protected}
    | ABSTRACT {Abstract}
    | STATIC {Static}
    | TRANSIENT {Transient}

arguments:
    |PARG expression separated_list(COMM, expression) PARD {}
    |PARG PARD {}

creator:
    |nonWildcardTypeArguments? createdName arrayCreatorRest {}
    |nonWildcardTypeArguments? createdName classCreatorRest {}

createdName:
    |identifier nonWildcardTypeArguments? separated_list( DOT, identifier, nonWildcardTypeArguments?) {}

innerCreator:
    |identifier classCreatorRest {}

arrayCreatorRest:
    |CROCHETG CROCHETD separated_list(CROCHETG, CROCHETD) arrayInitializer {}
    |CROCHETG expression CROCHETD separated_list(CROCHETG, expression, CROCHETD) separated_list(CROCHETG, CROCHETD) {}

classCreatorRest:
    |arguments {}
    |arguments classBody {}

arrayInitializer:
    |LBRACE variableInitializer separated_list(COMM, variableInitializer) COMM? RBRACE {}
    |LBRACE COMM? RBRACE {}

variableInitializer:
    |arrayInitializer {}
    |expression {}

parExpression:
    |PARG expression PARD {}

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
    | ttype variableDeclarators SEMI {}
    | FINAL ttype variableDeclarators SEMI {}

statement:
    | block {}
    | ASSERT expression SEMI {}
    | ASSERT expression COL expression SEMI {}
    | IF parExpression statement {}
    | IF parExpression statement ELSE statement {}
    | FOR PARG forControl PARD statement {}
    | WHILE parExpression statement {}
    | DO statement WHILE parExpression SEMI {}
	| TRY block catches {} 
	| TRY block FINALLY block {}
	| TRY block catches FINALLY block {}
	| SWITCH parExpression LBRACE switchBlockStatementGroups RBRACE {}
	| SYNCHRONIZED parExpression block {}
	| RETURN SEMI {}
	| RETURN expression SEMI {}
	| THROW expression SEMI {}
	| BREAK {}
	| BREAK identifier {}
	| CONTINUE {}
	| CONTINUE identifier {}
	| SEMI {}
	| statementExpression SEMI {}
	| identifier COL statement {}


catches:
	|catchClause {}
	|catches catchClause {}

catchClause:
	|CATCH PARG formalParameters PARD block {}

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
	|forInit SEMI expression SEMI {}
	|forInit SEMI  SEMI {}
	|forInit SEMI  SEMI {}

forVarControl:
	|ttype identifier forVarControlRest {}
	|FINAL ttype identifier forVarControlRest {}
	|FINAL annotations ttype identifier forVarControlRest {}
	|annotations ttype identifier forVarControlRest {}

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

elementValueArrayInitializer: (* ******************* *)
	|LBRACE elementValues RBRACE{}
	|LBRACE elementValues COMM RBRACE {}
    |LBRACE COMM RBRACE {}


elementValues:
	|elementValue {}
	|elementValue elementValues  {}	

forVarControlRest: 
	|variableDeclaratorsRest SEMI  SEMI {}
    |variableDeclaratorRest SEMI expression SEMI
	|COL expression  {}

forInit:
	|statementExpression expression  {}

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

variableDeclarators:
    |variableDeclarator {}
    |variableDeclarator COMM variableDeclarators {}

variableDeclaratorsRest:
    |variableDeclaratorRest
    |variableDeclaratorRest COMM variableDeclarator {}
    |variableDeclaratorRest COMM variableDeclarator {}

constantDeclaratorsRest:
    |constantDeclaratorRest
    |constantDeclaratorRest COMM constantDeclarator {}
    |constantDeclaratorsRest COMM constantDeclarator {}

variableDeclarator:
    |identifier variableDeclaratorRest {}

constantDeclarator:
    |identifier constantDeclaratorRest {}

(*externalisation de bracketGen pour permettre de mettre plusieurs brackets *)
variableDeclaratorRest:
    | {}
    | bracketGen {}
    | bracketGen EQUAL variableInitializer {}

(* idem *)
constantDeclaratorRest: 
    | EQUAL variableInitializer {}
    | bracketGen EQUAL variableInitializer {}


variableDeclaratorId:
    | identifier {}
    | variableDeclaratorId CROCHETG CROCHETD {}

(*externalisation de importDeclarationGen pour permettre de mettre plusieurs brackets *)
importDeclarationGen:
    | {}
    | importDeclarationGen importDeclaration {}


compilationUnit: 
    | {}
    | annotations PACKAGE qualifiedIdentifier SEMI {}
    | PACKAGE qualifiedIdentifier SEMI {}
    | annotations PACKAGE qualifiedIdentifier SEMI importDeclarationGen {}
    | PACKAGE qualifiedIdentifier SEMI importDeclarationGen {}
    | importDeclarationGen {}
    | compilationUnit typeDeclaration {}


importDeclaration:
    | IMPORT identifier SEMI {} 
    | IMPORT STATIC identifier SEMI {}
    | IMPORT identifier DOT MUL SEMI {}
    | IMPORT STATIC identifier SEMI {}

typeDeclaration:
    |classOrInterfaceDeclaration {}
    |SEMI {}

modifierGen:
    | {}
    | modifierGen modifier {}

classOrInterfaceDeclaration:
    | classDeclaration {}
    | interfaceDeclaration {}
    | modifierGen classDeclaration {}
    | modifierGen interfaceDeclaration {}
    
classDeclaration:
    | normalClassDeclaration {}
    | enumDeclaration {}

normalClassDeclaration:
    | CLASS identifier classBody {}
    | CLASS identifier typeParameters {}
    | CLASS identifier typeParameters EXTENDS ttype classBody {}
    | CLASS identifier EXTENDS ttype classBody {}


typeParameterGen:
    | typeParameter {}
    | typeParameterGen COMM typeParameter {}

typeParameters:
    | LTHAN typeParameterGen GTHAN {}

typeParameter:
    | identifier {}
    | identifier EXTENDS bound {}

bound:
    | ttype {}
    | bound BAND ttype {}

enumDeclaration:
    | ENUM identifier enumBody {}
    | ENUM identifier IMPLEMENTS typeList enumBody {}

(*1 item of enumBody *)
enumBodyItem:
    | enumConstants {}
    | enumConstants COMM {}
    | enumConstants COMM enumBodyDeclarations {}
    | enumConstants enumBodyDeclarations {}
    | COMM enumBodyDeclarations {}
    | COMM {}
    | enumBodyDeclarations {}

enumBody:
    | enumBodyItem {}
    | enumBody enumBodyItem {}

enumConstants:
    | enumConstant {}
    | enumConstants enumConstant {}

enumConstant:
    | annotations identifier {}
    | annotations identifier arguments {}
    | annotations identifier arguments classBody {}
    | annotations identifier classBody {}

enumBodyDeclarations:
    | SEMI classBodyDeclaration {}
    | enumBodyDeclarations classBodyDeclaration {}

interfaceDeclaration:
    | normalInterfaceDeclaration {}
    | annotationTypeDeclaration {}

normalInterfaceDeclaration:
    | INTERFACE identifier interfaceBody {}
    | INTERFACE identifier typeParameters interfaceBody  {}
    | INTERFACE identifier typeParameters EXTENDS typeList interfaceBody {}
    | INTERFACE identifier EXTENDS typeList interfaceBody {}

typeList:
    |ttype {}
    |typeList COMM ttype {}

annotationTypeDeclaration:
    | AT INTERFACE identifier annotationTypeBody {}

annotationTypeBody:
    | LBRACE RBRACE {}
    | LBRACE annotationTypeElementDeclarations RBRACE {}

enumConstantName:

	|identifier {}

methodBody:

	|block {}

formalParameterDeclsRest:

	|variableDeclaratorId COMM formalParameterDecls {}

	|variableDeclaratorId {}

	|TPOINT variableDeclaratorId {}

formalParameterDecls: (* weird a voir "]"? *)

	|FINAL ttype formalParameterDeclsRest {}

	|FINAL annotations formalParameterDeclsRest {}

	|annotations formalParameterDeclsRest {} 

	|formalParameterDeclsRest {}

formalParameters:

	| PARG PARD {} 

	| PARG formalParameterDecls PARD {}

qualifiedIdentifierList:

	|qualifiedIdentifier {}

	|qualifiedIdentifierList COMM qualifiedIdentifier {}


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

	|interfaceBodyDeclarationGen interfaceBodyDeclaration {}

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


identifier:
    |IDENT {}

qualifiedIdentifier:
    |identifier {}
    |qualifiedIdentifier identifier {}

