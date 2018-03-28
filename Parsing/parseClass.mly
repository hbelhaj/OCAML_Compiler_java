%{
    open Lexing
    open AST
}%

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























































