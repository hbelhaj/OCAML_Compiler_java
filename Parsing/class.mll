{
open Lexing


(* la gestion des commentaires ,float,IDENT *)

(*errors *)

type error =
	|Illegal_character of char
	|Illegal_float of string
exception Error of error * position * position
exception EOF
let raise_error err lexbuf =
raise(Error(err, lexeme_start_p lexbuf , lexeme_end_p lexbuf))

let report_error = function
	|Illegal_character c ->
		print_string " Illegal character ' ";
		print_char c;
		print_string "' "
	|Illegal_float d ->
		print_string "Illegal float ' ";
		print_string d;
		print_string "' "



(*basic functions *)

 
let print_position debut fin =

if(debut.pos_lnum = fin.pos_lnum) then
  begin
	print_string "line ";
	print_int debut.pos_lnum;
	print_string " characters ";
	print_int(debut.pos_cnum - debut.pos_bol);
	print_string "-";
	print_int(fin.pos_cnum - fin.pos_bol)
  end

else

  begin
	print_string "from line ";
	print_int debut.pos_lnum;
	print_string " character ";
	print_int(debut.pos_cnum - debut.pos_bol);
	print_string " to line ";
	print_int fin.pos_lnum;
	print_string " character ";
	print_int(fin.pos_cnum - fin.pos_bol);
  end 

let new_line lexbuf=
 let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <-
	  {
	    pos with
	      pos_lnum = pos.pos_lnum + 1;
	      pos_bol = pos.pos_cnum;
            }

let create_hashtable size init = 
		let tbl = Hashtbl.create size in 
		List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
		tbl


 
type lexeme =
| EOF
| CONST
| GOTO
| FLOAT 
| IDENT
| AUTO_EQUAL
| AUTO_ADD
| AUTO_SUB
| AUTO_MUL
| AUTO_DIV
| AUTO_MOD
| AUTO_XOR
| AUTO_AND
| AUTO_OR 
| AUTO_SHR 
| AUTO_SHL 
| AUTO_SHRR 
| PARG 
| PARD 
| LBRACE 
| RBRACE 
| CROCHETG
| CROCHETD
| VIRG 
| PVIRG 
| POINT
| COLON 
| PLUS 
| MINUS 
| INC 
| DEC 
| TIMES 
| DIV 
| MOD 
| AND 
| OR 
| XOR 
| NOT 
| CAND 
| COR 
| COND 
| SUPS 
| SUP 
| INFS 
| INF 
| EG 
| NEG 
| SHL 
| SHR 
| SHRR 
| BNOT 
| VARARG 

| CLASS 
| INTERFACE 
| AT 
|NEW 
|INSTANCEOF
|VOID 
|FINALLY   
|CATCH
|TRY
|SYNCHRONIZED 
|THROW 
|THROWS   
|RETURN
|BREAK
|SUPER
|THIS
|VOLATILE
|TRANSIENT
|STRICTFP
|FINAL
|STATIC
|ABSTRACT
|IMPLEMENTS 
|EXTENDS
|IMPORT
|PACKAGE
|PRIVATE
|PROTECTED
|PUBLIC 
|ENUM
|BOOLEAN
|ASSERT 
|BYTE 
|CHAR 
|CASE 
|CONTINUE 
|DEFAULT 
|DO  
|DOUBLE 
|ELSE 
|FOR 
|FALSE 
|IF 
|INT 
|LONG 
|NATIVE 
|NULL 
|SWITCH 
|SHORT  
|TRUE 
|WHILE
| INT_LIT of int
| STRING_LIT of string
| BOOL_LIT of bool
|FLOAT_LIT of float

|IDENTIFIER of bytes 



let print_lexeme = function
| EOF-> print_string "EOF"
| AUTO_EQUAL-> print_string "="
| AUTO_ADD  -> print_string "+="
| AUTO_SUB -> print_string "-="
| AUTO_MUL  -> print_string "*="
| AUTO_DIV -> print_string "/="
| AUTO_MOD  -> print_string "%="
| AUTO_XOR -> print_string "^="
| AUTO_AND -> print_string "&="
| AUTO_OR  -> print_string "|="
| AUTO_SHR  -> print_string ">>=" 
| AUTO_SHL  -> print_string "<<=" 
| AUTO_SHRR -> print_string ">>>="  
| PARG -> print_string "("
| PARD -> print_string ")"
| LBRACE -> print_string "{"
| RBRACE -> print_string "}"
| CROCHETG-> print_string "["
| CROCHETD-> print_string "]"
| VIRG -> print_string ","
| PVIRG -> print_string ";"
| POINT -> print_string "."
| COLON -> print_string ":"
| PLUS -> print_string  "+"
| MINUS -> print_string "-"
| INC -> print_string   "++"
| DEC -> print_string  "--"
| TIMES -> print_string "*"
| DIV -> print_string  "/"
| MOD -> print_string  "%"
| AND -> print_string  "&"
| OR -> print_string  "|"
| XOR -> print_string "^"
| NOT -> print_string "!"
| CAND -> print_string "&&"
| COR -> print_string "||"
| COND -> print_string "?"
| SUPS -> print_string ">"
| SUP -> print_string ">"
| INFS -> print_string "<"
| INF -> print_string "<="
| EG -> print_string "=="
| NEG -> print_string "!=="
| SHL -> print_string "<<"
| SHR -> print_string ">>"
| SHRR -> print_string ">>>"
| BNOT -> print_string "~"
| VARARG -> print_string "..."
|CLASS -> print_string "class"
|INTERFACE -> print_string "interface" 
|AT -> print_string "at"
|NEW -> print_string "new"
|INSTANCEOF -> print_string "instanceof"
|VOID -> print_string "void"
|FINALLY  -> print_string "finally" 
|CATCH -> print_string "catch"
|TRY -> print_string "try"
|SYNCHRONIZED -> print_string "synchronized"
|THROW -> print_string "throw" 
|THROWS -> print_string "throws" 
|RETURN -> print_string "return"
|BREAK -> print_string "break"
|SUPER -> print_string "super"
|THIS -> print_string "this"
|VOLATILE -> print_string "volatile"
|TRANSIENT -> print_string "transient"
|STRICTFP -> print_string "stricfp"
|FINAL -> print_string "final"
|STATIC -> print_string "static"
|ABSTRACT -> print_string "abstract"
|IMPLEMENTS -> print_string "implements"
|EXTENDS -> print_string "extends"
|IMPORT -> print_string "import"
|PACKAGE -> print_string "package"
|PRIVATE -> print_string "private"
|PROTECTED -> print_string "protected"
|PUBLIC -> print_string "public"
|ENUM -> print_string "enum"
|BOOLEAN -> print_string "boolean"
|ASSERT -> print_string "assert"
|BYTE -> print_string "byte" 
|CHAR -> print_string "char" 
|CASE -> print_string "case" 
|CONTINUE -> print_string "continue"
|DEFAULT -> print_string "default"
|DO -> print_string "do"
|DOUBLE -> print_string "double" 
|ELSE -> print_string "else"
|FLOAT -> print_string "float" 
|FOR -> print_string "for"
|FALSE-> print_string "false"
|IF -> print_string "if"
|INT -> print_string "int"
|LONG -> print_string "long"
|NATIVE -> print_string "native"
|NULL -> print_string "null" 
|SWITCH -> print_string "switch"
|SHORT -> print_string "short"
|TRUE -> print_string "true"
|WHILE -> print_string "while"
|IDENTIFIER s -> print_string"IDENTIFIER("; print_string s; print_string")"
| INT_LIT f -> print_string "INTLIT("; print_int f; print_string")"
| STRING_LIT  s -> print_string"STRINGLIT("; print_string s; print_string")"
| BOOL_LIT b -> print_string "BOOL_LIT("; print_string")"
| FLOAT_LIT f -> print_string"FLOAT_LIT("; print_float f; print_string")"
|GOTO -> print_string "goto"

let kw_table = 
		[
	 	 "abstract", ABSTRACT ;
		 "assert", ASSERT ;
		 "boolean", BOOLEAN ;
		 "break", BREAK ;
		 "byte", BYTE ;
		 "case", CASE ;
		 "catch", CATCH ;
		 "char", CHAR ;
		 "class", CLASS ;
		 "const", CONST ;
		 "continue", CONTINUE ;
		 "default", DEFAULT ;
		 "do", DO ;
		 "double", DOUBLE ;
		 "else", ELSE ;
		 "enum", ENUM ;
		 "extends", EXTENDS ;
		 "final", FINAL ;
		 "finally", FINALLY ;
		 "float", FLOAT  ;
		 "for", FOR ;
		 "if", IF ;
		 "goto", GOTO ;
		 "implements", IMPLEMENTS ;
		 "import", IMPORT ;
		 "instanceof", INSTANCEOF ;
		 "int", INT ;
		 "interface", INTERFACE ;
		 "long", LONG ;
		 "native", NATIVE ;
		 "new", NEW ;
		 "switch", SWITCH ;
		 "synchronized", SYNCHRONIZED ;
		 "package", PACKAGE ;
		 "private", PRIVATE ;
		 "this", THIS ;
		 "protected", PROTECTED ;
		 "throw", THROW ;
		 "public", PUBLIC ;
		 "throws", THROWS ;
		 "return", RETURN ;
		 "transient", TRANSIENT ;
		 "short", SHORT ;
		 "try", TRY ;
		 "static", STATIC ;
		 "void", VOID ;
		 "strictfp", STRICTFP ;
		 "volatile", VOLATILE ;
		 "super", SUPER ;
		 "while", WHILE ;
		 "true" , TRUE;
		 "false", FALSE;
] 

}






let int_type= ('l'|'L')
let letter= ['a'-'z''A'-'Z']
let digit = ['0'-'9']
let hex = digit | ['a'-'f''A'-'F']	 
let hex_sign = ['x' 'X']
let lineterminator = (['\n' '\r'] | "\r\n")
let not_lineterminator = ([^ '\n' '\r'] | "\r\n")
let space = [' ' '\t']
let comment_oneline= "//" (letter | digit | [' ''[''{''}''('')''['']''=''!''&''|'';''.''<''>'',''+''-''*''/''=''"'':'] )* lineterminator
let comment_mlines= "/*" ([^'*'] | lineterminator | ('*'*[^'/']))* "*/"
let integer=  digit+ | '0'  hex_sign hex+
let signed_integer = ['+' '-']? integer
let blank= [' ' '\009']
let identifier= letter(letter | digit |'_')* 
let integer = ('0' | digit) (digit)*
let float_type = ('f' | 'F' | 'd' | 'D')
let floats = (digit+ '.' digit+ ( ('e'|'E') ('+'|'-')? (digit)+ )? float_type?
  | '.' digit+ ( ('e'|'E') ('+'|'-')? (digit)+ )? float_type?
  | digit+ ( ('e'|'E') ('+'|'-')? (digit)+ ) float_type?
| digit+ ( ('e'|'E') ('+'|'-')? (digit)+ ) float_type)
let esc_seq= ('\b' | '\t' | '\n' | '\r') (* '\' | \' | \\)*)
let input_char = (letter | digit | [' ''[''{''}''('')''['']''=''!''&''|'';''.''<''>'',''+''-''*''/''=''"'':']) 
let str_char = (input_char | esc_seq)
let str = '"' str_char* '"'
let character = "'" str_char "'"
let boolean = ("true" | "false")





rule token = parse
  | lineterminator { Lexing.new_line lexbuf; token lexbuf }
  | blank+         { token lexbuf }
  | "eof"	{EOF}
  | "="		{AUTO_EQUAL}
  | "+="	{AUTO_ADD}
  | "-="	{AUTO_SUB}
  | "*="	{AUTO_MUL}
  | "/="	{AUTO_DIV}
  | "%="	{AUTO_MOD}
  | "^="	{AUTO_XOR}
  | "&="	{AUTO_AND}
  | "|="	{AUTO_OR }
  | ">>="	{AUTO_SHR }
  | "<<="	{AUTO_SHL }
  | ">>>="	{AUTO_SHRR} 
  | "("		{PARG }
  | ")"		{PARD} 
  | "{"		{LBRACE }
  | "}"		{RBRACE }
  | "["		{CROCHETG}
  | "]"		{CROCHETD}
  | ","		{VIRG }
  | ";"		{PVIRG }
  | "."		{POINT}
  | ":"		{COLON }
  | "+"		{PLUS }
  | "-"		{MINUS} 
  | "++"	{INC }
  | "--"	{DEC }
  | "*"		{TIMES }
  | "/"		{DIV  }
  | "%"		{MOD }
  | "&"		{AND }
  | "|"		{OR }
  | "^"		{XOR }
  | "!"		{NOT }
  | "&&"	{CAND }
  | "||"        {COR }
  | "?"		{COND} 
  | ">"		{SUPS} 
  | ">="	{SUP} 
  | "<"		{INFS} 
  | "<="	{INF} 
  | "=="	{EG} 
  | "!=="	{NEG} 
  | "<<"	{SHL} 
  | ">>"	{SHR} 
  | ">>>"	{SHRR} 
  | "~"         {BNOT }
  | "..."       {VARARG }
  | identifier as id { 
		(* try keywords if not found then it's an identifier *)
		let l = String.lowercase id in
		try List.assoc l kw_table
		with Not_found -> IDENTIFIER id
  }
| (integer int_type?) as nb            { INT_LIT(int_of_string nb )}
  | '"' (str_char* as s) '"' { STRING_LIT s }
  | boolean as b { BOOL_LIT (bool_of_string b) }
  | floats as nb { FLOAT_LIT(float_of_string nb )}
  | comment_oneline { token lexbuf }
  | comment_mlines { token lexbuf } 

(*classes *)
  | "class"   { CLASS }
  | "interface"   { INTERFACE }
  | "@"   { AT }
  | "new"   { NEW }
  | "instanceof"   { INSTANCEOF }
  | "void"   { VOID }
  | "finally"   { FINALLY }   
  | "catch"   { CATCH }
  | "try"   { TRY }
  | "synchronized"   { SYNCHRONIZED }
  | "throw"   { THROW }
  | "throws"   { THROWS }   
  | "return"   { RETURN }
  | "continue"   { CONTINUE }
  | "break"   { BREAK }
  | "super"   { SUPER }
  | "this"   { THIS }
  | "volatile"   { VOLATILE }
  | "transient"   { TRANSIENT }
  | "strictfp"   { STRICTFP }
  | "final"   { FINAL }
  | "static"   { STATIC }
  | "abstract"   { ABSTRACT }
  | "implements"   { IMPLEMENTS }
  | "extends"   { EXTENDS }
  | "import"   { IMPORT }
  | "package"   { PACKAGE }
  | "private"   { PRIVATE }
  | "protected"   { PROTECTED }
  | "public"   { PUBLIC }
  | "enum"   { ENUM }
  | "boolean"   { BOOLEAN }
  | "assert" { ASSERT }
  | "byte"  { BYTE }
  | "char" { CHAR }
  | "case" { CASE }
  | "const" { CONST }
  | "default" { DEFAULT }
  | "do"  { DO  }
  | "else"  { ELSE }
  | "for" { FOR }
  | "false" { FALSE }
  | "goto"  { GOTO  }
  | "if" { IF } 
  | "int" { INT }
  |"double" {DOUBLE}
  | "long" { LONG }
  | "native" { NATIVE }
  | "null" { NULL }
  | "switch" { SWITCH }
  | "short"  { SHORT  }
  | "true"  { TRUE }
  | "while" { WHILE }
  

 
{
let rec examine_all lexbuf =
	let res = token lexbuf in
	print_lexeme res;
	print_string " ";
	
	match res with
		| EOF -> ()
		| _  -> examine_all lexbuf

let compile file =
	print_string("File " ^file^ " is being treated!\n");
	try
	let input_file = open_in file in
	let lexbuf = Lexing.from_channel input_file in
	examine_all lexbuf;
	print_newline ();
	close_in(input_file)
	with Sys_error s ->
	print_endline("Can’t find file ’"^ file ^"’")




}
























 
