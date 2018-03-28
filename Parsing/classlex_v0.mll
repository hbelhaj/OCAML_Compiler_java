{
open Lexing


(* la gestion des commentaires , finir les print et les keywords *)

(*errors *)

type error =
	|Illegal_character of char
	|Illegal_float of string
exception Error of error * position * position

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


type lexeme =
| EOF
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


(*class*)



|CLASS 
|INTERFACE 
|AT 
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
|CONTINUE
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
open Lexing

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
}

let letter= ['a'-'z''A'-'Z']
let digit = ['0'-'9']
let hex = digit | ['a'-'f''A'-'F']	 
let hex_sign = ['x' 'X']
let lineterminator = (['\n' '\r'] | "\r\n")
let space = [' ' '\t']
let comment= "//"lineterminator
let integer=  digit+ | '0'  hex_sign hex+
let signed_integer = ['+' '-']? integer
let blank = [' ' '\009']



rule token = parse
  | lineterminator          { Lexing.new_line lexbuf; token lexbuf }
  | blank+                  { token lexbuf }
  | "EOF"	{EOF}
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
  | "||"		{COR }
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

























