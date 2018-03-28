{
open Lexing

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
| INT_LIT of int
| STRING of string
| BOOL_LIT of bool
|FLOAT_LIT of float
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
| INT_LIT f -> print_string "INTLIT("; print_int f; print_string")"
| STRING  s -> print_string"STRINGLIT("; print_string s; print_string")"
| BOOL_LIT b -> print_string "BOOL_LIT("; print_string")"
| FLOAT_LIT f -> print_string"FLOAT_LIT("; print_float f; print_string")"
}

let esc_seq= ('\b' | '\t' | '\n' | '\r') (* '\' | \' | \\)*)
let letter= ['a'-'z''A'-'Z']
let digit = ['0'-'9']
let hex = digit | ['a'-'f''A'-'F']	 
let hex_sign = ['x' 'X']
let newline = (['\n' '\r'] | "\r\n")
let space = [' ' '\t']
let input_char = (letter | digit | [' ''[''{''}''('')''['']''=''!''&''|'';''.''<''>'',''+''-''*''/''=''"'':']) 
let boolean = ("true" | "false")
let integer=  digit+ | '0'  hex_sign hex+
let signed_integer = ['+' '-']? integer


let comment_mul = "/*" ([^'*'] | newline | ('*'*[^'/']))* "*/"
let comment= "//" input_char* newline

let int_type= ('l'|'L')
let float_type = ('f' | 'F' | 'd' | 'D')
let floats = (digit+ '.' digit+ ( ('e'|'E') ('+'|'-')? (digit)+ )? float_type?
  | '.' digit+ ( ('e'|'E') ('+'|'-')? (digit)+ )? float_type?
  | digit+ ( ('e'|'E') ('+'|'-')? (digit)+ ) float_type?
| digit+ ( ('e'|'E') ('+'|'-')? (digit)+ ) float_type)

let str_char = (input_char | esc_seq)
let str = '"' str_char* '"'
let character = "'" str_char "'"
let null = "null"

rule token = parse
  | newline          { Lexing.new_line lexbuf; token lexbuf }
  | space+                  { token lexbuf }
  |  comment	{ token lexbuf }
  | comment_mul { token lexbuf }
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
  | (integer int_type?) as nb            { INT_LIT(int_of_string nb )}
  | '"' (str_char* as s) '"' { STRING s }
  | boolean as b { BOOL_LIT (bool_of_string b) }
  | floats as nb { FLOAT_LIT(float_of_string nb )}

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

























