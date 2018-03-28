{
  type lexeme =
    | EOF
    | AND
    | OR
    | TRUE
    | FALSE  
}
  
let space = [' ' '\t' '\n']

rule nexttoken = parse
    space+  { nexttoken lexbuf }
  | eof     { EOF }
  | "et"    { AND } 
  | "ou"    { OR } 
  | "vrai"  { TRUE } 
  | "faux"  { FALSE } 
