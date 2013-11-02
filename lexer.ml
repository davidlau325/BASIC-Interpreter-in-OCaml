type lexeme = Lint of int
			| Lident of string
			| Lsymbol of string
			| Lstring of string
			| Lend;;

type string_lexer = {string:string; mutable current:int; size:int};;

let init_lex s = {string=s;current=0;size=String.length s};;
let forward cl= cl.current <- cl.current+1;;
let forward_n cl n = cl.current <- cl.current+n;;
let extract pred cl =
	let st = cl.string and pos = cl.current in
	let rec ext n = if n<cl.size && (pred st.[n]) then ext (n+1) else n in
	let res = ext pos
	in cl.current <- res; String.sub cl.string pos (res-pos);;

let extract_int = 
	let is_int = function '0'..'9' -> true | _ -> false
	in function cl -> int_of_string (extract is_int cl);;

let extract_ident = 
	let is_alpha_num = function
		'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true
		| _ -> false
	in extract is_alpha_num;;

exception LexerError;;

let rec lexer cl =
	let lexer_char c = match c with
	  ' '
	| '\t' -> forward cl;lexer cl
	| 'a'..'z'
	| 'A'..'Z' -> Lident (extract_ident cl)
	| '0'..'9' -> Lint (extract_int cl)
	| '"' -> forward cl;
			 let res = Lstring (extract ((<>) '"') cl)
			 in forward cl; res
	| '+' | '-' | '*' | '/' | '%' | '&' | '|' | '!' | '=' | '(' | ')' ->
			 forward cl; Lsymbol (String.make 1 c)
	| '<'
	| '>' -> forward cl;
			 if cl.current >= cl.size then Lsymbol (String.make 1 c)
			 else let cs = cl.string.[cl.current]
			 	  in (match (c,cs) with
			 	  		 ('<','=') -> forward cl; Lsymbol "<="
			 	  	   | ('>','=') -> forward cl; Lsymbol ">="
			 	  	   | ('<','>') -> forward cl; Lsymbol "<>"
			 	  	   | _ -> Lsymbol (String.make 1 c)
			 		)
	| _ -> raise LexerError
in
	if cl.current >= cl.size then Lend
	else lexer_char cl.string.[cl.current];; 