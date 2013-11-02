type exp_elem = 
	  Texp of expression
	| Tbin of bin_op
	| Tunr of unr_op
	| Tlp;;

exception ParseError;;

let unr_symb = function 
	"!" -> NOT | "-" -> UMINUS | _ -> raise ParseError;;

let bin_symb = function
	"+" -> PLUS | "-" -> MINUS | "*" -> MULT | "/" -> DIV | "%" -> MOD
	| "=" -> EQUAL | "<" -> LESS | "<=" -> LESSEQ | ">" -> GREAT
	| ">=" -> GREATEQ | "<>" -> DIFF | "&" -> AND | "|" -> OR
	| _ -> raise ParseError;;

let tsymb s = try Tbin (bin_symb s) with ParseError -> Tunr (unr_symb s);;

let reduce pr = function
	(Texp e)::(Tunr op)::st when (priority_uop op) >= pr
		-> (Texp (ExpUnr (op,e)))::st
	| (Texp e1)::(Tbin op)::(Texp e2)::st when (priority_binop op) >= pr
		-> (Texp (ExpBin (e2,op,e1)))::st
	| _ -> raise ParseError;;

let rec stack_or_reduce lex stack = match lex,stack with
	Lint n,_ -> (Texp (ExpInt n))::stack
	| Lident v,_ -> (Texp (ExpVar v))::stack
	| Lstring s,_ -> (Texp (ExpStr s))::stack
	| Lsymbol "(",_ -> Tlp::stack
	| Lsymbol ")",(Texp e)::Tlp::st -> (Texp e)::st
	| Lsymbol ")",_ -> stack_or_reduce lex (reduce 0 stack)
	| Lsymbol s,_ ->
		let symbol =
			if s<>"-" then tsymb s
			else match stack with
				(Texp _)::_ -> Tbin MINUS
				| _ -> Tunr UMINUS
			in (match symbol with
					Tunr op -> (Tunr op)::stack
					| Tbin op ->
					(try stack_or_reduce lex (reduce (priority_binop op) stack)
					with ParseError -> (Tbin op)::stack)
					| _ -> raise ParseError)
	| _,_ -> raise ParseError;; 

let rec reduce_all = function
	| [] -> raise ParseError
	| [Texp x] -> x
	| st -> reduce_all (reduce 0 st);;

let parse_exp stop cl =
	let p = ref 0 in
		let rec parse_one stack =
			let l = (p:=cl.current;lexer cl)
		in if not (stop l) then parse_one (stack_or_reduce l stack)
			else (cl.current <- !p; reduce_all stack)
	in parse_one [];;