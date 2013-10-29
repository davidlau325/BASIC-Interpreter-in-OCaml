let pp_binop = function
	 PLUS -> "+" | MULT -> "*" | MOD -> "%" | MINUS -> "-"
   | DIV -> "/" | EQUAL -> " = " | LESS -> " < "
   | LESSEQ -> " <= " | GREAT -> " > "
   | GREATEQ -> " >= " | DIFF -> " <> " | AND -> " & " 
   | OR -> " | ";;

let pp_unrop = function UMINUS -> "-" | NOT-> "!";;

let parenthesis x = "(" ^ x ^ ")";;

let pp_expression =
	let rec ppl pr = function
		 ExpInt n -> (string_of_int n)
	   | ExpVar v -> v
	   | ExpStr s -> "\"" ^ s ^ "\""
	   | ExpUnr (op,e) ->
	   		let res = (pp_unrop op)^(ppl (priority_uop op) e)
	   	    in if pr=0 then res else parenthesis res
	   | ExpBin (e1,op,e2) ->
	   		let pr2 = priority_binop op
	   		in let res=(ppl pr2 e1)^(pp_binop op)^(ppr pr2 e2)
	   		in if pr2 >= pr then res else parenthesis res
	and ppr pr exp = match exp with
	   		ExpBin (e1,op,e2) ->
	   			let pr2=priority_binop op
	   			in let res=(ppl pr2 e1)^(pp_binop op)&(ppr pr2 e2)
	   			in if pr2 > pr then res else parenthesis res
	   | _ -> ppl pr exp
	   in ppl 0;; 

let pp_command = function
	  Rem s -> "REM "^s
	| Goto n -> "GOTO "^(string_of_int n)
	| Print e -> "PRINT "^(pp_expression e)
	| Input v -> "INPUT "^v
	| If (e,n) -> "IF "^(pp_expression e)^" THEN "^(string_of_int n)
	| Let (v,e) -> "LET "^v^" = "^(pp_expression e);;

let pp_line l=(string_of_int l.num)^" "^(pp_command l.cmd);;