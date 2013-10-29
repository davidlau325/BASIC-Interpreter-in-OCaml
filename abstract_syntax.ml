type unr_op = UMINUS | NOT;;
type bin_op = PLUS | MINUS | MULT | DIV | MOD
			| EQUAL | LESS | LESSEQ | GREAT | GREATEQ
			| DIFF | AND | OR;;
type expression =
	 ExpInt of int
   | ExpVar of string
   | ExpStr of string
   | ExpUnr of unr_op * expression
   | ExpBin of expression * bin_op * expression;;
type command = 
	 Rem of string
   | Goto of int
   | Print of expression
   | Input of string
   | If of expression * int
   | Let of string * expression;;
type line = {num:int; cmd:command};;
type program = line list;;
type phrase = Line of line | List | Run | PEnd;;

let priority_uop = function NOT-> 1 | UMINUS-> 7;;
let priority_binop = function
	 MULT | DIV -> 6
   | PLUS | MINUS -> 5
   | MOD -> 4
   | EQUAL | LESS | LESSEQ | GREAT | GREATEQ | DIFF -> 3 
   | AND | OR -> 2;;