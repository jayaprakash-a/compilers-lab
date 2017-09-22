(*Author : Jayaprakash A*)
(*Roll no : 111501010*)
(*Code : Compilers lab assignment two*)
(*Course : Compilers-lab*)


(*

Formulate an ML data type to capture context free grammars. In the rest of the steps you will write functions to process this data type. This will reduce the burden of writing I/O routines for entering the grammar.

*)

type startState = Start of string;

datatype terminal = Ter of string;
datatype nonTerminal = NonTer of string;

datatype symbol = T of terminal | Nt of nonTerminal;
type rhs = Sym of symbol list;

datatype rule = Rule of ( nonTerminal * rhs );
datatype cfg = Grammar of (startState * rule list ) ;


(*
fun first-loop [] = [] |
	first-loop (x :: (x1 :: xs)) = first(x) @ first(x1);



fun first [] = [] |
	first (x :: (x1 :: xs)) = if first-loop(x) == []
				then first(x1)
				else first(x);

*)

(*Functional finite maps with atom keys.*)

structure AtomRedBlackMap =
	RedBlackMapFn (
		struct
			type ord_key = Atom.atom
			val compare = Atom.compare
		end)


(*

Grammar of balanced paranthesis.


val start = NonTer("S");
val ter_one = Ter("(");
val ter_two = Ter(")");
val rule_1 = Rule(start,Sym([T((), Nt(start), T())]));
val rule_2 = Rule(start,Sym([]));


val rule_list = [rule_1,rule_2];
val example_grammar =  Grammar(start,rule_list);

*)


(*

Write an ML function to compute the FIRST and FOLLOW of all non-terminals in your grammar. Implicit in this problem is to design an ML data type for storing this table.

*)


fun get_rules(Grammar(start,rule_list)) = rule_list

val rule_list = find_rules(example_grammar)

fun get_variable_name( T ( Ter(x) )) = x
		|get_variable_name( Nt ( NonTer(x) )) = x

fun get_non_term(Rule(nonTerminal,rhs)) = nonTerminal

(*val nullable_variables = []*)

val map_item = MapT.empty

