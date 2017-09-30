(*Author : Jayaprakash A*)
(*Roll no : 111501010*)
(*Code : Compilers lab assignment two*)
(*Course : Compilers-lab*)


(*

Formulate an ML data type to capture context free grammars. In the rest of the steps you will write functions to process this data type. This will reduce the burden of writing I/O routines for entering the grammar.

*)


datatype terminal = Ter of string;
datatype nonTerminal = NonTer of string;
type startState = nonTerminal;

datatype symbol = T of terminal | Nt of nonTerminal;
type rhs = symbol list;

datatype rule = Rule of ( nonTerminal * rhs );
datatype cfg = Grammar of (startState * rule list ) ;

fun get_variable_name( T ( Ter(X) )) = X
		|get_variable_name( Nt ( NonTer(X) )) = X

fun compare_keys(X:symbol,y:symbol) = String.compare(get_variable_name(X),get_variable_name(y));

(*Functional finite maps with atom keys.*)

structure AtomRedBlackMap =
	RedBlackMapFn (
		struct
			type ord_key = symbol
			val compare = compare_keys
		end)




(*Grammar of balanced paranthesis.*)


val start = NonTer("S");
val non_ter_A = NonTer("A")
val non_ter_B = NonTer("B")
val non_ter_C = NonTer("C")
val ter_one = Ter("a");
val ter_two = Ter("b");
val ter_three = Ter("c");
val rule_1 = Rule(start,[Nt(non_ter_A), Nt(non_ter_C), Nt(non_ter_A), Nt(non_ter_C)]);
val rule_2 = Rule(non_ter_A,[]);
val rule_3 = Rule(non_ter_B,[]);
val rule_4 = Rule(non_ter_C,[T(ter_three)]);
val rule_5 = Rule(non_ter_A,[T(ter_one), Nt(non_ter_A)]);
val rule_6 = Rule(non_ter_B,[T(ter_two), Nt(non_ter_B)]);


val rule_list = [rule_1,rule_2, rule_3,rule_4,rule_5, rule_6];
val example_grammar =  Grammar(start,rule_list);




(*

Write an ML function to compute the FIRST and FOLLOW of all non-terminals in your grammar. Implicit in this problem is to design an ML data type for storing this table.

*)

(*Calculating the nullables*)
(*Initialise map*)
val map_nullable = AtomRedBlackMap.empty

(*check if a symbol is present in map or not*)

fun is_present_in_map(Nt(X),map_nullable) = if AtomRedBlackMap.find(map_nullable,Nt(X)) = NONE
										then
											false
										else true
	| is_present_in_map(T(X),map_nullable) =
		if AtomRedBlackMap.find(map_nullable,T(X)) = NONE
		then
			false
		else true;

(*Check if rule is empty or if symbols belong to the nullables set until the entire rhs aprt finishes*)
fun  find_symbol_list([],map_nullable) = true |
	find_symbol_list(sym :: sym_list,map_nullable) = 
			is_present_in_map(sym,map_nullable) andalso find_symbol_list(sym_list,map_nullable);



fun get_rule_rhs(Rule(nonterm, rhs)) = rhs;

(*Check if rule is empty and then insert the nullable non terminal accordingly*)

fun save_in_map(map_nullable,Rule(nonterm,[])) = AtomRedBlackMap.insert(map_nullable,Nt(nonterm),true) |
	save_in_map(map_nullable,Rule(nonterm,sym :: sym_list)) = 
		if find_symbol_list(get_rule_rhs(Rule(nonterm, sym :: sym_list)),map_nullable) = true
		then
			AtomRedBlackMap.insert(map_nullable,Nt(nonterm),true)
		else map_nullable;


(*See all the rules and save nullables in the map accordingly*)
fun save_rule_map(map_nullable,[]) = map_nullable |
	save_rule_map(map_nullable,rule :: rule_list) = 
			let val map_nullable_new = save_in_map(map_nullable,rule); in
				save_rule_map(map_nullable_new,rule_list)
			end

fun check_fixed_point(map_nullable, rule_list) =

			let val map_nullable_new = save_rule_map(map_nullable, rule_list) in

			if ( AtomRedBlackMap.listItems(map_nullable) = AtomRedBlackMap.listItems(map_nullable_new) )
			then map_nullable_new
			else
				check_fixed_point(map_nullable_new, rule_list)
			end;

(*End of calculation of nullable*)

(*
For checking the working of nullables
*)
fun find_rules(Grammar(start,rule_list)) = rule_list
val rule_list = find_rules(example_grammar)

val map_nullable_new = check_fixed_point(map_nullable ,rule_list)
val nullables = AtomRedBlackMap.listItemsi(map_nullable_new)

(*Calculating the first and follow*)

val map_first  =  AtomRedBlackMap.empty;


