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
val ter_one = Ter("(");
val ter_two = Ter(")");
val rule_1 = Rule(start,[T(ter_one), Nt(start), T(ter_two)]);
val rule_2 = Rule(start,[]);


val rule_list = [rule_1,rule_2];
val example_grammar =  Grammar(start,rule_list);




(*

Write an ML function to compute the FIRST and FOLLOW of all non-terminals in your grammar. Implicit in this problem is to design an ML data type for storing this table.

*)

(*Initialise map*)
val map_item = AtomRedBlackMap.empty

(*check if a symbol is present in map or not*)

fun is_not_present_in_map(Nt(X),map_item) = if AtomRedBlackMap.find(map_item,Nt(X)) = NONE
										then
											true
										else false
	| is_not_present_in_map(T(X),map_item) =
		if AtomRedBlackMap.find(map_item,T(X)) = NONE
		then
			true
		else false;

(*Check if rule is empty or if symbols belong to the nullables set until the entire rhs aprt finishes*)
fun  find_symbol_list([],map_item) = true |
	find_symbol_list(sym :: sym_list,map_item) = 
			is_not_present_in_map(sym,map_item) andalso find_symbol_list(sym_list,map_item);



fun get_rule_rhs(Rule(nonterm, rhs)) = rhs;

(*Check if rule is empty and then insert the nullable non terminal accordingly*)

fun save_in_map(map_item,Rule(nonterm,[])) = map_item |
	save_in_map(map_item,Rule(nonterm,sym :: sym_list)) = 
		if find_symbol_list(get_rule_rhs(Rule(nonterm, sym :: sym_list)),map_item) = true
		then
			AtomRedBlackMap.insert(map_item,Nt(nonterm),true)
		else map_item;


(*See all the rules and save nullables in the map accordingly*)
fun save_rule_map(map_item,[]) = map_item |
	save_rule_map(map_item,rule :: rule_list) = 
			let val map_item_new = save_in_map(map_item,rule); in
				save_rule_map(map_item_new,rule_list)
			end

(*Check if two maps are equal*)
fun check_fixed_point(map_item, rule_list) =

			let val map_item_new = save_rule_map(map_item, rule_list) in

			if ( AtomRedBlackMap.listItems(map_item) = AtomRedBlackMap.listItems(map_item_new) )
			then map_item_new
			else
				check_fixed_point(map_item_new, rule_list)
			end;



(*
For checking the working of nullables
*)
fun find_rules(Grammar(start,rule_list)) = rule_list
val rule_list = find_rules(example_grammar)

val map_item_new = check_fixed_point(map_item ,rule_list)
val nullables = AtomRedBlackMap.listItemsi(map_item_new)

