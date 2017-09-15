type startState = char;
type terminal = string;

datatype nonTerminal = NonTer of char;

datatype symbol = terminal | nonTerminal;


type rhs = symbol list;

datatype rule = Rule of ( nonTerminal * rhs );



datatype cfg = Grammar of (startState * rule list ) ;



fun first-loop [] = [] |
	first-loop (x :: (x1 :: xs)) = first(x) @ first(x1);



fun first [] = [] |
	first (x :: (x1 :: xs)) = if first-loop(x) == []
				then first(x1)
				else first(x);
