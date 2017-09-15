type startState = char;

datatype terminal = char | nil;

datatype nonTerminal = NonTer of char;


datatype rhs = nil | nonTerminal | terminal | RHS of (char * rhs);

datatype rule = Rule of ( nonTerminal * rhs );



datatype cfg = Grammar of (startState * rule list ) ;






