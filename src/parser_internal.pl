:- module(parser_internal,[ rec_expr/1,
                            parse_expr/2
                          ]).

/** <module> Parser For Logical Expressions.

Parser for logical expression according to the grammar G (given in EBNF).

==
    Def: Grammar G

    expr =      v |
               "T" |
               "F" |
               not, exp |
               exp, and, exp |
               exp, or, exp |
               "(", exp, ")" ;

==

As G is left-recursive it has to be transformend to avoid an infinite loop.
The following algorithm was used eliminate left-recursion:

==
    A --> Aa1 | ... | Aan | b1 | ... | bn

    where
    * A is a left-recursive non-terminal
    * a is a sequence of terminals and non-terminals that is not null
    * b is a sequence of terminals and non-terminals that does not start with A

    I. repalce the A-production by the production
    A --> b1A' | ... | bnA'
    II. create a new non-terminal
    A' --> null | a1A' | ... | anA'

==

*/

/** rec_expr(+Expr:expression)
 *
 * Expression recogniser: Determines if a given expression is in the language of PC.
 */
rec_expr(Expr) :-
    expr(_,Expr,[]).

/** parse_expr(?ParseTree:parse_tree,+Expr:expression).
 *
 * Expression parser: Determines the parse tree ParseTree of a given expression Expr
 * if it is in the language of PC (propositional calculus).
 */

parse_expr(ParseTree,Expr) :-
    expr(ParseTree,Expr,[]).

% production rules

%% logical variables
expr(v(X))          --> v(X), expr_rest(end,_).
expr(and(v(X),Y))   --> v(X), expr_rest(and,Y).
expr(or(v(X),Y))    --> v(X), expr_rest(or,Y).

%% junctors of arity 0
expr(X)        --> ver(X), expr_rest(end,_).
expr(and(X,Y)) --> ver(X), expr_rest(and,Y).
expr(or(X,Y))  --> ver(X), expr_rest(or,Y).

expr(X)        --> fal(X), expr_rest(end,_).
expr(and(X,Y)) --> fal(X), expr_rest(and,Y).
expr(or(X,Y))  --> fal(X), expr_rest(or,Y).

%% junctors of arity 1
expr(neg(X))        --> neg, expr(X), expr_rest(end,_).
expr(and(neg(X),Y)) --> neg, expr(X), expr_rest(and,Y).
expr(or(neg(X),Y))  --> neg, expr(X), expr_rest(or,Y).

%%% parentheses
expr(X)             --> l_par, expr(X), r_par, expr_rest(end,_).
expr(and(X,Y))      --> l_par, expr(X), r_par, expr_rest(and,Y).
expr(or(X,Y))       --> l_par, expr(X), r_par, expr_rest(or,Y).

%% junctors of arity 2
expr_rest(end,[])      --> [].
expr_rest(and,Y)      --> and, expr(Y), expr_rest(_,_).
expr_rest(or,Y)       --> or, expr(Y), expr_rest(_,_).

%% alphabet (terminals)
v(X) --> [X],{ dif(X,'T'), dif(X,'F') }.

ver('T') --> ['T'].
fal('F') --> ['F'].

neg --> ['~'].

and --> ['&&'].
or --> ['||'].

l_par --> ['('].
r_par --> [')'].




