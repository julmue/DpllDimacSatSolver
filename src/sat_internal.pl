:- module(sat_internal,[    vars_in_parsetree/2,
                            vars_in_parsetreelist/2,
                            dpll/1
                        ]).

:- use_module(parser_internal).
:- use_module(simplification).
:- use_module(substitution).

/** <module> Davis–Putnam–Logemann–Loveland (DPLL) algorithm.

This algorithm is based on the DPLL-algorithm with some slight variations.

==
    algorithm DPLL
        input:  a logical expression Expr.
        output: if Expr is satisfiable 'True', 'False' otherwise.

    dpll(Expr)
        //  initialization
        1.  Expr is parsed into ParseTree.
        2.  ParseTree is simplified.
        3.  The List Vars of free variables in ParseTree is extracted.
        //  main searching
        4.  while ParseTree != True and Vars != []
            4.1 assign a boolean value V to the variable in head of Vars HVars.
                -> set choicepoint.
            4.2 substitute V for HVars in Expr.
            4.3 simplifiy.
        5.  if ParseTree == True
              return True.
            else
                if there are choicepoints
                    backtrack to last choicepoint.
                else
                    return False.

==

*/

/** vars_in_parsetree(-Vars:list,+ParseTree:parse_tree).
 *
 * Extracts a list Vars of free variables form ParseTree.
 */
vars_in_parsetree(Vars,v(X)) :-
    Vars = [v(X)], !.
vars_in_parsetree(Vars,ParseTree) :-
    ParseTree =.. [Functor|Args],
    dif(Functor,v),
    vars_in_parsetreelist(IVars,Args),
    list_to_set(IVars,Vars).

/** vars_in_parsetreelist(-Vars:list,+ParseTree:parse_tree_list).
 *
 * Extracts a list Vars of free variables form a list of parse trees ParseTreeList.
 */

vars_in_parsetreelist([],[]) :- !.
vars_in_parsetreelist(Vars,[ParseTree1|Rem]) :-
    !,
    vars_in_parsetree(Vars1,ParseTree1),
    vars_in_parsetreelist(VarsRem,Rem),
    flatten([Vars1|VarsRem],IVars),
    list_to_set(IVars,Vars).

pair_val_var(X,val_var(X,Y)) :-
    nonvar(X),
    var(Y).

pair_val_var_list([],[]) :- !.
pair_val_var_list([Val|ValListRem],[Pair|PairListRem]) :-
    pair_val_var(Val,Pair),
    pair_val_var_list(ValListRem,PairListRem).

/** dpll(+Expr:expression).
 *
 * Davis–Putnam–Logemann–Loveland (DPLL) algorithm as depicted in the module description.
 */

dpll(Expr) :-
    parse_expr(ParseTree,Expr),
    simpl(ParseTree,IParseTree),
    vars_in_parsetree(Vars,IParseTree),
    pair_val_var_list(Vars,Pairs),
    dpll(Pairs,IParseTree).
dpll([ val_var(Val,Var)|PairsRem],ParseTree) :-
    boolean(Var),
    substitution(Val,Var,ParseTree,IParseTree),
    simpl(IParseTree,IIParseTree),
    dpll(PairsRem,IIParseTree).
dpll(_,'T').

boolean('T').
boolean('F').
