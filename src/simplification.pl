:- module(simplification,[simpl/2]).

/** <module> Simplification Of Logical Terms Represented As Parse Trees.

Simplification of logical terms represented as parsetrees.
According to the truth functions of the logical operators AND, OR, NOT.
No further simplification is implemented e.g. DeMorgan-laws.
*/


/**
 * simpl(+Old:parse_tree,?New:parse_tree).
 *
 * Old is simplified to New according to the truth functions of basic logic operators.
 *
 */

%% determined simplification
simpl(or('T',_),'T') :- !.
simpl(or(_,'T'),'T') :- !.
simpl(or('F','F'),'F') :- !.

simpl(and('T','T'),'T') :- !.
simpl(and('F',_),'F') :-!.
simpl(and(_,'F'),'F') :-!.

simpl(not('F'),'T') :-!.
simpl(not('T'),'F') :-!.


%% non-determined simplification
simpl(or(X,'F'),Res) :- simpl(or('F',X), Res), !.
simpl(or('F',X),Res) :-
    dif(X,'T'),
    dif(X,'F'),
    !,
    simpl(X,IX),
    ( (IX = 'T' ; IX = 'F') ->
        simpl(or('F',IX),Res)
    ;
        Res = or('F',IX)
    ).

simpl(and(X,'T'),Res) :- simpl(and('T',X), Res), !.
simpl(and('T',X),Res) :-
    dif(X,'T'),
    dif(X,'F'),
    !,
    simpl(X,IX),
    ( (IX = 'T' ; IX = 'F') ->
        simpl(and('T',IX),Res)
    ;
        Res = and('T',IX)
    ).

simpl(Term, Res) :-
    functor(Term,F,2),
    arg(1,Term,X),
    arg(2,Term,Y),
    \+ member(X,['T','F']),
    \+ member(Y,['T','F']),
    !,
    simpl(X,X1),
    simpl(Y,Y1),
    functor(Term1,F,2),
    arg(1,Term1,X1),
    arg(2,Term1,Y1),
    ( (member(X1,['T','F']); member(Y1,['T','F'])) ->
        simpl(Term1,Res)
    ;
        Res = Term1
    ).

simpl(not(X),Res) :-
	dif(X,'T'),
	dif(X,'F'),
	simpl(X,IX),

    ( (IX = 'T' ; IX = 'F') ->
        simpl(not(IX),Res)
	;
		Res = IX
	).


simpl(v(X),v(X)) :- !.
simpl('T','T') :- !.
simpl('F','F') :- !.
