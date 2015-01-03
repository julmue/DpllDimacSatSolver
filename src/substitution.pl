:- module(substitution,[substitution/4]).

/** <module> Simultaneous Substitution Of Subterms In Prolog-Terms.
*/

/**
 * substitution(+Old:term,+New:term,+OldTerm:term,-NewTerm:term).
 *
 * NewTerm is the result of replacing all occurrences of Old in OldTerm by New.
 */

substitution(Old,New,Old,New) :- !.
substitution(Old,_,Term,Term) :-
    dif(Old,Term),
    atomic(Term),
    !.
substitution(Old,New,Term,Term1) :-
    dif(Old,Term),
    compound(Term),
    !,
    functor(Term,F,N),
    functor(Term1,F,N),
    substitution(N,Old,New,Term,Term1).
substitution(N,Old,New,Term,Term1) :-
    N > 0,
    !,
    arg(N,Term,Arg),
    substitution(Old,New,Arg,Arg1),
    arg(N,Term1,Arg1),
    N1 is N-1,
    substitution(N1,Old,New,Term,Term1).
substitution(0,_,_,_,_) :- !.
