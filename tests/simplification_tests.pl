:- use_module('../src/simplification').

:- begin_tests(simpl).

% tests for the determined simplifications

test(or_1) :-
    simpl(or('T',v(x)),'T').

test(or_2) :-
    simpl(or(v(x),'T'),'T').

test(and_1) :-
    simpl(and('F',v(x)),'F').

test(and_2) :-
    simpl(and(v(x),'F'),'F').

test(not_1) :-
    simpl(not('T'),'F').

test(not_2) :-
    simpl(not('F'),'T').

% tests for the non-determined simplification

test(or_3) :-
    simpl(or('T','T'),'T').

test(or_4) :-
    simpl(or('F','T'),'T').

test(or_5) :-
    simpl(or('T','F'),'T').

test(or_6) :-
    simpl(or('F','F'), 'F').


test(and_3) :-
    simpl(and('T','T'),'T').

test(and_4) :-
    simpl(and('F','T'),'F').

test(and_5) :-
    simpl(and('T','F'),'F').

test(and_6) :-
    simpl(and('F','F'), 'F').


test(not_3) :-
    simpl(not(and('F','F')),'T').

test(not_4) :-
    simpl(not(and('T','T')),'F').


test(or_7) :-
    simpl(or('F',v(x)),or('F',v(x))).

test(or_8) :-
    simpl(or(v(x),'F'),or('F',v(x))).

test(or_9) :-
    simpl(or(v(x),v(y)),or(v(x),v(y))).

test(and_7) :-
    simpl(and('T',v(x)),and('T',v(x))).

test(and_8) :-
    simpl(and(v(x),'T'),and('T',v(x))).

test(and_10) :-
    simpl(and(v(x),v(y)),and(v(x),v(y))).

:- end_tests(simpl).
