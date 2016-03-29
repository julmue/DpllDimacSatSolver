:- module(parser_dimac_internal,
    [ parse_dimac/2
    , decomment/2
    , normalize/2
    , filter_empty/2
    , convert_lines/2
    , convert_line/2
    , atom_number_line/2
    , break_at/3
    , break_at_first/4
    ]).

:- use_module(library(lists)).

/*
for the grammar definition consult /docu/DIMAC_CNF.md.
*/

parse_dimac(In,Out) :-
    [Sep] = "\n",
    break_at(In,Sep,I1),
    normalize(I1,I2),
    decomment(I2,I3),
    strip_dimac_header(I3,I4),
    filter_empty(I4,I5),
    convert_lines(I5,Out).

break_at([],_,[]) :- !.
break_at(String,Seperator,[Line1 | Lines]) :-
    break_at_first(String,Seperator,Line1,RemString),
    break_at(RemString,Seperator,Lines).

break_at_first(String,Seperator,Head,Tail) :-
    break_at_first(String,Seperator,[],Head,Tail).
break_at_first([C|Cs0],Sep, Acc, Head, Tail ) :-
    dif(C,Sep),
    break_at_first(Cs0,Sep,[C|Acc], Head, Tail).
break_at_first([C|Cs0],C,Acc,Head,Cs0) :-
    reverse(Acc,Head).
break_at_first([],_,Acc,Head,[]) :-
    reverse(Acc,Head).

normalize([Line0|Lines0],[Line1|Lines1]) :-
    atom_codes(LI1,Line0),
    normalize_space(atom(LI2),LI1),
    atom_codes(LI2,Line1),
    normalize(Lines0,Lines1).
normalize([],[]).

decomment([Line0|Lines0],Lines1) :-
    atom_codes(LI1,Line0),
    normalize_space(atom(LI2),LI1),
    %% 99 is the code for c
    atom_codes(LI2,[99|_]), !,
    decomment(Lines0,Lines1).
decomment([Line|Lines0],[Line|Lines1]) :-
    decomment(Lines0,Lines1).
decomment([],[]).

strip_dimac_header([_|Tail],Tail).

filter_empty([],[]).
filter_empty([[]|Lines0],Lines1) :-
    filter_empty(Lines0,Lines1).
filter_empty([Line|Lines0],[Line|Lines1]) :-
    dif([],Line),
    filter_empty(Lines0,Lines1).

convert_lines([L0|Ls0],[L1|Ls1]) :-
    convert_line(L0,L1),
    convert_lines(Ls0,Ls1).
convert_lines([],[]).

convert_line(Line0,Line1) :-
    atom_codes(LI1,Line0),
    normalize_space(atom(LI2),LI1),
    atomic_list_concat(LI3,' ',LI2),
    atom_number_line(LI3,Line1).

atom_number_line([],[]).
atom_number_line([A|As],[N|Ns]) :-
    atom_number(A,N),
    atom_number_line(As,Ns).
