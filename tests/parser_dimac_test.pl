:- use_module('../src/parser_dimac_internal').

:- begin_tests(break_at_first).

    test(break_at_first_1) :-
        break_at_first([],1,[],[]).

    test(break_at_first_2) :-
        break_at_first([1,2],2,[1],[]).

    test(break_at_first_3) :-
        break_at_first([1,2,3],2,[1],[3]).

    test(break_at_first_4) :-
        break_at_first([1],1,[],[]).

    test(break_at_first_5) :-
        break_at_first([1,1,2,3,3],2,[1,1],[3,3]).

    test(break_at_first_6) :-
        break_at_first([2],1,[2],[]).

    test(break_at_first_7) :-
        break_at_first([1,1],2,[1,1],[]).

:- end_tests(break_at_first).


:- begin_tests(break_at).

    test(break_at_1) :-
        break_at([],1,[]).

    test(break_at_2) :-
        break_at([1,2,3],2,[[1],[3]]).

    test(break_at_3) :-
        break_at([1,1,2,3,2,4,4],2,[[1,1],[3],[4,4]]).

    test(break_at_4) :-
        S1="ae\nou",
        [S]="\n",
        S2="ae",
        S3="ou",
        break_at(S1,S,[S2,S3]).

    test(break_at_4) :-
		read_file_to_codes('./testdata/lines',Codes,[]),
		S1="line1",
        S2="line2",
        [S]="\n",
        break_at(Codes,S,[S1,S2]).

:- end_tests(break_at).


:- begin_tests(normalize).

    test(normalize1) :-
        normalize([" String "],["String"]).

    test(normalize2) :-
        normalize([],[]).

:- end_tests(normalize).


:- begin_tests(decomment).

    test(decomment_1) :-
        In = ["String1","c comment", "String2"],
        Out = ["String1", "String2"],
        decomment(In,Out).

    test(decomment_2) :-
        In = ["String1","   c comment  ", "String2"],
        Out = ["String1", "String2"],
        decomment(In,Out).

    test(decomment_3) :-
        decomment([],[]).

:- end_tests(decomment).


:- begin_tests(convert_line).

    test(convert_line_1) :-
        convert_line("1",[1]).

    test(convert_line_2) :-
        convert_line("-1",[-1]).

    test(convert_line_3) :-
        convert_line("1 -2 3",[1,-2,3]).

:- end_tests(convert_line).


:- begin_tests(parse_dimac).

    test(parse_dimac_1) :-
        parse_dimac("header \n",[]).

    test(parse_dimac_2) :-
        parse_dimac("c \n header \n",[]).

    test(parse_dimac_3) :-
        parse_dimac("c \n header \n c",[]).

    test(parse_dimac_4) :-
        parse_dimac("header \n 1 \n",[[1]]).

    test(parse_dimac_5) :-
        parse_dimac("header \n 1 -2 3 \n",[[1,-2,3]]).

    test(parse_dimac_6) :-
        parse_dimac("header \n 1 -2 3 \n 4 -5 -6 \n",[[1,-2,3],[4,-5,-6]]).

    test(parse_dimac_7) :-
        In = "c Here is a comment. \n p cnf 5 3 \n 1 -5 4 0 \n 3 4  2 1",
        parse_dimac(In,[[1,-5,4,0],[3,4,2,1]]).

    test(parse_dimac_8) :-
	    read_file_to_codes('./testdata/2.cnf',Codes,[]),
        parse_dimac(Codes,[[1,-5,4,0],[3,4,2,1]]).

:- end_tests(parse_dimac).



/*
:- begin_tests(parser_dimac).

test(parser_dimac_1) :-
	read_file_to_codes('./testdata/1.cnf',String,[]),
    parse_dimac(String,[]).

test(parser_dimac_2) :-
	read_file_to_codes('./testdata/2.cnf',String,[]),
    parse_dimac(String,[[1,-5,4,0]]).

:- end_tests(parser_dimac).
*/

%% helper predicates -- not necessary??
testdata(File,String) :-
	read_file_to_codes(File,Codes,[]),
	atom_codes(Atom,Codes),
	atom_string(Atom,String).
