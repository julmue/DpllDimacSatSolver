:- use_module('../src/sat_dimac_internal').
:- use_module('../src/parser_dimac_internal').

:- begin_tests(to_positive).

    test(to_positive_1) :-
        to_positive([1],[1]).

    test(to_positive_2) :-
        to_positive([-1],[1]).

    test(to_positive_3) :-
        to_positive([1,-2,3,-4],[1,2,3,4]).

:- end_tests(to_positive).

:- begin_tests(vars_dimac).

    test(vars_dimac_1) :-
        vars_dimac([],[]).

    test(vars_dimac_2) :-
        vars_dimac([[1]],[1]).

    test(vars_dimac_3) :-
        vars_dimac([[1,2,3]],[1,2,3]).

    test(vars_dimac_4) :-
        vars_dimac([[1,2,3],[3,4,5]],[1,2,3,4,5]).

    test(vars_dimac_5) :-
        vars_dimac([[1,-1]],[1]).

:- end_tests(vars_dimac).

:- begin_tests(simpl_line).

    test(simpl_line_1) :-
        simpl_line((1,'F'),[1],[]).

    test(simpl_line_2) :-
        simpl_line((1,'T'),[-1],[]).

    test(simpl_line_3) :-
        simpl_line((1,'T'),[2],[2]).

    test(simple_line_4) :-
        simpl_line((1,'F'),[1,2],[2]).

    test(simple_line_4) :-
        simpl_line((1,'T'),[-1,2],[2]).

:- end_tests(simpl_line).

:- begin_tests(simpl).

    test(simpl_1) :-
        simpl((_,_),[],[]).

    test(simpl_2) :-
        simpl((1,'T'),[[1]],[]).

    test(simpl_3) :-
        simpl((1,'F'),[[-1]],[]).

    test(simpl_4) :-
        simpl((1,'F'),[[1]],[[]]).

    test(simpl_5) :-
        simpl((1,'T'),[[-1]],[[]]).

    test(simpl_6) :-
        simpl((1,'T'),[[2]],[[2]]).

    test(simpl_7) :-
        simpl((1,'T'),[[1],[2],[-1]],[[2],[]]).

:- end_tests(simpl).


:- begin_tests(dpll).

    test(dpll_1) :-
        DIMAC = [[1]],
        vars_dimac(DIMAC,Vars),
        assign_free_vars(Vars,VarsValues),
        dpll(VarsValues,DIMAC,_).

    test(dpll_2,fail) :-
        DIMAC = [[1],[-1]],
        vars_dimac(DIMAC,Vars),
        assign_free_vars(Vars,VarsValues),
        dpll(DIMAC,_).

    test(dpll_3) :-
        read_file_to_codes('./testdata/testfile1.cnf',DimacCodes,[]),
        sat_internal(DimacCodes,_).

:- end_tests(dpll).
