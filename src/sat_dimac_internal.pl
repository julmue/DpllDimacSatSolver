:- module(sat_dimac_internal,[  sat_internal/2,
                                dpll/2,
                                dpll/3,
                                vars_dimac/2,
                                to_positive/2,
                                assign_free_vars/2,
                                simpl/3,
                                simpl_line/3
                                ]).

:- use_module(library(lists)).
:- use_module('./parser_dimac_internal').
:- use_module(library(readutil)).


vars_dimac(DIMAC,Unique) :-
    flatten(DIMAC,I1),
    to_positive(I1,I2),
    list_to_set(I2,Unique).


to_positive([Z|Zs],[N|Ns]) :-
    (   (0 > Z) ->
        N is 0 - Z
    ;   N = Z),
    to_positive(Zs,Ns).
to_positive([],[]).


assign_free_vars([Head0|Tail0],[(Head0,_)|Tail1]) :-
    assign_free_vars(Tail0,Tail1).
assign_free_vars([],[]).


simpl((Var,'T'),[Line0|Lines0], Lines1) :-
	member(Var,Line0),
	simpl((Var,'T'),Lines0,Lines1).
simpl((Var,'F'),[Line0|Lines0], Lines1) :-
	NegVar is 0 - Var,
	member(NegVar,Line0),
	simpl((Var,'F'),Lines0,Lines1).
simpl((Var,Value),[Line0|Lines0], [Line1|Lines1]) :-
	NegVar is 0 - Var,
	\+ (member(Var,Line0), Value = 'T'),
	\+ (member(NegVar,Line0), Value = 'F'),
	simpl_line((Var,Value),Line0,Line1),
	simpl((Var,Value),Lines0,Lines1).
simpl((_,_),[],[]).


simpl_line((Var,'F'),Line0,Line1) :-
    member(Var,Line0),
    subtract(Line0,[Var],Line1).
simpl_line((Var,'T'),Line0,Line1) :-
    NegVar is 0 - Var,
    member(NegVar,Line0),
    subtract(Line0,[NegVar],Line1).
simpl_line((Var,_),Line0,Line0) :-
    NegVar is 0 - Var,
    \+ member(Var,Line0),
    \+ member(NegVar,Line0).


dpll(Dimac, Model) :-
    vars_dimac(Dimac,Vars),
    assign_free_vars(Vars,VarsValues),
    dpll(VarsValues,Dimac,[]),
	Model = VarsValues.

dpll([(Var,Value)|VarValues],Dimac0,Dimac1) :-
    boolean(Value),
    simpl((Var,Value),Dimac0,IDimac),
    dpll(VarValues,IDimac,Dimac1).
dpll([],Dimac,Dimac).


sat_internal(DimacCodes,Model) :-
    parse_dimac(DimacCodes,Dimac),
    dpll(Dimac,Model).


boolean('T').
boolean('F').
