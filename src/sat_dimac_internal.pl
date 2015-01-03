:- module(sat_dimac_internal,[  sat_internal/3,
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




/** vars_dimac
 *
 * Extracts a list of variables from DIMAC CNF
 */


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



/** assign_free_vars
 *
 * Assigns a free Prolog variable to every element of a list.
 */

assign_free_vars([Head0|Tail0],[(Head0,_)|Tail1]) :-
    assign_free_vars(Tail0,Tail1).
assign_free_vars([],[]).


/** simpl
*/

%% case 1: var is 'T' and appears unnegated in line - skip line
simpl((Var,Value),[Line0|Lines0],Result) :-
    NegVar is 0 - Var,
    (   (member(Var,Line0), Value = 'T') ->
            simpl((Var,Value),Lines0,Lines1),
            Result = Lines1
    ;   (member(NegVar,Line0), Value = 'F') ->
            simpl((Var,Value),Lines0,Lines1),
            Result = Lines1
    ;   simpl_line((Var,Value),Line0,Line1),
        simpl((Var,Value),Lines0,Lines1),
            Result = [Line1|Lines1]
    ).
simpl((_,_),[],[]).

%% case : var is 'F' and appears unnegated in line - remove var from line
simpl_line((Var,'F'),Line0,Line1) :-
    member(Var,Line0),
    subtract(Line0,[Var],Line1).

%% case:
simpl_line((Var,'T'),Line0,Line1) :-
    NegVar is 0 - Var,
    member(NegVar,Line0),
    subtract(Line0,[NegVar],Line1).

simpl_line((Var,_),Line0,Line0) :-
    NegVar is 0 - Var,
    \+ member(Var,Line0),
    \+ member(NegVar,Line0).


dpll(Dimac, Bool ,Model) :-
    vars_dimac(Dimac,Vars),
    assign_free_vars(Vars,VarsValues),
    (   dpll(VarsValues,Dimac) ->
        VarsValues = Model,
        Bool = true
    ;   Model = [],
        Bool = false
    ).

dpll([(Var,Value)|VarValues], Dimac0) :-
    boolean(Value),
    simpl((Var,Value),Dimac0,Dimac1),
    dpll(VarValues,Dimac1).
dpll(_,[]).

sat_internal(DimacCodes,Bool,Model) :-
    parse_dimac(DimacCodes,Dimac),
    dpll(Dimac,Bool,Model).



boolean('T').
boolean('F').
