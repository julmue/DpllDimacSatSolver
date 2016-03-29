:- use_module('./sat_dimac_internal').
:- use_module(library(readutil)).

:- initialization(main).

%% swipl -q -t main -f sat_dimac.pl <path/to/input.cnf>
main :-
    current_prolog_flag(argv,Args),
    (Args = [] ->
        write('Error: no file specified!'),
        nl,
        halt
    ; length(Args,LArgs), LArgs > 1 ->
        write('Error: too many argument files! (specify exactly 1 file)'),
        nl,
        halt
    ; Args = [File] ->
        read_file_to_codes(File,CodesDimac,[]),
            (sat_internal(CodesDimac,Model) ->
               write(true), nl,
               write(Model), nl,
               halt
            ; write(false), nl,
               halt
            )
    ).
