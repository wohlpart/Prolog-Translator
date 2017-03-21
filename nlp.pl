% :- module(csse403nlp,[parse/2,translate/2]).
% we should use a module,really, but it wasn't obvious how to
% make it so the that the operator => displays correctly when it
% is in a module.

% this creates a new => operator to have the same priority as +
% seems to make it behave as you would expect.
:- op(200, xfx, =>).
=>(_,_).

% your code goes below
% the tests are in another file

is_noun(plural, apples).
is_noun(singular, apple).
is_noun(plural, boys).
is_noun(singular, boy).
is_noun(plural, girls).
is_noun(singular, girl).

is_verb(singular, runs, it).
is_verb(plural, run, it).
is_verb(singular, dances, it).
is_verb(plural, dance, it).
is_verb(singular, likes, t).
is_verb(plural, like, t).
is_verb(singular, hates, t).
is_verb(plural, hate, t).
is_verb(singular, respects, t).
is_verb(plural, respect, t).

is_relcl(that). is_relcl(which).

parse(X,statement(NT,VT)) :-append(N,V,X),
                            noun_phrase(Sop, NT, N),
                            verb_phrase(Sop, VT, V).

noun_phrase(_, all(noun(Noun)), [all,Noun]) :- is_noun(plural, Noun).
noun_phrase(Sop, some(noun(Noun)), [some,Noun]) :- is_noun(Sop, Noun).
noun_phrase(Sop, some(RP), [some|Phrase]) :- relcl_phrase(Sop, RP, Phrase).
noun_phrase(_, all(RP), [all|Phrase]) :- relcl_phrase(plural, RP, Phrase).
relcl_phrase(Sop, relcl(noun(Noun), VT), [Noun|[R|Verb]]) :-
                  is_noun(Sop, Noun),
                  is_relcl(R),
                  verb_phrase(Sop, VT, Verb).
relcl_phrase(Sop, relcl(noun(Noun), NPT, verb(TV)), [Noun|[R|[P|[NP,TV]]]]) :-
                  is_noun(Sop, Noun),
                  is_relcl(R),
                  noun_phrase(S2, NPT, [P,NP]),
                  is_verb(S2, TV, t).

verb_phrase(Sop, verb(Verb, NT), [Verb|Noun]) :- is_verb(Sop, Verb, t),
                                                  noun_phrase(_, NT, Noun).
verb_phrase(Sop, verb(Verb), [Verb]) :- is_verb(Sop, Verb, it).


translate(statement(X, Y), Z) :- tr((X,Y), Z, 1).

tr((all(NT),VT), all(1, RN=>RV), Num) :- noun_translate(NT, RN, Num),
                                        verb_translate(VT, Num, RV).
tr((some(NT),VT), exists(1, RN+RV), Num) :- noun_translate(NT, RN, Num),
                                        verb_translate(VT, Num, RV).

noun_translate(noun(NT), Noun, Num) :-  ((case(NT, X), Noun =.. [X, Num]) ;
                                    (is_noun(singular, NT),
                                      Noun =.. [NT, Num])).
noun_translate(relcl(NT, VT), NR + VR, Num) :- noun_translate(NT, NR, Num),
                                  verb_translate(VT, Num, VR).
noun_translate(relcl(NT, some(N2), verb(VT)), (Noun1+exists(Num1, Noun2+Verb)), Num) :-
                                              Num1 is Num + 1,
                                              noun_translate(NT, Noun1, Num),
                                                noun_translate(N2, Noun2, Num1),
                                                ((case(VT, X), Verb =.. [X, Num1, Num]) ;
                                                (is_verb(plural, VT, _), Verb =.. [VT, Num1, Num])).
noun_translate(relcl(NT, all(N2), verb(VT)), (Noun1+all(Num1, Noun2=>Verb)), Num) :-
                                              Num1 is Num + 1,
                                              noun_translate(NT, Noun1, Num),
                                                noun_translate(N2, Noun2, Num1),
                                                ((case(VT, X), Verb =.. [X, Num1, Num]) ;
                                                (is_verb(plural, VT, _), Verb =.. [VT, Num1, Num])).

verb_translate(verb(Verb), Num, Result) :- (case(Verb, X), Result =.. [X, Num]) ;
                                                (is_verb(plural, Verb, _), Result =.. [Verb, Num]).

verb_translate(verb(Verb, all(NP)), Num, all(Num1, NR=>Result )) :- Num1 is Num+1,
                                          ((case(Verb, X), Result =.. [X, Num, Num1]) ;
                                          (is_verb(plural, Verb, _), Result =.. [Verb, Num, Num1])),
                                                noun_translate(NP, NR, Num1).

verb_translate(verb(Verb, some(NP)), Num, exists(Num1, NR+Result )) :- Num1 is Num+1,
                                            ((case(Verb, X), Result =.. [X, Num, Num1]) ;
                                            (is_verb(plural, Verb, _), Result =.. [Verb, Num, Num1])),
                                                noun_translate(NP, NR, Num1).

do_nlp :- get_string(X), nlp_help(X).

nlp_help(X) :- X = "done".
nlp_help(X) :- atomic_list_concat(L, ' ', X), \+ X= "done",
            \+parse(L, _), write("invalid sentance"), nl, do_nlp.
nlp_help(X) :- atomic_list_concat(L, ' ', X),
            parse(L, Y), translate(Y,Z), write(Z), nl, do_nlp.

% get_string method taken from the course notes
get_string(X) :- get_string_helper(Y), string_codes(X,Y).
get_string_helper(X) :- get_code(Y), (Y = 10, X = []; get_string_helper(Z), X = [Y|Z]), !.


case(boys, boy).
case(girls, girl).
case(apples, apple).
case(runs, run).
case(dances, dance).
case(likes, like).
case(hates, hate).
case(respects, respect).
