:- module(autocomplete_hook, []).

/* This hook allows the autocomplete cpack to suggest skos concepts
 * from virtual vocabulary schemes created by amalgame.
 */

:- use_module(library(amalgame/vocabulary)).

:- multifile
        cliopatria:in_scheme/2.  % filter hook

cliopatria:in_scheme(Concept, Scheme) :-
	vocab_member(Concept, Scheme).
