:- module(ancestor_generator,
	  []).

:- use_module(library(amalgame/ag_strategy)).
:- use_module(ancestor_match).
:- use_module(generator_snd_input).

:- public amalgame_module/1.
:- public matcher/4.
:- public parameter/4.

amalgame_module(amalgame:'AncestorMatcher').

parameter(steps, integer, 1,
	  'depth of search, defaults to 1, e.g. direct parents only').

%%	matcher(+Source, +Target, -Mappings, +Options)
%
%	Mappings is a list of matches between instances of Source and
%	Target.

matcher(Source, Target, Mappings, Options) :-
	generator_snd_input(ancestor_match, Source, Target, Mappings, Options).

