:- module(related_generator,
	  []).

:- use_module(related_match).
:- use_module(generator_snd_input).

:- public amalgame_module/1.
:- public matcher/4.
:- public parameter/4.

amalgame_module(amalgame:'RelatedMatcher').

parameter(steps, integer, 1,
	  'depth of search, defaults to 1, e.g. directly related only').

%%	matcher(+Source, +Target, -Mappings, +Options)
%
%	Mappings is a list of matches between instances of Source and
%	Target.

matcher(Source, Target, Mappings, Options) :-
	generator_snd_input(related_match, Source, Target, Mappings, Options).

