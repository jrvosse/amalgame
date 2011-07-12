:- module(preloaded_matcher,[]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/map)).
:- use_module(library(amalgame/vocabulary)).

:- public amalgame_module/1.
:- public matcher/4.
:- public parameter/4.

amalgame_module(amalgame:'SelectPreLoaded').

parameter(name, 'atom', 'no default',
	  'url of named mapping graph').

%%	selecter(+Mapping, -Selected, -Discarded, -Undecided, +Options)
%
%	Selected contains only the unique correspondences between
%	a source and target concept.

matcher(Source, Target, Mapping, Options) :-
	option(name(Graph), Options),
	findall(align(S,T,P),
		(   has_map([S,T],_,Graph),
		    vocab_member(S, Source),
		    vocab_member(T, Target),
		    P=[preloaded(Graph)]
		),
		Mapping).



