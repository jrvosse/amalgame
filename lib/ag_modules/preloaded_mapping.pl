:- module(preloaded_matcher,[]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(amalgame/map)).
:- use_module(library(amalgame/vocabulary)).

:- public amalgame_module/1.
:- public matcher/4.
:- public filter/3.
:- public parameter/4.

amalgame_module(amalgame:'SelectPreLoaded').

parameter(name, oneof(Loaded), 'no default',
	  'url of named mapping graph') :-
	findall(M,
		(   rdfs_individual_of(M, amalgame:'LoadedMapping'),
		    rdf_graph(M),
		    rdf_graph_property(M, triples(N)),
		    N > 0
		), Loaded).

matcher(Source, Target, Mapping, Options) :-
	option(name(Graph), Options),
	findall(Correspondence,
		c_from_graph(Correspondence, Source, Target, Graph),
		Mapping0),
	sort(Mapping0, Mapping).

filter(In, Out, Options) :-
	option(name(Graph), Options),
	filter_(Graph, In, Out).

filter_(_Graph, [], []).
filter_(Graph, [Hin|Tin], Out) :-
	Hin = align(S,T,P),
	(   has_correspondence(align(S, T, PGraph), Graph)
	->  append(PGraph, P, Pmerged),
	    Out = [ align(S, T, Pmerged) | Tout]
	;   Out = Tout
	),
	filter_(Graph, Tin, Tout).

c_from_graph(align(S,T,P), Source, Target, Graph) :-
	has_correspondence(align(S,T,P), Graph),
	vocab_member(S, Source),
	vocab_member(T, Target).



