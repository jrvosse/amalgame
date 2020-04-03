:- module(preloaded_selecter,[]).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(amalgame/mapping_graph)).

:- public amalgame_module/1.
:- public selecter/5.
:- public parameter/4.

amalgame_module(amalgame:'SelectPreLoadedSelecter').

parameter(name, oneof(Loaded), 'no default',
	  'url of named mapping graph') :-
	findall(M,
		(   rdfs_individual_of(M, amalgame:'LoadedMapping'),
		    rdf_graph(M),
		    rdf_graph_property(M, triples(N)),
		    N > 0
		), Loaded).

selecter(In, Sel, Dis, [], Options) :-
	option(name(Graph), Options),
	selecter_(Graph, In, Sel, Dis).

selecter_(_Graph, [], [], []).
selecter_(Graph, [Hin|Tin], Sel, Dis) :-
	Hin = align(S,T,P),
	(   has_correspondence(align(S, T, PGraph), Graph)
	->  append(PGraph, P, Pmerged),
	    Sel = [ align(S, T, Pmerged) | Tsel],
	    Dis = Tdis
	;   Sel = Tsel,
	    Dis = [ align(S,T,P) | Tdis]
	),
	selecter_(Graph, Tin, Tsel, Tdis).


