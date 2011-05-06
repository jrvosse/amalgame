:- module(descendent_match,
	  []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/vocabulary)).
:- use_module(library(amalgame/map)).

:- public amalgame_module/1.
:- public filter/3.
:- public matcher/4.
:- public parameter/4.

amalgame_module(amalgame:'DescendentMatcher').
amalgame_module(amalgame:'DescendentFilter').

parameter(graph, atom, 'DEFAULT_GRAPH', 'named graph to query for descendents, defaults to full repository').

%%	filter(+MappingsIn, -MappingsOut, +Options)
%
%	Filter mappings based on exact matching of labels.

filter([], [], _).
filter([align(S,T,P)|Cs], [C|Mappings], Options) :-
	(   T = scheme(_)
	->  match(align(S,_,P), C, Options),
	    C=align(_,T2,_),
	    vocab_member(T2, T)
	;   match(align(S,T,P), C, Options)
	),
	!,
	filter(Cs, Mappings, Options).
filter([_|Cs], Mappings, Options) :-
	filter(Cs, Mappings, Options).


%%	matcher(+Source, +Target, -Mappings, +Options)
%
%	Mappings is a list of matches between instances of Source and
%	Target.

matcher(Source, Target, Mappings, Options) :-
	findall(M, align(Source, Target, M, Options), Mappings).

align(Source, Target, Match, Options) :-
	vocab_member(S, Source),
	match(align(S,T,[]), Match, Options),
	vocab_member(T, Target).


match(align(S, T, Prov0), align(S, T, [Prov|Prov0]), Options) :-
	(   option(graph(Graph), Options, 'DEFAULT_GRAPH'), Graph \== 'DEFAULT_GRAPH'
	->  true
	;   Graph = _
	),

	descendent(S, DescS, R1),
	descendent(T, DescT, R2),
	has_map([DescS, DescT],_, O, Graph),
	memberchk(relation(R), O),
	memberchk(provenance(P), O),
	rdf_equal(PM, amalgame:descendent_match),
	Prov = [method(descendent_match),
		graph([R1,R2,rdf(P,PM,R)])
	       ].

descendent(R, Child, rdf_reachable(R, Prop, Child)) :-
	rdf_equal(skos:narrower, Prop),
	rdf_reachable(R, Prop, Child),
	\+ R == Child.
descendent(R, Child, rdf_reachable(Child, Prop, R)) :-
	rdf_equal(skos:broader, Prop),
	rdf_reachable(Child, Prop, R),
	\+ R == Child,
	\+ rdf_reachable(R, skos:narrower, Child).
