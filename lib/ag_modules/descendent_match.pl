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

parameter(graph, atom, 'DEFAULT_GRAPH',
	  'named graph to query for descendents, defaults to full repository').
parameter(steps, integer, 1, 'depth of search, defaults to 1, e.g. direct children only').

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
	findall(M, align(Source, Target, M, Options), Mappings0),
	sort(Mappings0, Mappings).

align(Source, Target, Match, Options) :-
	vocab_member(S, Source),
	vocab_member(T, Target),
	match(align(S,T,[]), Match, Options).


match(align(S, T, Prov0), align(S, T, [Prov|Prov0]), Options) :-
	(   option(graph(Graph), Options, 'DEFAULT_GRAPH'), Graph \== 'DEFAULT_GRAPH'
	->  true
	;   Graph = _
	),
	option(steps(MaxSteps), Options),
	descendent(S, MaxSteps, DescS, R1, _Steps1),
	descendent(T, MaxSteps, DescT, R2, _Steps2),
	has_correspondence(align(DescS, DescT,_), Graph),
	Prov = [method(descendent_match),
		graph([R1,R2])
	       ].

	/* FIXME: need to make a decision about what to do with align:relation ...
        has_map([DescS, DescT],_, O, Graph),
	memberchk(relation(R), O),
	Prov = [method(descendent_match),
		graph([R1,R2,rdf(DescS, R, DescT)])
	       ].
        */

descendent(R, MaxSteps, Child, rdf_reachable(R, Prop, Child), Steps) :-
	rdf_equal(skos:narrower, Prop),
	rdf_reachable(R, Prop, Child, MaxSteps, Steps),
	\+ R == Child.
descendent(R, MaxSteps, Child, rdf_reachable(Child, Prop, R), Steps) :-
	rdf_equal(skos:broader, Prop),
	rdf_reachable(Child, Prop, R, MaxSteps, Steps),
	\+ R == Child,
	\+ rdf_reachable(R, skos:narrower, Child).
