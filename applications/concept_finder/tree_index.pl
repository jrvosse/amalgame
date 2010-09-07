:- module(tree_index,
	  [ build_skos_tree_index/0,
	    build_skos_tree_index/1,   % +ConceptScheme
 	    flush_tree_index/0,
	    tree_index/3
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(debug)).

:- dynamic
        tree_index/3.

:- debug(tree_index).

%%	flush_tree_index
%
%	Remove all assert indexes.

flush_tree_index :-
	retractall(tree_index(_,_,_)).

%%	build_skos_tree_index.
%%      build_skos_tree_index(+ConceptScheme).
%
%	Build a tree index for the concepts in ConceptScheme.

build_skos_tree_index :-
	forall(rdf(ConceptScheme, rdf:type, skos:'ConceptScheme'),
	       build_skos_tree_index(ConceptScheme)).

build_skos_tree_index(Scheme) :-
	debug(tree_index, 'build tree index for: ~w', Scheme),
	findall(C, top_concept(Scheme, C), Cs),
	build_tree_index(Cs, 0, _).

build_tree_index([], N, N).
build_tree_index([C|Cs], Start, Last) :-
	Next is Start + 1,
	node_index(C, Next, End),
	assert(tree_index(C, Next, End)),
	build_tree_index(Cs, End, Last).

node_index(R, Start, End) :-
	findall(C, narrower_concept(R, C), Cs),
	build_tree_index(Cs, Start, End).


%%	top_concept(+ConceptScheme, -Concept)
%
%	True if Concept is a skos:hasTopConcept of ConceptScheme, or
%	inversely by skos:topConceptOf

top_concept(ConceptScheme, Concept) :-
	rdf(ConceptScheme, skos:hasTopConcept, Concept).
top_concept(ConceptScheme, Concept) :-
	rdf(Concept, skos:topConceptOf, ConceptScheme),
	\+ rdf(ConceptScheme, skos:hasTopConcept, Concept).

%%	narrower_concept(+Concept, -Narrower)
%
%	True if Narrower is related to Concept by skos:narrower or
%	inversely by skos:broader.

narrower_concept(Concept, Narrower) :-
	rdf_has(Concept, skos:narrower, Narrower).
narrower_concept(Concept, Narrower) :-
	rdf_has(Narrower, skos:broader, Concept),
	\+ rdf_has(Concept, skos:narrower, Narrower).
