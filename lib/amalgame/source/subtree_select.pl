:- module(subtree_select,[]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/alignment_graph)).

:- public source_select/3.
:- multifile amalgame:component/2.

amalgame:component(source_select, voc_objprop_split(alignment_graph, selected_URI_list, [])).

%%	partition(+Input, -Output, +Options)
%
%	Output a list of graphs where the first element contains the
%	vocabulary graph with concepts that have a common
%	ancestor through skos:broader. The (list of) common ancestor(s)
%	is given in Options as ancestor(Concept).
%


% One ancestor
source_select(VocGraph, URIs, Options) :-
	option(ancestor(Concept),Options),
	atom(Concept),
	findall(Subj ,
		(graph_member(Subj, VocGraph),
		 in_skos_subtree(Subj, Concept)
		),
		URIs),!.


% List of common ancestors
source_select(VocGraph, URIs, Options) :-
	option(ancestor(ConceptList),Options),
	is_list(ConceptList),
	findall(Subj ,
		(graph_member(Subj, VocGraph),
		 in_skos_subtree(Subj, Concept),
		 member(Concept,ConceptList)
		),
		URIs),!.


in_skos_subtree(Concept, TopConcept):-
	rdf_reachable(Concept, skos:broader, TopConcept).
in_skos_subtree(Concept, TopConcept):-
	rdf_reachable(Concept, skos:broaderTransitive, TopConcept).

% propably redundant because of owl:inverse
in_skos_subtree(Concept, TopConcept):-
	rdf_reachable(Concept, skos:narrower, TopConcept).
in_skos_subtree(Concept, TopConcept):-
	rdf_reachable(Concept, skos:narrowerTransitive, TopConcept).
