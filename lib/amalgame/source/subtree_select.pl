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
		 rdf_reachable(Subj, skos:broader, Concept)
		),
		URIs),!.

% List of common ancestors

source_select(VocGraph, URIs, Options) :-
	option(ancestor(ConceptList),Options),
	is_list(ConceptList),
	findall(Subj ,
		(graph_member(Subj, VocGraph),
		 rdf_reachable(Subj, skos:broader, Concept),
		 member(Concept,ConceptList)
		),
		URIs),!.

