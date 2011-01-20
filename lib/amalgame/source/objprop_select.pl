:- module(objprop,[]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/alignment_graph)).

:- public source_select/3.
:- multifile amalgame:component/2.

amalgame:component(source_select, voc_objprop_split(alignment_graph, selected_URI_list, [])).

%%	partition(+Input, -Output, +Options)
%
%	Output a list of graphs where the first element contains the
%	vocabulary or alignment graph with concepts that have a
%	matching object-property value pair. The second element contain
%	the concepts for which this does not hold.
%
%	The property and its value are given in Options as splitprop(P)
%	and splitval(Val)



source_select(VocGraph, URIs, Options) :-
	option(splitprop(SplitProp),Options),
	option(splitval(SplitVal),Options),

	findall(Subj ,
		(graph_member(Subj, VocGraph),
		 rdf_has(Subj, SplitProp, SplitVal)
		),
		URIs).



