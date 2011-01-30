:- module(align_exclude,
	  [
	  ]
	 ).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/alignment_graph)).

:- public source_select/3.
:- multifile amalgame:component/2.

amalgame:component(source_select, align_exlude(align_source, uris, [exclude(align_graph)])).

%%	source_select(+Source, +URIs, +Options)
%
%	URIs is a list of sorted Resources, representing all Concepts in
%	Source, excluding the sources in the ExcludeAlignment graph.
%	The latter is passed as an Option:
%	* exclude(ExcludeAlignment)
%

source_select(Source, URIs, Options) :-
	option(exclude(Alignments), Options),
	maplist(align_source, Alignments, Es),
	findall(S, graph_member(S, Source), Ss),
	length(Es, NEs),
	length(Ss, NSs),
	NLeft is NSs - NEs,
	debug(align, 'Excluding ~w alignments from ~w, leaving ~w',[NEs, NSs, NLeft]),
	sort(Ss, SsSorted),
	sort(Es, EsSorted),
	ord_subtract(SsSorted, EsSorted, URIs).

align_source(align(S,_,_), S).
