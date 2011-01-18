:- module(align_exclude,
	  [
	  ]
	 ).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/alignment_graph)).

:- public source_select/3.
:- multifile amalgame:component/2.

amalgame:component(source_select, align_exlude(align_source, uris, [exclude(align_graph)])).

%%	source_select(+Source, +Target, -Alignment, +Options)
%
%	Enumerate over members of Source.

source_select(Source, URIs, Options) :-
	option(exclude(Alignments), Options),
	maplist(align_source, Alignments, Es),
	findall(S, graph_member(S, Source), Ss),
	subtract(Ss, Es, URIs).

align_source(align(S,_,_), S).
