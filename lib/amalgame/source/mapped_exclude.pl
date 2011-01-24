:- module(mapped_exclude, []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/alignment_graph)).
:- use_module(library(amalgame/map)).

:- public source_select/3.
:- multifile amalgame:component/2.

amalgame:component(source_select, mapped_exclude(align_source, uris, [])).

%%	source_select(+Source, +Target, -Alignment, +Options)
%
%	Enumerate over members of Source.

source_select(Source, Ss, _Options) :-
 	findall(S, ( graph_member(S, Source),
		     \+ has_map([S,_], _, _)
		   ),
		Ss).
