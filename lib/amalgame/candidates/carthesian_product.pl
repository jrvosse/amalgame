:- module(carthesian_product, []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/alignment_graph)).

:- public candidate/4.
:- multifile amalgame:component/2.

amalgame:component(candidate_generator, carthesian_product(source, source, align(uri, uri, provlist), [])).

%%	candidate(+Source, +Target, -Align, +Options)

candidate(SourceScheme, TargetScheme, align(Source, Target, []), _Options) :-
	graph_member(Source, SourceScheme),
	graph_member(Target, TargetScheme).
