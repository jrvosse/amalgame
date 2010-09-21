:- module(amalgame, [ag_load_schemas/0]).

:- use_module(library(semweb/rdf_library)).

user:file_search_path(amalgame_apps, amalgame('applications')).
http:location(amalgame, cliopatria(amalgame), []).

ag_load_schemas :-
	rdf_attach_library(amalgame(ontologies)),
	rdf_load_library(amalgame).


:- [
    amalgame(namespaces),
    amalgame_apps(align_stats),
    amalgame_apps(voc_stats),
    amalgame_apps(concept_finder/concept_finder),
    amalgame_apps(equalizer/equalizer),
    amalgame_apps(evaluator/evaluator),
    menu, skin
   ].
