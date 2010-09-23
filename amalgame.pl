:- module(amalgame, [ag_load_schemas/0]).

:- use_module(library(semweb/rdf_library)).
:- use_module(library(version)).


user:file_search_path(amalgame_apps, amalgame('applications')).
http:location(amalgame, cliopatria(amalgame), []).

ag_load_schemas :-
	rdf_attach_library(amalgame(ontologies)),
	rdf_load_library(amalgame).

:- check_prolog_version(51106).          % Demand >= 5.11.6
:- register_git_component('amalgame',
                          [ home_url('http://eculture.cs.vu.nl/git/econnect/amalgame.git')
                          ]).

:- [
    amalgame(namespaces),
    amalgame_apps(align_stats),
    amalgame_apps(voc_stats),
    amalgame_apps(concept_finder/concept_finder),
    amalgame_apps(equalizer/equalizer),
    amalgame_apps(evaluator/evaluator),
    menu, skin
   ].
