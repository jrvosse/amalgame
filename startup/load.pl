:-module(load, []).

user:file_search_path(amalgame, '../../amalgame/').
user:file_search_path(amalgame_apps, amalgame('applications/')).

http:location(amalgame, cliopatria(amalgame), []).

:- [
    amalgame(namespaces),
    amalgame_apps(align_stats),
    amalgame_apps(voc_stats),
    amalgame_apps(concept_finder/concept_finder),
    menu
   ].
