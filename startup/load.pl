:-module(load, []).

user:file_search_path(amalgame, '../../amalgame/').

http:location(amalgame, cliopatria(amalgame), []).

:- [
    amalgame(namespaces),
    amalgame(applications/align_stats),
    amalgame(applications/voc_stats),
    menu
   ].
