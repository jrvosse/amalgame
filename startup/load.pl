:-module(load, []).

user:file_search_path(amalgame, '../../amalgame/').

http:location(amalgame, cliopatria(amalgame), []).

:- [
    amalgame(namespaces),
    amalgame(compare/compare),
    amalgame(skos/vocabularies),
    menu
   ].
