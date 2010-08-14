:-module(load, []).

user:file_search_path(amalgame, '../../amalgame/').

:- [
    amalgame(namespaces),
    amalgame(compare/compare)
   ].
