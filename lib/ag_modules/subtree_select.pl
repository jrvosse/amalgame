:- module(subtree_select, []).

:- public amalgame_module/1.
:- public parameter/4.
:- public selecter/3.

amalgame_module(amalgame:'SubtreeSelect').

parameter(parent, uri, '',
'Concept that is the top concept of the subtree').
parameter(mode, oneof([select, remove]), select, 'select or remove concepts from this subtree').

% a bit naive at the moment: we simply change the query that should be
% done.

selecter(Scheme, and((Scheme), subtree(Parent)), Options) :-
	option(mode(select), Options),
	option(parent(Parent), Options).

selecter(Scheme, and((Scheme), not(subtree(Parent))), Options) :-
	option(mode(remove), Options),
	option(parent(Parent), Options).
