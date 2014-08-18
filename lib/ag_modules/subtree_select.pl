:- module(subtree_select, []).

:- use_module(library(option)).
:- use_module(library(http/html_write)).
:- use_module(components(amalgame/util)). % for the amalgame:input_item//3 multifile hook

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

% Add class 'concept' to the input form so we can use the values from
% the concept browser to assist the user in filling in the form:

amalgame:input_item(uri, Value, parent) -->
	html(input([name(parent), class(concept), value(Value)])).
