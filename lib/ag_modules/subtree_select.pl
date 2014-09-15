:- module(subtree_select, []).

:- use_module(library(option)).
:- use_module(library(amalgame/vocabulary)).

:- use_module(components(amalgame/util)). % for the amalgame:input_item//3 multifile hook
:- use_module(library(http/html_write)).

:- public amalgame_module/1.
:- public parameter/4.
:- public specifier/5.
:- public selecter/5.

amalgame_module(amalgame:'SubtreeSelect').

parameter(parent, uri, '',
	  'Concept that is the top concept of the subtree').
parameter(mode, oneof([select, remove]), select,
	  'select or remove concepts from this subtree').

specifier(VocSpec, Sel, Dis, none, Options) :-
	option(parent(Parent), Options),
	S =  and((VocSpec),	subtree(Parent)),
	D =  and((VocSpec), not(subtree(Parent))),
	(   option(mode(select), Options, select)
	->  Sel = S, Dis = D
	;   Sel = D, Dis = S
	).
selecter(VocSpec, SelConcepts, DisConcepts, [], Options) :-
	specifier(VocSpec, Sel, Dis, _, Options),
	all_vocab_members(Sel, SelConcepts),
	all_vocab_members(Dis, DisConcepts).

% Add class 'concept' to the input form in components(amalgame/util) so
% we can use the values from the concept browser to assist the user in
% filling in the parent parameter in the html form:

amalgame:input_item(uri, Value, parent) -->
	html(input([name(parent), class(concept), value(Value)])).
