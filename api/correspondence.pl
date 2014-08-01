:- module(ag_api_correspondence,
	  [
	  ]).

:- use_module(library(lists)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(semweb/rdf_db)).

:- use_module(library(amalgame/map)).
:- use_module(library(amalgame/expand_graph)).
:- use_module(components(amalgame/correspondence)).

% http handlers

:- http_handler(amalgame(api/correspondence), http_correspondence, []).

%%	http_correspondence(+Request)
%
%	Returns HTML with the dynamic part of the correspondence detail
%	overlay widget.
%
%	This is used in both the builder and the evaluater.

http_correspondence(Request) :-
	http_parameters(Request,
			[ source(Source,
				 [description('URI of the source concept')]),
			  target(Target,
				 [description('URI of the target concept')]),
			  mapping(Mapping,
				  [description('URI of the mapping')]),
			  strategy(Strategy, [description('URL of strategy')]),
			  fillmode(Mode, [
                                   oneof(empty, 'fill-in'),
                                   default(empty),
                                   description('Fill-in the form or leave it empty')]),
			  allsource(AllSource,
				 [boolean, default(false),
				  description('Include all sources')]),
			  alltarget(AllTarget,
				 [boolean, default(false),
				  description('Include all target')])
			]),
	find_correspondences(Mapping, Strategy, Source, Target, AllSource, AllTarget, Cs),
	html_current_option(content_type(Type)),
	findall(R-L, mapping_relation(L, R), Relations),
	phrase(html_correspondences(Cs, [relations(Relations), mode(Mode)]), HTML),
	format('Content-type: ~w~n~n', [Type]),
	print_html(HTML).

find_correspondences(Mapping, Strategy, Source, Target, true, true, Cs):-
% Case we need all correspondences involving Source or Target
	As = align(Source, _, _),
	At = align(_, Target, _),
	(   rdf(_, amalgame:evidenceGraph, _, Mapping)
	->  findall(As, has_correspondence(As, Mapping), Ss),
	    findall(At, has_correspondence(At, Mapping), Ts)
	;   expand_node(Strategy, Mapping, Ms),
	    findall(As, member(As, Ms), Ss),
	    findall(At, member(At, Ms), Ts)
	),
	append(Ss, Ts, Cs0),
	sort(Cs0, Cs).

find_correspondences(Mapping, Strategy, Source, Target, AllSource, AllTarget, Cs):-
% Other 3 cases:
	(   AllSource, \+ AllTarget
	->  A = align(Source,_,_)
	;   AllTarget, \+ AllSource
	->  A = align(_,Target,_)
	;   \+ AllSource, \+ AllTarget
	->  A = align(Source,Target,_)
	),
	!
	,
	(   rdf(_, amalgame:evidenceGraph, _, Mapping)
	->  findall(A, has_correspondence(A, Mapping), Cs)
	;   expand_node(Strategy, Mapping, Ms),
	    findall(A, member(A, Ms), Cs)
	).

