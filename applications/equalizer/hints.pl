:- module(ag_hints, []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).

:- use_module(eq_util).

:- http_handler(amalgame(data/hint), http_json_hint, []).

http_json_hint(Request) :-
	http_parameters(Request,
			[ strategy(Strategy,
				   [uri,
				    description('URI of an alignment strategy')]),
			  focus(Focus,
			       [ uri,
				 description('Node that is currently in focus in the builder'),
				 optional(true)
			       ])
			]),
	find_hint(Strategy, Focus, Hint),
	reply_json(Hint).

find_hint(Strategy, _Focus, Hint) :-
	% If there are no mappings yet, advise an exact label match
	\+ rdf(_, rdf:type, amalgame:'Mapping',Strategy),
	!,
	rdf_equal(Match, amalgame:'ExactLabelMatcher'),
	rdf_display_label(Match, Label),
	format(atom(Text), 'hint: maybe you\'d like to try a simple label Matcher like ~w to create your first mapping', [Label]),
	rdf(Strategy, amalgame:includes, Voc1, Strategy),
	rdf(Strategy, amalgame:includes, Voc2, Strategy),
	Voc1 \== Voc2,
	concept_count(Voc1, Strategy, Count1),
	concept_count(Voc2, Strategy, Count2),
	(   Count1 < Count2
	->  Source = Voc1, Target = Voc2
	;   Source = Voc2, Target = Voc1
	),
	Hint =	json([
		    event(submit),
		    data(json([
			     process(Match),
			     source(Source),
			     target(Target),
			     alignment(Strategy)
			      ])),
		    text(Text)
		     ]).

find_hint(Strategy, Focus, Hint) :-
	% if there are end-point mappings with ambiguous correspondences, advise an ambiguity remover
	needs_disambiguation(Strategy, Focus, Mapping),
	rdf_equal(Process, amalgame:'AritySelect'),
	rdf_display_label(Process, PLabel),
	rdf_display_label(Mapping, MLabel),
	format(atom(Text), 'hint: maybe you\'d like to remove the ambiguity from node "~w" by running an ~w', [MLabel, PLabel]),
	Hint =	json([
		    event(submit),
		    data(json([
			     process(Process),
			     input(Mapping),
			     alignment(Strategy)
			      ])),
		    text(Text)
		     ]).
find_hint(Strategy, Focus, Hint) :-
	% if focus node has been evaluated, maybe it can be get final status?
	rdf(Eval, amalgame:evaluationOf, Focus, Strategy),
	rdf_graph(Eval), % check eval graph ain't empty ...
	rdf(Focus, amalgame:status, Status, Strategy),
	rdf_equal(FinalStatus, amalgame:final),
	FinalStatus \== Status,
	!,
	rdf_global_id(_:Local, Status),
	format(atom(Text), 'hint: this dataset has been evaluated, if the results were satisfactory, you might want to change the status from \'~p\' to \'final\'.', [Local]),
	Hint = json([
		   event(nodeChange),
		   data(json([
			    uri(Focus),
			    alignment(Strategy),
			    status(FinalStatus)
			     ])),
		   text(Text),
		   focus(Focus)
		    ]).

find_hint(Strategy, Focus, Hint) :-
	% if focus node is unambigious and not been evaluated,
	% this might be a good idea to do.
	\+ rdf(Focus, amalgame:evaluationOf, _, Strategy),
	with_mutex(Focus, mapping_counts(Focus, Strategy, N,N,N,_,_)),
	N > 0,
	!,
	format(atom(Text), 'hint: this dataset contains ~w unambigious mappings, that is good!  It has not yet been evaluated, however.  Manual inspection could help you decide if the quality is sufficiently good.', [N]),
	http_link_to_id(http_eq_evaluate, [alignment(Strategy), mapping(Focus)],EvalPage),
	Hint =	json([
		    event(evaluate),
		    data(json([
			     focus(Focus),
			     alignment(Strategy),
			     page(EvalPage)
			      ])),
		    text(Text)
		     ]).
find_hint(Strategy, Focus, Hint) :-
	is_known_to_be_disambiguous(Strategy, Focus, Mapping),
	http_link_to_id(http_eq_evaluate, [alignment(Strategy), mapping(Mapping)],EvalPage),
	format(atom(Text), '~w contains ambiguous mappings.  Maybe you can select the good ones after looking at what is causing the problem.', [Mapping]),
	Hint =	json([
		    event(evaluate),
		    data(json([
			     focus(Mapping),
			     alignment(Strategy),
			     page(EvalPage)
			      ])),
		    text(Text)
		     ]).

find_hint(_, _, json([])).

/*
 todo: if more than one mapping is in need for disambiguation,
 select the one closest to focus node
*/
needs_disambiguation(Strategy, Focus, Mapping) :-
	rdf_equal(amalgame:'AritySelect', AritySelect),
	% Beware: findall below is needed to prevent deadlocks later in mapping_counts!
	findall(Mapping,
		(is_endpoint(Strategy, Mapping), % looking for endpoint mappings
		 \+ is_result_of_process_type(Mapping, AritySelect) % not already resulting from ar.select
		),
		Endpoints0),
	!,

	(   memberchk(Focus, Endpoints0)
	->  Endpoints = [Focus|Endpoints0]
	;   Endpoints = Endpoints0
	),
	member(Mapping, Endpoints),
	\+ mapping_counts(Mapping, Strategy, N, N, N, _, _).   %  differs from the total number of mappings

is_known_to_be_disambiguous(Strategy, _Focus, Mapping) :-
	rdf_has(Mapping, amalgame:discardedBy, Process),
	rdfs_individual_of(Process, amalgame:'AritySelect'),
	is_endpoint(Strategy, Mapping).

%%	is_endpoint(+Strategy, ?Mapping) is nondet.
%
%	Evaluates to true if Mapping is a mapping that is
%	the output of some process in Strategy
%	but not the input of some other
%	process in Strategy.
%
is_endpoint(Strategy, Mapping) :-
	rdf(Mapping, rdf:type, amalgame:'Mapping', Strategy),
	 \+ rdf(_Process, amalgame:input, Mapping, Strategy).
is_endpoint(Strategy, Mapping) :-
	rdf(Mapping, rdf:type, amalgame:'Mapping', Strategy),
	rdf(_Process, amalgame:x, foo).

%%	is_result_of_process_type(?Mapping, ?Type) is nodet.
%
%	Evaluates to true if Mappings was generated by a process of
%	type Type.
%
is_result_of_process_type(Mapping, Type) :-
	rdf_has(Mapping, opmv:wasGeneratedBy, Process),
	rdfs_individual_of(Process, Type).




