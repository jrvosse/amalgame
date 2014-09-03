:- module(strategy_backward_compatability, []).

:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(settings)).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(skos/util)).
:- use_module(library(amalgame/rdf_util)).
:- use_module(api(ag_process)). % hack: we use ag_process:assert_output

:- multifile
	amalgame:prebuilder/1.

:- rdf_meta
	is_old_vocab_selecter_triple(r,r,r,r).

amalgame:prebuilder(Strategy) :-
	backward_compatibilty_fixes(Strategy).

backward_compatibilty_fixes(Strategy) :-
	fix_vocab_selecters(Strategy),
	fix_opmv_ns(Strategy),
	fix_sec_inputs(Strategy),
	fix_arity_params(Strategy),
	fix_publish_ns(Strategy).

fix_vocab_selecters(Strategy) :-
	findall(rdf(S,P,O,Strategy), is_old_vocab_selecter_triple(S,P,O,Strategy), OldTriples),
	% maplist(old_vocab_selecter_to_new(OldTriples),
	rdf_retract_list(OldTriples).

rdf_retract_list([]).
rdf_retract_list([rdf(S,P,O,G)|T]) :-
	rdf_retractall(S,P,O,G),
	rdf_retract_list(T).

is_old_vocab_selecter_triple(S,amalgame:wasGeneratedBy,O, G) :-
	rdf(S,amalgame:wasGeneratedBy,O, G),
	skos_is_vocabulary(S).

old_vocab_selecter_to_new(rdf(S,_,Process,Strategy)) :-
	ag_process:assert_output(Process, amalgame:'VocabPartitioner', Strategy, _, _, S).

fix_publish_ns(S) :-
	(   rdf(S, amalgame:publish_ns, _,S)
	->  true
	;   setting(amalgame:default_publish_namespace, NS),
	    rdf_assert(S, amalgame:publish_ns, NS, S)
	).

fix_sec_inputs(Strategy) :-
	findall(rdf(S,RP,O),
		(   rdf_has(S,amalgame:secondary_input, O, RP),
		    rdf(S, RP, O, Strategy)
		), Triples),
	forall(member(rdf(S,P,O), Triples),
	       (   rdf_retractall(S,P,O,Strategy),
		   rdf_assert(S,amalgame:secondary_input, O, Strategy)
	       )
	      ).
fix_opmv_ns(Strategy) :-
	OldProp = 'http://purl.org/net/opmv/ns#wasGeneratedBy',
	findall(rdf(S,OldProp,O),
		rdf(S, OldProp, O, Strategy),
		Triples),
	forall(member(rdf(S,P,O), Triples),
	       (   rdf_retractall(S,P,O,Strategy),
		   rdf_assert(S,amalgame:wasGeneratedBy, O, Strategy)
	       )
	      ).

fix_arity_params(Strategy) :-
	rdf_equal(amalgame:parameters, ParamProp),
	findall(rdf(S,ParamProp,O),
		(   rdf(S,ParamProp, literal(O), Strategy),
		    rdfs_individual_of(S, amalgame:'AritySelect')
		), ToBeFixed),
	forall(member(rdf(S,P,O), ToBeFixed),
	       (   rdf_retractall(S,P,literal(O),Strategy),
		   arity_param_convert(O,NewO),
		   rdf_assert(S,P,literal(NewO), Strategy)
	       )
	      ).

arity_param_convert('type=11', 'type=both'):- !.
arity_param_convert('type=1N', 'type=target'):- !.
arity_param_convert('type=N1', 'type=source'):- !.
arity_param_convert(X,X):- !.
