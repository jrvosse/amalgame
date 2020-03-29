:- module(strategy_backward_compatability, []).

:- use_module(library(lists)).
:- use_module(library(settings)).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(skos/util)).
:- use_module(library(amalgame/ag_strategy)).

:- multifile
	amalgame:prebuilder/1.

:- rdf_meta
	is_old_vocab_selecter_triple(r,r,r,r),
	mapping_filter_type_to_partitioner(r,r).

amalgame:prebuilder(Strategy) :-
	backward_compatibility_fixes(Strategy).

mapping_filter_type_to_partitioner(amalgame:'SelectPreLoaded',	 amalgame:'SelectPreLoadedSelecter').
mapping_filter_type_to_partitioner(amalgame:'ExactLabelMatcher', amalgame:'ExactLabelSelecter').
mapping_filter_type_to_partitioner(amalgame:'IsubMatcher',       amalgame:'IsubSelecter').
mapping_filter_type_to_partitioner(amalgame:'CompoundMatcher',   amalgame:'CompoundLabelSelecter').
mapping_filter_type_to_partitioner(amalgame:'SnowballMatcher',   amalgame:'SnowballLabelSelecter').
mapping_filter_type_to_partitioner(amalgame:'AncestorMatcher',   amalgame:'AncestorSelecter').
mapping_filter_type_to_partitioner(amalgame:'DescendentMatcher', amalgame:'DescendentSelecter').
mapping_filter_type_to_partitioner(amalgame:'RelatedMatcher',    amalgame:'RelatedSelecter').

backward_compatibility_fixes(Strategy) :-
	fix_opmv_ns(Strategy),
	fix_vocab_selecters(Strategy),
	fix_old_mapping_filters(Strategy),
	fix_sec_inputs(Strategy),
	fix_arity_params(Strategy),
	fix_publish_ns(Strategy),
	!. % do not do this again

fix_vocab_selecters(Strategy) :-
	findall(rdf(S,P,O,Strategy), is_old_vocab_selecter_triple(S,P,O,Strategy), OldTriples),
	maplist(old_vocab_selecter_to_new, OldTriples),
	rdf_retract_triples(OldTriples).

fix_old_mapping_filters(Strategy) :-
	findall(P-E-T, is_old_mapping_filter(Strategy, P,E,T), Filters),
	maplist(old_filter_to_new(Strategy), Filters).
	% rdf_retract_list(OldTriples).

old_filter_to_new(Strategy, Process-Entity-OldType) :-
	rdf_retractall(Process, rdf:type, OldType, Strategy),
	rdf_retractall(Entity,  amalgame:wasGeneratedBy, Process, Strategy),
	mapping_filter_type_to_partitioner(OldType, NewType),
	rdf_assert(Process, rdf:type, NewType, Strategy),
	ag_strategy:assert_output(Process, amalgame:'MappingPartitioner', Strategy, _, _, Entity).

rdf_retract_triples([]).
rdf_retract_triples([rdf(S,P,O,G)|T]) :-
	rdf_retractall(S,P,O,G),
	rdf_retract_triples(T).

is_old_vocab_selecter_triple(S,amalgame:wasGeneratedBy,O, G) :-
	rdf(S,amalgame:wasGeneratedBy,O, G),
	skos_is_vocabulary(S).

is_old_mapping_filter(Strategy, Filter, Entity, Type) :-
	rdfs_individual_of(Filter, amalgame:'CandidateGenerator'),
	rdf(Filter, rdf:type, Type),
	rdf(Filter, amalgame:input, _, Strategy),
	rdf(Entity, amalgame:wasGeneratedBy, Filter).

old_vocab_selecter_to_new(rdf(S,_,Process,Strategy)) :-
	ag_strategy:assert_output(Process, amalgame:'VocabPartitioner', Strategy, _, _, S).

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
		(   rdf(S,ParamProp, O^^xsd:string, Strategy),
		    rdfs_individual_of(S, amalgame:'AritySelect')
		), ToBeFixed),
	forall(member(rdf(S,P,O), ToBeFixed),
	       (   rdf_retractall(S,P,O^^xsd:string,Strategy),
		   arity_param_convert(O,NewO),
		   rdf_assert(S,P,NewO^^xsd:string, Strategy)
	       )
	      ).

arity_param_convert('type=11', 'type=both'):- !.
arity_param_convert('type=1N', 'type=target'):- !.
arity_param_convert('type=N1', 'type=source'):- !.
arity_param_convert(X,X):- !.
