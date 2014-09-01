:- module(strategy_backward_compatability, []).

:- use_module(library(lists)).
:- use_module(library(settings)).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- multifile
	amalgame:prebuilder/1.

amalgame:prebuilder(Strategy) :-
	backward_compatibilty_fixes(Strategy).

backward_compatibilty_fixes(Strategy) :-
	fix_opmv_ns(Strategy),
	fix_sec_inputs(Strategy),
	fix_arity_params(Strategy),
	fix_publish_ns(Strategy).

fix_publish_ns(S) :-
% backward compatibility
	(   rdf(S, amalgame:publish_ns, _,S)
	->  true
	;   setting(amalgame:default_publish_namespace, NS),
	    rdf_assert(S, amalgame:publish_ns, NS, S)
	).

fix_sec_inputs(Strategy) :-
% backward compatibility
	findall(rdf(S,RP,O),
		(   rdf_has(S,amalgame:secondary_input, O, RP),
		    rdf(S, RP, O, Strategy)
		), Triples),
	forall(member(rdf(S,P,O), Triples),
	       (   rdf_retractall(S,P,O,Strategy),
		   rdf_assert(S,amalgame:secondary_input, O, Strategy)
	       )
	      ).
fix_opmv_ns(Strategy) :- % backward compatibility
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
% backward compatibility
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
