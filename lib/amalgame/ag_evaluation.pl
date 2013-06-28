:- module(ag_evaluation, [
			  evaluation_graph_chk/3, % check if eval graph exist
			  evaluation_graph/3,     % check or create if not
			  is_empty_eval_graph/1,
			  delete_empty_eval_graphs/1
			 ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(amalgame/ag_provenance)).
:- use_module(library(amalgame/util)).

:- setting(amalgame:reference_alignment, oneof([strategy, mapping]), strategy,
	   'Keep reference alignments per mapping or per strategy. Note that in the per mapping mode, the references are lost when the mapping is deleted.').

evaluation_graph_chk(Strategy, Mapping, EvalGraph) :-
	(   setting(amalgame:reference_alignment, mapping)
	->  rdf(EvalGraph, amalgame:evaluationOf, Mapping,  Strategy)
	;   rdf(EvalGraph, amalgame:evaluationOf, Strategy, Strategy)
	),
	!.

evaluation_graph(Strategy, Mapping, EvalGraph) :-
	(   evaluation_graph_chk(Strategy, Mapping, EvalGraph)
	->  true
	;   create_evaluation_graph(Strategy, Mapping, EvalGraph)
	).

create_evaluation_graph(Strategy, Mapping, EvalGraph) :-
	mint_node_uri(Strategy, manual_evaluation_process, EvalProcess),
	mint_node_uri(Strategy, manual_reference_alignment, EvalGraph),

	rdf_assert(EvalProcess, rdf:type,       amalgame:'EvaluationProcess',         Strategy),
	rdf_assert(EvalProcess, rdfs:label,     literal('Manual evaluation process'), Strategy),

	rdf_assert(EvalGraph, rdf:type,     amalgame:'EvaluatedMapping',      Strategy),
	rdf_assert(EvalGraph, rdfs:label,   literal('Evaluation results'),    Strategy),
	rdf_assert(EvalGraph, amalgame:wasGeneratedBy, EvalProcess,           Strategy),

	% defaults to final status so it gets saved by default:
	rdf_assert(EvalGraph, amalgame:status, amalgame:final, Strategy),

	(   setting(amalgame:reference_alignment, mapping)
	->  format(atom(Comment), 'Manual evaluation of ~p', [Mapping]),
	    rdf_assert(EvalProcess, amalgame:input,	Mapping,         Strategy),
	    rdf_assert(EvalGraph, amalgame:evaluationOf,   Mapping,	 Strategy),
	    Options = [was_derived_from([Mapping, Strategy])]
	;   format(atom(Comment), 'Manual evaluation of mappings from ~p', [Strategy]),
	    rdf_assert(EvalProcess, amalgame:input,	Strategy,	 Strategy),
	    rdf_assert(EvalGraph, amalgame:evaluationOf,   Strategy,	 Strategy),
	    Options = [was_derived_from([Strategy])]
	),
	rdf_assert(EvalGraph, rdfs:comment, literal(Comment),	 Strategy),
	provenance_graph(Strategy, ProvGraph),
	prov_was_generated_by(EvalProcess, [EvalGraph], ProvGraph, Options).


delete_empty_eval_graphs(Strategy) :-
	forall((rdf(EvalGraph, amalgame:evaluationOf, Mapping, Strategy),
		is_empty_eval_graph(EvalGraph)
		),
		delete_eval_graph_admin(Strategy, Mapping, EvalGraph)
	      ).

is_empty_eval_graph(Eval) :-
	   rdfs_individual_of(Eval, amalgame:'EvaluatedMapping'),
	   \+ rdf_graph(Eval).


delete_eval_graph_admin(Strategy, Mapping, EvalGraph) :-
	% Beware, this will delete all metadata about your manual evaluations!
	rdf(EvalGraph, amalgame:evaluationOf, Mapping, Strategy),
	rdf(EvalGraph, amalgame:wasGeneratedBy, EvalProcess, Strategy),
	!,
	rdf_retractall(EvalGraph, _, _, Strategy),
	rdf_retractall(EvalProcess, _, _, Strategy).
