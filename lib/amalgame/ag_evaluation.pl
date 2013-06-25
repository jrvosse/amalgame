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


evaluation_graph_chk(Strategy, Mapping, EvalGraph) :-
	rdf(EvalGraph, amalgame:evaluationOf, Mapping, Strategy),
	!.

evaluation_graph(Strategy, Mapping, EvalGraph) :-
	(   evaluation_graph_chk(Strategy, Mapping, EvalGraph)
	->  true
	;   create_evaluation_graph(Strategy, Mapping, EvalGraph)
	).

create_evaluation_graph(Strategy, Mapping, EvalGraph) :-
	mint_node_uri(Strategy, evaluation_process, EvalProcess),
	mint_node_uri(Strategy, evaluation_result, EvalGraph),

	format(atom(Comment), 'Manual evaluation of ~w', [Mapping]),

	rdf_assert(EvalProcess, rdf:type,       amalgame:'EvaluationProcess',         Strategy),
	rdf_assert(EvalProcess, rdfs:label,     literal('Manual evaluation process'), Strategy),
	rdf_assert(EvalProcess, amalgame:input,	Mapping,                              Strategy),

	rdf_assert(EvalGraph, rdf:type,     amalgame:'EvaluatedMapping',   Strategy),
	rdf_assert(EvalGraph, rdfs:label,   literal('Evaluation results'), Strategy),
	rdf_assert(EvalGraph, rdfs:comment, literal(Comment),              Strategy),

	rdf_assert(EvalGraph, amalgame:wasGeneratedBy, EvalProcess,           Strategy),
	rdf_assert(EvalGraph, amalgame:evaluationOf,   Mapping,               Strategy),
	rdf_assert(EvalGraph, amalgame:status,	       amalgame:intermediate, Strategy),

	Options = [was_derived_from([Mapping])],
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
