:- module(ag_evaluation, [
			  is_empty_eval_graph/1,
			  evaluation_graph/3
			 ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(amalgame/ag_provenance)).
:- use_module(library(amalgame/opm)).

is_empty_eval_graph(Eval) :-
	   rdfs_individual_of(Eval, amalgame:'EvaluatedMapping'),
	   \+ rdf_graph(Eval).

evaluation_graph(Strategy, Mapping, EvalGraph) :-
	rdf(EvalGraph, amalgame:evaluationOf, Mapping, Strategy),
	!.

evaluation_graph(Strategy, Mapping, EvalGraph) :-
	repeat,
	gensym(evaluation_process, EvalProcess),
	\+ rdf_subject(EvalProcess),

	repeat,
	gensym(evaluation_result, EvalGraph),
	\+ rdf_subject(EvalGraph),
	\+ rdf_graph(EvalGraph),
	!,

	format(atom(Comment), 'Manual evaluation of ~w', [Mapping]),

	rdf_assert(EvalProcess, rdf:type, amalgame:'EvaluationProcess', Strategy),
	rdf_assert(EvalProcess, rdfs:label, literal('Manual evaluation process'), Strategy),
	rdf_assert(EvalProcess, amalgame:input,	Mapping, Strategy),

	rdf_assert(EvalGraph, rdf:type, amalgame:'EvaluatedMapping', Strategy),
	rdf_assert(EvalGraph, rdfs:label, literal('Evaluation results'), Strategy),
	rdf_assert(EvalGraph, rdfs:comment, literal(Comment), Strategy),
	rdf_assert(EvalGraph, opmv:wasGeneratedBy, EvalProcess, Strategy),
	rdf_assert(EvalGraph, amalgame:evaluationOf, Mapping, Strategy),
	rdf_assert(EvalGraph, amalgame:status, amalgame:intermediate, Strategy),

	Options = [was_derived_from([Mapping])],
	provenance_graph(Strategy, ProvGraph),
	opm_was_generated_by(EvalProcess, [EvalGraph], ProvGraph, Options).

delete_eval_graph_admin(Strategy, Mapping, EvalGraph) :-
	% Beware, this will delete all metadata about your manual evaluations!
	rdf(EvalGraph, amalgame:evaluationOf, Mapping, Strategy),
	rdf(EvalGraph, opmv:wasGeneratedBy, EvalProcess, Strategy),
	!,
	rdf_retractall(EvalGraph, _, _, Strategy),
	rdf_retractall(EvalProcess, _, _, Strategy).

