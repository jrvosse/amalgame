:- module(conf_amalgame, []).
:- use_module(cliopatria(cliopatria)).	% cp_after_load/1.
:- use_module(library(semweb/rdf_library)).

/** <module> Setup amalgame
*/

:- multifile http:location/3.

http:location(amalgame, cliopatria(amalgame), []).

ag_load_schemas :-
	rdf_attach_library(amalgame(rdf)),
	rdf_attach_library(cliopatria(rdf)),
	rdf_load_library('Amalgame'),
	rdf_load_library('skos').

:- cp_after_load(ag_load_schemas).

:- use_module([ applications(align_stats),
		applications(alignment/alignment),
		applications(vocabularies/vocabularies),
		applications(concept_finder/concept_finder),
		applications(equalizer/equalizer),
		applications(evaluator/evaluator)
	      ]).
