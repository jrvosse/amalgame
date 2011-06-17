:- module(conf_amalgame, []).
:- use_module(library(semweb/rdf_library)).
:- use_module(library(skos_schema)).
:- use_module(library(void_schema)).

/** <module> Setup amalgame
*/

:- multifile http:location/3.

http:location(amalgame, cliopatria(amalgame), []).

:- rdf_attach_library(amalgame(rdf)).
:- rdf_load_library(amalgame).

:- use_module([
	% Deprecated:
		applications(align_stats),
		applications(alignment/alignment),
		applications(vocabularies/vocabularies),
		applications(concept_finder/concept_finder),
		applications(evaluator/evaluator),
	% New UI:
		applications(equalizer/eq)
	      ]).
