:- module(amalgame, [ag_load_schemas/0]).

		 /*******************************
		 *	   SEARCH PATHS		*
		 *******************************/

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

:- (   user:file_search_path(amalgame, _)
   ->  true
   ;   prolog_load_context(directory, Amalgame),
       assert(user:file_search_path(amalgame, Amalgame))
   ).

user:file_search_path(cliopatria, amalgame('ClioPatria')).

:- use_module(cliopatria(cliopatria)).

user:file_search_path(ontology,   	amalgame(ontologies)).
user:file_search_path(amalgame_apps,	amalgame('applications')).

:- use_module(library(semweb/rdf_library)).
:- use_module(library(version)).


		 /*******************************
		 *	  WEB SERVER SETUP	*
		 *******************************/

:- multifile http:location/3.

http:location(amalgame, cliopatria(amalgame), []).

ag_load_schemas :-
	rdf_attach_library(amalgame(ontologies)),
	rdf_load_library('Amalgame').

:- cp_after_load(ag_load_schemas).

:- check_prolog_version(51106).          % Demand >= 5.11.6
:- register_git_module('amalgame',
		       [ home_url('http://eculture.cs.vu.nl/git/econnect/amalgame.git')
		       ]).

:- use_module([ amalgame(namespaces),
		amalgame_apps(align_stats),
		amalgame_apps(vocabularies/vocabularies),
		amalgame_apps(concept_finder/concept_finder),
		amalgame_apps(equalizer/equalizer),
		amalgame_apps(evaluator/evaluator)
	      ]).
