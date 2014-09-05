:- module(ag_strategy,
	  [ process_entity/3
	  ]).

:- use_module(library(semweb/rdf_db)).

:- rdf_meta
	process_entity(r,r,r).

process_entity(Strategy, Process,Entity) :-
	rdf_has(Entity, amalgame:wasGeneratedBy, Process, RealProperty),
	rdf(Entity, RealProperty, Process, Strategy).
