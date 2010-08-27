:- module(ag_map,
	  [
	   map_iterator/1,	   % -Map
	   has_map/3               % ?Map, ?Format ?Graph
	  ]
	 ).

/** <module> Amalgame map module

This module contains predicates to deal with mappings while abstracting
from the underlying formats.

@author Jacco van Ossenbruggen
@license GPL
*/

%%	map_iterator(-Map) is non_det.
%
%	Iterates over all maps to be compared. Map is currently of the
%	form [C1, C2], simply meaning there is a mapping from C1 to C2.
%	What other information is available about this mapping depends
%	on the format it is stored in, see has_map/3 for details.
%
%	This is a stub implementation.
%	@tbd make this configurable over a web interface so that we can
%	restrict the source and target vocabulary.

map_iterator([E1,E2]) :-
	has_map([E1, E2], _, _).


%%	has_map(+Map, -Format, -Graph) is non_det.
%
%	Intended to be used to find graphs that contain Map, and in what
%	Format. Map can be stored in the triple store in several
%	formats. We currently support the following formats:
%
%	* edoal: Alignment map format (EDOAL)
%	* skos:  SKOS Mapping Relation
%       * dc:    dc:replaces
%       * owl:   owl:sameAs
%
%	@see EDOAL: http://alignapi.gforge.inria.fr/edoal.html

has_map([E1, E2], edoal, Graph) :-
	(   ground(E1)
	->  rdf(Cell, align:entity1, E1, Graph),
	    rdf(Cell, align:entity2, E2, Graph)
	;   ground(E2)
	->  rdf(Cell, align:entity2, E2, Graph),
	    rdf(Cell, align:entity1, E1, Graph)
	;   var(Graph)
	->  rdf(Cell, align:entity1, E1, Graph),
	    rdf(Cell, align:entity2, E2, Graph)
	;   rdf(Cell, align:entity1, E1),
	    rdf(Cell, align:entity1, E1, Graph),
	    rdf(Cell, align:entity2, E2, Graph)
	).

has_map([E1, E2], skos, Graph) :-
	rdf_has(E1, skos:mappingRelation, E2, RealProp),
	rdf(E1, RealProp, E2, Graph).

has_map([E1, E2], dc, Graph) :-
	rdf_has(E1, dcterms:replaces, E2, RealProp),
	rdf(E1, RealProp, E2, Graph).

has_map([E1, E2], owl, Graph) :-
	rdf_has(E1, owl:sameAs, E2, RealProp),
	rdf(E1, RealProp, E2, Graph).


%%	prolog:message(map(found, What, From, Number))// is det.
%
%	Used to print various progress messages.
%

prolog:message(map(found, What, From, Number)) -->
        [
          'Found ', Number, ' ', What, ' (', From, ') to process'
        ].
