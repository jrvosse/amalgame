:- module(ag_map,
	  [
	   map_iterator/1,	   % -Map
	   has_map/4,              % ?Map, ?Format ?Options, ?Graph
	   has_map/3,		   % ?Map, ?Format ?Graph
	   has_map_chk/3,	   % ?Map, ?Format ?Graph
	   retract_map/3           % +Map, +Format, +Graph
	  ]
	 ).

/** <module> Amalgame map module

This module contains predicates to deal with mappings while abstracting
from the underlying formats.

@author Jacco van Ossenbruggen
@license GPL
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- dynamic
	mapping_props/1.

:-	P = [align:measure,
	     align:relation,
	     rdfs:comment,
	     amalgame:method
	    ],
	rdf_global_term(P, Props),
	retractall(mapping_props(_)),
	assert(mapping_props(Props)).



%%	map_iterator(-Map) is non_det.
%
%	Iterates over all maps in the triple store. Map is currently of the
%	form [C1, C2], simply meaning there is a mapping from C1 to C2.
%	What other information is available about this mapping depends
%	on the format it is stored in, see has_map/3 for details.
%
%	This is a stub implementation.
%	@tbd make this configurable over a web interface so that we can
%	restrict the source and target vocabulary.

map_iterator([E1,E2]) :-
	has_map([E1, E2], _, _).

%%	has_map(+Map, -Format, Options, -Graph) is non_det.
%%%	has_map(+Map, -Format, -Graph) is non_det.
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

has_map([E1, E2], edoal, Options, Graph) :-
	has_map_([E1, E2], Cell, Graph),
	mapping_props(Props),
	findall(Term,
		(   member(Prop, Props),
		    rdf(Cell, Prop, Value, Graph),
		    prop_to_term(Prop, Value, Term)
		),
		Options).

has_map([E1, E2], skos, [relation(RealProp)], Graph) :-
	(   ground(E1), ground(E2)
	->  rdf_has(E1, skos:mappingRelation, E2, RealProp),
	    rdf(E1, RealProp, E2, Graph)
	;   rdfs_subproperty_of(RealProp, skos:mappingRelation),
	    rdf(E1, RealProp, E2, Graph)
	).

has_map([E1, E2], dc, [relation(RealProp)], Graph) :-
	(   ground(E1), ground(E2)
	->  rdf_has(E1, dcterms:replaces, E2, RealProp),
	    rdf(E1, RealProp, E2, Graph)
	;   rdfs_subproperty_of(RealProp, dcterms:replaces),
	    rdf(E1, RealProp, E2, Graph)
	).

has_map([E1, E2], owl, [relation(RealProp)], Graph) :-
	(   ground(E1), ground(E2)
	->  rdf_has(E1, owl:sameAs, E2, RealProp),
	    rdf(E1, RealProp, E2, Graph)
	;   rdfs_subproperty_of(RealProp, owl:sameAs),
	    rdf(E1, RealProp, E2, Graph)
	).

has_map(Map, edoal, Graph) :-
	has_map_(Map, _, Graph).

has_map(Map, Format, Graph) :-
	Format \= edoal,
	has_map(Map, Format, _, Graph).

has_map_chk(Map, Format, Graph) :-
	has_map(Map, Format, Graph),!.

has_map_([E1,E2], Cell, Graph) :-
	(   ground(E1)
	->  rdf(Cell, align:entity1, E1, Graph),
	    rdf(Cell, align:entity2, E2, Graph)
	;   rdf(Cell, align:entity2, E2, Graph),
	    rdf(Cell, align:entity1, E1, Graph)
	).

%%	retract_map(+Map, +Format, Graph) is det.
%
%	retracts Map in Format form Graph if it exists in Graph,
%	succeeds without doing anything if not.

retract_map([E1,E2], edoal, Graph) :-
	(   has_map_([E1, E2], Cell, Graph)
	->  rdf_retractall(Cell, _,_,Graph)
	;   true
	).

prop_to_term(Prop, Value, Term) :-
	rdf_global_id(NS:Local, Prop),
	(   NS:Local = align:measure
	->  (literal(type(_,LRealValue)) = Value;literal(LRealValue) = Value),
	    term_to_atom(RealValue, LRealValue)
	;   NS:Local = amalgame:method
	->  literal(Literal) = Value,
	    term_to_atom(RealValue, Literal)
	;   RealValue = Value
	),
	Term =.. [Local, RealValue],!.

%%	message(+Term)// is det.
%
%	Used to print various progress messages.
%	Term = map(found, What, From, Number)).

prolog:message(map(found, What, From, Number)) -->
        [ 'Found ', Number, ' ', What, ' (', From, ') to process.' ].
prolog:message(map(cleared, What, From, Number)) -->
        [ 'Cleared ', Number, ' ', What, ' (', From, ').' ].






