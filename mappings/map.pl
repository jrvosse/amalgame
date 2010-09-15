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
	mapping_props/1,
	mapping_format/2.

init_mapping_props :-
	P = [align:measure,
	     align:relation,
	     rdfs:comment,
	     amalgame:method
	    ],
	rdf_global_term(P, Props),
	retractall(mapping_props(_)),
	assert(mapping_props(Props)).

init_mapping_format :-
	rdf_equal(skos:mappingRelation, SkosRelation), assert(mapping_format(skos, SkosRelation)),
	rdf_equal(dcterms:replaces, DcRelation),       assert(mapping_format(dc, DcRelation)),
	rdf_equal(owl:sameAs, OwlRelation),            assert(mapping_format(owl, OwlRelation)).

:- init_mapping_props.
:- init_mapping_format.


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

%%	has_map(+Map, ?Format, ?Properties, -Graph) is non_det.
%%%	has_map(+Map, ?Format, -Graph) is non_det.
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

has_map([E1, E2], edoal, Properties, Graph) :-
	has_edoal_map_([E1, E2], Cell, Graph),
	mapping_props(Props),
	findall(Term,
		(   member(Prop, Props),
		    rdf(Cell, Prop, Value, Graph),
		    prop_to_term(Prop, Value, Term)
		),
		Properties).

has_map([E1, E2], Format, [relation(RealProp)], Graph) :-
	mapping_format(Format,MappingProp),
	(   ground(E1), ground(E2)
	->  rdf_has(E1, MappingProp, E2, RealProp),
	    rdf(E1, RealProp, E2, Graph)
	;   rdfs_subproperty_of(RealProp, MappingProp),
	    rdf(E1, RealProp, E2, Graph)
	).

has_map(Map, edoal, Graph) :-
	has_edoal_map_(Map, _Cell, Graph).

has_map([E1, E2], Format, Graph) :-
	(   ground(Format), Format = edoal
	->  fail
	;   true
	),
	mapping_format(Format,MappingProp),
	(   ground(E1), ground(E2)
	->  rdf_has(E1, MappingProp, E2, RealProp),
	    rdf(E1, RealProp, E2, Graph)
	;   rdfs_subproperty_of(RealProp, MappingProp),
	    rdf(E1, RealProp, E2, Graph)
	).


has_map_chk(Map, Format, Graph) :-
	has_map(Map, Format, Graph),!.

has_edoal_map_([E1,E2], Cell, Graph) :-
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
	(   has_edoal_map_([E1, E2], Cell, Graph)
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






