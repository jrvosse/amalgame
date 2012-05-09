:- module(http_mapping,
	  []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).


:- http_handler(amalgame(api/mapping), http_mapping, []).

http_mapping(Request) :-
	http_parameters(Request,
			[ source(Sources,
				 [uri,
				  zero_or_more,
				  description('URI of a source concept')]),
			  targetScheme(TargetScheme,
				       [uri,
					optional(true),
					description('URI of conceptScheme to which the target should belong')
				       ]),
			  relation(Relation,
				[uri,
				 optional(true),
				 description('URI of the mapping relation')
				])
			]),
	mappings(Sources, TargetScheme, Relation, Mappings),
	reply_json(json(Mappings)).

mappings([], _, _, []).
mappings([Source|Ss], TargetScheme, Relation, [Source=Mappings|Rest]) :-
	findall(Mapping, source_mapping(Source, TargetScheme, Relation, Mapping), Mappings),
	mappings(Ss, TargetScheme, Relation, Rest).
mappings([Source|Ss], TargetScheme, Relation, [Source=[]|Rest]) :-
	mappings(Ss, TargetScheme, Relation, Rest).

source_mapping(Source, TargetScheme, Relation, Mapping) :-
Mapping = json([target=TargetObj,
			targetScheme=TargetSchemeObj,
			relation=RelationObj,
			evidence=json(Properties)
		       ]),
	mapping(Source, Target, Cell),
	rdf(Cell, amalgame:evidence, Evidence),
	target_chk(TargetScheme, Target),
	relation_chk(Relation, Evidence),
	!,
	findall(P=V, evidence_property(Evidence, P, V), Properties),
	json_resource_object(Target, TargetObj),
	json_resource_object(Relation, RelationObj),
	json_resource_object(TargetScheme, TargetSchemeObj).


%%	mapping(+Source, ?Target, ?Cell)
%
%	Cell contains a mapping between Source and Target.

mapping(Source, Target, Cell) :-
	rdf(Cell, align:entity1, Source),
	rdf(Cell, align:entity2, Target).
mapping(Source, Target, Cell) :-
	rdf(Cell, align:entity2, Source),
	rdf(Cell, align:entity1, Target).


%%	relation_chk(?Relation, +Evidence)
%
%	Check for existence of Relation in Evidence in case Relation is
%	instantiated. Otherwise intantiate with Relation or set to @null
%	if not existent.

relation_chk(Relation, Evidence) :-
	nonvar(Relation),
	!,
	rdf(Evidence, align:relation, Relation).
relation_chk(Relation, Evidence) :-
	(   rdf(Evidence, align:relation, R)
	->  Relation = R
	;   Relation = @(null)
	).

target_chk(Scheme, Target) :-
	nonvar(Scheme),
	!,
	rdf(Target, skos:inScheme, Scheme).
target_chk(Scheme, Target) :-
	(   rdf(Target, skos:inScheme, S)
	->  Scheme = S
	;   Scheme = @(null)
	).


%%	evidence_property(+Evidence, -Property, -Value)
%
%	Normalized property value pairs of an Amalgame evidence node.
%
%	We can't yet deal with complex values

evidence_property(E, PropLabel, Value) :-
	rdf(E, Prop, O),
	rdf_display_label(Prop, PropLabel),
	(   O = literal(_)
	->  literal_text(O, Value)
	;   atom(O)
	->  rdf_display_label(O, Value)
	).


%%	resource_object(+Resource, -JSONObj)
%
%	Date contains URI and Label of Resource.

json_resource_object(R, json([uri=R, label=L])) :-
	rdf_display_label(R, L).
