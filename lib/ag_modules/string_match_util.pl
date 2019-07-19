:- module(ag_string_match_util,
	  [label_list/1,
	   skos_has/5,
	   matching_types/2
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).

%%	labels_list(-L) is det.
%
%	L is a sorted list of portrayed label options
label_list(LabelProps) :-
	findall(LP,
		(   label_property(LP)
		;   rdfs_subproperty_of(LP, skos:note)
		;   rdfs_subproperty_of(LP, skos:notation)
		;   rdfs_subproperty_of(LP, rdfs:label)
		;   rdf(LP, rdfs:range, skosxl:'Label')
		),
		LabelProps0),
	sort(LabelProps0, LabelProps).

%%	skos_has(Concept, Prop, Lit, RealProp) is nondet.
%
%	Like rdf_has(Concept, Prop, Lit, RealProp), but skosxl-aware.
%
%	Match literal Lit to Concept a la:
%       * plain rdf_has (skos),
%	* via literal object (skosxl)
%	** via amalgame:term if literal objects as amalgame:qualifier
%	** via skosxl:literalForm if no amalgame:qualifier

skos_has(Concept, MatchProp, Literal, RealProp, _Options) :-
	rdf_has(Concept, MatchProp, Literal, RealProp).

skos_has(Concept, MatchProp, Literal, RealProp, Options) :-
	nonvar(Concept),
	rdf_has(Concept, MatchProp, LiteralObject, RealProp),
	rdf_subject(LiteralObject),
	(   ( rdf_has(LiteralObject, amalgame:qualifier, _),
	      option(match_qualified_only(true), Options, false)
            )
	->  rdf_has(LiteralObject, amalgame:term, Literal)
	;   rdf_has(LiteralObject, skosxl:literalForm, Literal)
	).

skos_has(Concept, MatchProp, Literal, RealProp, Options) :-
	var(Concept),
	(   ( option(match_qualified_only(true), Options, false),
	      rdf_has(_, amalgame:term, Literal)
	    )
	->  rdf_has(LiteralObject, amalgame:term, Literal)
	;   rdf_has(LiteralObject, skosxl:literalForm, Literal)
	),
	rdf_has(Concept, MatchProp, LiteralObject, RealProp),
	true.

%%	matching_types(+S, +T) is semidet.
%
%	Fails if S and T have conflicting types.
%       Succeeds if
%	* S or T have no types other than skos:Concept, or,
%	* S and T have equal types other than skos:Concept, or,
%	* S and T have different types, other than skos:Concept,
%         and one is the subclass of the other.

matching_types(S1, S2) :-
	(   (rdf(S1, rdf:type, T1), \+ rdf_equal(T1, skos:'Concept')),
	    (rdf(S2, rdf:type, T2), \+ rdf_equal(T2, skos:'Concept'))
	->  ( T1 == T2
	    ->  true
	    ;   rdfs_subclass_of(T1, T2)
	    ->  true
	    ;   rdfs_subclass_of(T2, T1)
	    ->  true
	    ;   debug(ex_expand, 'Non matching types ~p/~p', [T1,T2]),
		false
	    )
	;
	true)
	,!.
