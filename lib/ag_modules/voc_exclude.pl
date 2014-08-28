:- module(voc_exclude, []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/vocabulary)).

:- public amalgame_module/1.
:- public parameter/4.
:- public exclude/4.

amalgame_module(amalgame:'VocExclude').

parameter(type, oneof([source,target]), source,
	  'Exclude matching sources or targets').

exclude(Vocab, Mapping, scheme(NewScheme), Options) :-
	option(type(Type), Options),
	(   option(new_scheme(NewScheme), Options)
	->  true
	;   rdf_bnode(NewScheme)
	),
	findall(C, vocab_member(C, Vocab), Concepts0),
	mapping_concepts(Type, Mapping, Exclude0),
	sort(Concepts0, Concepts),
	sort(Exclude0, Exclude),
	ord_subtract(Concepts, Exclude, Rest),
	rdf_transaction(forall(member(R,Rest),
			       add_to_scheme(R, NewScheme))),
	rdf_assert(NewScheme, rdf:type, skos:'ConceptScheme', NewScheme).

add_to_scheme(R, Scheme) :-
	rdf_has(R, skos:inScheme, Scheme),
	!.
add_to_scheme(R, Scheme) :-
	rdf_assert(R, skos:inScheme, Scheme, Scheme).

%	pairs_keys(Pairs, Rest),
%	ord_list_to_rbtree(Pairs, RestAssoc).

mapping_concepts(source, Mapping, Concepts) :-
	maplist(arg(1), Mapping, Concepts).
mapping_concepts(target, Mapping, Concepts) :-
	maplist(arg(2), Mapping, Concepts).
