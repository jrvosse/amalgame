:- module(voc_exclude, []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/map)).
:- use_module(library(amalgame/expand_graph)).
:- use_module(library(amalgame/vocabulary)).

:- public amalgame_module/1.
:- public parameter/4.
:- public exclude/4.

amalgame_module(amalgame:'VocExclude').

parameter(type, oneof([source,target]), source,
	  'Property to exclude matching sources or targets').

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
	rdf(R, skos:inScheme, Scheme),
	!.
add_to_scheme(R, Scheme) :-
	rdf_assert(R, skos:inScheme, Scheme, Scheme).

%	pairs_keys(Pairs, Rest),
%	ord_list_to_rbtree(Pairs, RestAssoc).

mapping_concepts(source, Mapping, Concepts) :-
	maplist(arg(1), Mapping, Concepts).
mapping_concepts(target, Mapping, Concepts) :-
	maplist(arg(2), Mapping, Concepts).

/*

concept_selecter(scheme(SourceVoc), Result, Options) :-
	option(exclude_sources(Exc), Options),!,
	maplist(align_source, Exc, ExcSrcs),
	findall(S, vocab_member(S, scheme(SourceVoc)), Sources),

	sort(Sources, SsSorted),
	sort(ExcSrcs, EsSorted),
	ord_subtract(SsSorted, EsSorted, SelSourceVoc),
	(   option(targetvoc(TargetVoc), Options)
	->  maplist(align_source(TargetVoc), Result, SelSourceVoc)
	;   maplist(align_source, Result, SelSourceVoc)
	).

concept_selecter(SourceAlignment, TargetAlignment, Options) :-
	option(exclude_sources(Exc), Options),!,
	voc_exclude(source, SourceAlignment, Exc, [], TargetAlignment0),
	predsort(ag_map:compare_align(targetplus), TargetAlignment0, TargetAlignment).

concept_selecter(SourceAlignment, TargetAlignment, Options) :-
	option(exclude_targets(Exc), Options),!,
	debug(align, 'Running target selector', []),
	predsort(ag_map:compare_align(targetplus), SourceAlignment, SourceAlignmentSorted),
	predsort(ag_map:compare_align(targetplus), Exc, ExcSorted),
	voc_exclude(target, SourceAlignmentSorted, ExcSorted, [], TargetAlignment0),
	predsort(ag_map:compare_align(targetplus), TargetAlignment0, TargetAlignment).

voc_exclude(_Type, L, [], Accum, Result) :- !, append(Accum, L, Result).
voc_exclude(_Type, [], _, Accum, Accum) :- !.
voc_exclude(Type, [A|ATail], [E|ETail], Accum, Result) :-
	compare_align(Type, Order, A, E),
	(   Order == '='
	->  voc_exclude(Type, ATail, [E|ETail], Accum, Result)
	;   Order == '<'
	->  voc_exclude(Type, ATail, [E|ETail], [A|Accum], Result)
	;   Order == '>'
	->  voc_exclude(Type, [A|ATail], ETail, Accum, Result)
	).

align_source(TargetVoc, align(S,scheme(TargetVoc),[]), S).

align_source(align(S1,_,P), S2) :- (nonvar(S2) -> P=[]; true), S1=S2.
% align_target(align(_,T1,P), T2) :- (nonvar(T2) -> P=[]; true), T1=T2.
*/
