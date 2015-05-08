:- module(ag_scheme_stats,
	  [
	      scheme_stats/4,
	      scheme_stats_deep/4
	  ]).

:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(skos/util)).
:- use_module(library(stat_lists)).
:- use_module(library(amalgame/ag_strategy)).
:- use_module(library(amalgame/ag_provenance)).

:- rdf_meta
	fcplp(+, r, r, r, -, -).

%%	scheme_stats(+Scheme, +Concepts, ?Strategy, -Stats) is det.
%
%	Stats are statistics for Concepts in Scheme.
%

scheme_stats(Strategy, Scheme, ConceptAssoc, Stats) :-
	Stats = scheme_stats_dict{
		    '@id': Scheme,
		    strategy: Strategy,
		    totalCount: TotalCount,
		    virtual: Virtual,
		    version: Version,
		    revision: Revision
		},
	debug(scheme_stats, 'Computing stats for ~p', [Scheme]),
	(   skos_in_scheme(Scheme, _)
	->  Virtual = false
	;   strategy_process_entity(Strategy, _ ,Scheme)
	->  Virtual = true
	;   Virtual = unknown_scheme
	),
	find_voc_revision(Scheme, Revision),
	find_voc_version(Scheme, Version),
	assoc_to_keys(ConceptAssoc, Concepts),
	length(Concepts, TotalCount).

scheme_stats_deep(Strategy, Scheme, ConceptAssoc, Stats) :-
	atomic_list_concat([scheme_stats_deep_,Scheme], Mutex),
	with_mutex(Mutex, scheme_stats_deep_(Strategy, Scheme, ConceptAssoc, Stats)).

scheme_stats_deep_(_Strategy, Scheme, ConceptAssoc, Stats) :-
	Stats = scheme_stats_dict{
		    '@private': Private,
		    formats: Formats,
		    structure: DStatsPub,
		    languages: Languages,
		    properties: LanguagesDict,
		    totalLabelCount: TotalLabelCount
		},
	assoc_to_keys(ConceptAssoc, Concepts),

	% compute all (prop:lang)-label pairs for skos and skosxl labels:
	concepts_stats(Concepts, Skos, XLP, XLA),
	length(Skos, SkosNr),
	length(XLP, XLPNr),
	length(XLA, XLANr),
	label_formats(SkosNr, XLPNr, XLANr, Formats),
	append([Skos, XLP, XLA], AllPropLangLabelPairs),
	length(AllPropLangLabelPairs, TotalLabelCount),

	% compute total and nr of amb. labels per prop:lang:
	msort(AllPropLangLabelPairs, SortedLabels),
	group_pairs_by_key(SortedLabels, Grouped),
	group_lengths(Grouped, GroupLengths),
	dictifyColonList(GroupLengths, LanguagesDict, Languages),

	compute_depth_stats(Scheme, ConceptAssoc, DStatsPub, Private).

duplicates([], []) :- !.
duplicates([K-V1, K-V2, K-V3 | T], [K-V1 | DT]) :-
	duplicates([K-V2, K-V3|T], DT), !.
duplicates([K-V1, K-V2 | T], [K-V1, K-V2 | DT]) :-
	duplicates(T, DT), !.
duplicates([H, H, H | T], [H | DT]) :-
	duplicates([H, H |T], DT), !.
duplicates([H, H | T], [H, H | DT]) :-
	duplicates(T, DT), !.
duplicates([_H | T], DT) :-
	duplicates(T, DT), !.

label_formats(0,0,0, [none]) :- !.
label_formats(0,_,_, [skosxl]):-!.
label_formats(_,0,0, [skos]):-!.
label_formats(_,_,_, [skos, skosxl]):-!.

cl_pairs([], [], []).
cl_pairs([(Property:Lang)-Count|T], [Property-(Lang-Count)|TO], [Lang|L0]) :-
	cl_pairs(T, TO, L0).

group_lengths([], []).
group_lengths([K-SortedPairs|T], [K-LDict|TLength]) :-
	length(SortedPairs, TotalLabelCount),
	pairs_values(SortedPairs, AllConcepts),
	sort(AllConcepts, UniqConcepts),
	length(UniqConcepts, UniqConceptsCount),

	duplicates(SortedPairs, DuplicateKeyPairs),
	pairs_keys(DuplicateKeyPairs, DuplicateLabels),
	sort(DuplicateLabels, UniqDubLabels),
	length(UniqDubLabels, UniqDubLabelsCount),

	pairs_values(DuplicateKeyPairs, DuplicateConcepts),
	sort(DuplicateConcepts, UniqDubConcepts),
	length(UniqDubConcepts, UniqDubConceptsCount),
	LDict=label{
		  totalConceptCount:     UniqConceptsCount,
		  totalLabelCount:       TotalLabelCount,
		  ambiguousConcepts:	 UniqDubConcepts,
		  ambiguousConceptCount: UniqDubConceptsCount,
		  ambiguousLabels:       UniqDubLabels,
		  ambiguousLabelCount:   UniqDubLabelsCount
	      },
	group_lengths(T, TLength).

dictifyColonList(CL, Dict, Langs):-
	cl_pairs(CL, Pairs, Langs0),
	sort(Langs0, Langs),
	group_pairs_by_key(Pairs, Grouped),
	dictify_grouped(Grouped, DictPairs),
	dict_pairs(Dict, property, DictPairs).

dictify_grouped([], []).
dictify_grouped([Key-Value|Tail], [Key-DValue|DTail]) :-
	dict_pairs(DValue, language, Value),
	dictify_grouped(Tail, DTail).

concepts_stats(L, S, P, A) :-
	concepts_stats_(L, S0, P0, A0),
	append(S0, S),
	append(P0, P),
	append(A0, A).

concepts_stats_([], [], [], []).
concepts_stats_([H|T], [SH|ST], [XLPH|XLPT], [XLAH|XLAT]) :-
	concept_stat(H, SH, XLPH, XLAH),
	concepts_stats_(T, ST, XLPT, XLAT).


concept_stat(C, Skos, SkosXLa, SkosXLp) :-
	findall((Prop:Lang)-(Label-C), fcplp(skos,   C, rdfs:label,       Prop, Lang, Label), Skos),
	findall((Prop:Lang)-(Label-C), fcplp(skosxl, C, skosxl:prefLabel, Prop, Lang, Label), SkosXLp),
	findall((Prop:Lang)-(Label-C), fcplp(skosxl, C, skosxl:altLabel,  Prop, Lang, Label), SkosXLa).

fcplp(Format, Concept, Prop, RealProp, Lang, Label) :-
	skos_match(Concept, Prop,  Literal, RealProp, [format(Format)]),
	(   var(Label)
	->  (   Literal = literal(lang(Lang, Label))
	    ->  true
	    ;   literal_text(Literal, Label), Lang = no_lang
	    )
	;   true
	).


%%	assert_voc_version(+Voc, +TargetGraph) is det.
%
%	Version of Voc

find_voc_version(Voc, Version) :-
	(   rdf(Voc, amalgame:subSchemeOf, SuperVoc)
	->  find_subvoc_version(Voc, SuperVoc, Version)
	;   rdf_has(Voc, owl:versionInfo, VersionL)
	->  literal_text(VersionL, Version)
	;   Version = ''
	).

find_subvoc_version(Voc, SuperVoc, Version) :-
	(   rdf_has(Voc, owl:versionInfo, VersionL)
	->  literal_text(VersionL, Version)
	;   find_voc_version(SuperVoc, Version)
	).

find_voc_revision(Voc, Version) :-
	rdf(_, skos:inScheme, Voc, SourceGraph:_),!,
	prov_get_entity_version(Voc, SourceGraph, Version).
find_voc_revision(Voc, Version) :-
	rdf(Voc, amalgame:graph, SourceGraph), !,
	prov_get_entity_version(Voc, SourceGraph, Version).
find_voc_revision(Voc, Version) :-
	atom(Voc),
	rdf_graph(Voc),
	rdf_graph_property(G, modified(Modified)),
	rdf_graph_property(G, source_last_modified(Time)),
	stamp_date_time(Time, Date, 'UTC'),
        format_time(atom(Atom),
                    '%d %b %y %T',
                    Date),
	dirty(Modified, Dirty),
	atomic_list_concat([Dirty, Atom], Version).
find_voc_revision(_Voc, '?').

dirty(true, 'dirty:').
dirty(_, '').

parent_child_chk(P,C) :-
	skos_parent_child(P,C),!.

% compute_depth_stats(_Assoc, _{}, _{}) :- !.

compute_depth_stats(Voc, Assoc, Public, Private) :-
	Public = structure{ depths: DC5,
			    children: CC5,
			    topConceptCount: TC
			  },
	Private = structure{topConcepts: TopConcepts,
			    depthMap: Depths},
	findall(TopConcept,
		(   gen_assoc(TopConcept, Assoc, _),
		    \+ (parent_child_chk(Child, TopConcept),
			get_assoc(Child, Assoc,_)
		       )
		),
		TopConcepts),
	length(TopConcepts, TC),
	atomic_list_concat([depth_stats_, Voc], Mutex),
	with_mutex(Mutex, compute_depths(Mutex,TopConcepts,Assoc,Depths)),
	assoc_to_values(Depths, Pairs),
	pairs_keys(Pairs,   DepthCounts0), msort(DepthCounts0, DepthCounts),
	pairs_values(Pairs, ChildCounts0), msort(ChildCounts0, ChildCounts),
	list_five_number_summary_dict(DepthCounts, DC5),
	list_five_number_summary_dict(ChildCounts, CC5).


compute_depths(Mutex, TopConcepts, Assoc, Depths) :-
	empty_assoc(Visited),
	nb_setval(Mutex, Visited),
	forall(member(C, TopConcepts),
		   compute_concept_depth(Mutex, C, Assoc, 1)
	      ),
	nb_getval(Mutex, Depths),
	nb_delete(Mutex).

compute_concept_depth(Mutex, Concept, Assoc, Depth) :-
	nb_getval(Mutex, Visited),
	(   get_assoc(Concept, Visited, _Stats)
	->  true
	;   findall(Child,
		    (   skos_parent_child(Concept, Child),
			get_assoc(Child, Assoc, _)
		    ),
		    Children),
	    length(Children, ChildrenCount),
	    put_assoc(Concept, Visited, Depth-ChildrenCount, Visited1),
	    nb_linkval(Mutex, Visited1),
	    Depth1 is Depth + 1,

	    forall(member(Child, Children),
		   compute_concept_depth(Mutex, Child, Assoc, Depth1)
		  )
	).

list_five_number_summary_dict([], []).
list_five_number_summary_dict([_], []).
list_five_number_summary_dict(List, Stats) :-
	list_five_number_summary(List, OptionFormat),
	dict_create(Stats, stats, OptionFormat).
