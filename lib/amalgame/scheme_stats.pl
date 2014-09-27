:- module(ag_scheme_stats,
	  [
	      scheme_stats/4
	  ]).

:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(skos/util)).
:- use_module(library(amalgame/ag_strategy)).
:- use_module(library(amalgame/ag_provenance)).

:- rdf_meta
	fcplp(+, r, r, r, -, -).

%%	scheme_stats(+Scheme, +Concepts, ?Strategy, -Stats) is det.
%
%	Stats are statistics for Concepts in Scheme.
%

scheme_stats(Scheme, ConceptAssoc, Strategy, Stats) :-
	Stats = scheme_stats_dict{
		    '@id': Scheme,
		    strategy: Strategy,
		    totalCount: TotalCount,
		    formats: Formats,
		    virtual: Virtual,
		    version: Version,
		    revision: Revision,
		    languages: Languages,
		    properties: LanguagesDict,
		    totalLabelCount: TotalLabelCount,
		    uniqueLabelCount: UniqueLabelCount
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
	length(Concepts, TotalCount),
	concepts_stats(Concepts, Skos, XLP, XLA),
	length(Skos, SkosNr),
	length(XLP, XLPNr),
	length(XLA, XLANr),
	label_formats(SkosNr, XLPNr, XLANr, Formats),
	append([Skos, XLP, XLA], AllLabels),
	length(AllLabels, TotalLabelCount),
	sort(AllLabels, UniqueLabels),
	msort(AllLabels, SortedLabels),
	group_pairs_by_key(SortedLabels, Grouped),
	group_lengths(Grouped, GroupLengths),
	length(UniqueLabels, UniqueLabelCount),
	dictifyColonList(GroupLengths, LanguagesDict, Languages).

label_formats(0,0,0, [none]) :- !.
label_formats(0,_,_, [skosxl]):-!.
label_formats(_,0,0, [skos]):-!.
label_formats(_,_,_, [skos, skosxl]):-!.

cl_pairs([], [], []).
cl_pairs([(Property:Lang)-Count|T], [Property-(Lang-Count)|TO], [Lang|L0]) :-
	cl_pairs(T, TO, L0).

group_lengths([], []).
group_lengths([K-List|T], [K-Length|TLength]) :-
	length(List, Length),
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

concepts_stats([], [], [], []).
concepts_stats([H|T], S, XLP, XLA) :-
	concepts_stats(T, ST, XLPT, XLAT),
	concept_stat(H, SH, XLPH, XLAH),
	append(SH, ST, S),
	append(XLPH, XLPT, XLP),
	append(XLAH, XLAT, XLA).

concept_stat(C, Skos, SkosXLa, SkosXLp) :-
	findall((Prop:Lang)-Label,   fcplp(skos,   C, rdfs:label, Prop, Lang, Label), Skos),
	findall((Prop:Lang)-Label, fcplp(skosxl, C, skosxl:prefLabel, Prop, Lang, Label), SkosXLp),
	findall((Prop:Lang)-Label, fcplp(skosxl, C, skosxl:altLabel,  Prop, Lang, Label), SkosXLa).

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
find_voc_revision(_Voc, amalgame).

