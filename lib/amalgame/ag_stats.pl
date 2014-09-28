:- module(ag_stats,[
	      node_stats/4,
	      reference_counts/3,
	      mapping_stats/4
	  ]).

:- use_module(library(apply)).
:- use_module(library(assoc)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(library(skos/util)).
:- use_module(library(stat_lists)).

:- use_module(library(amalgame/ag_strategy)).
:- use_module(library(amalgame/expand_graph)).
:- use_module(library(amalgame/caching)).
:- use_module(library(amalgame/vocabulary)).
:- use_module(library(amalgame/ag_reference)).
:- use_module(library(amalgame/map)).
:- use_module(library(amalgame/util)).
:- use_module(library(amalgame/json_util)).

node_stats(Strategy, Node, Stats, Options) :-
	(   rdfs_individual_of(Node, amalgame:'Mapping')
	->  node_counts(Node, Strategy, Stats, Options)
	;   skos_is_vocabulary(Node)
	->  node_counts(Node, Strategy, Stats, Options)
	;   Stats = []
	).

%%	node_counts(+URI,+Strategy, -Stats, +Options)
%	is det.
%
%	Counts for the items in the set denoted by URI.

node_counts(URL, Strategy, Stats, _Options) :-
	stats_cache(URL-Strategy, Stats),
	!.
node_counts(_URL, _Strategy, _Stats, Options) :-
	option(compute(false), Options, true),
	!,
	fail.

node_counts(URL, Strategy, Stats, Options) :-
	option(compute(true), Options, true),
	!,
	atomic_concat(node_counts, URL, Mutex),
	debug(mutex, 'Locking mutex: ~w', [Mutex]),
	with_mutex(Mutex, node_counts_(URL, Strategy, Stats)),
	debug(mutex, 'Releasing mutex: ~w', [Mutex]).

node_counts_(URL, Strategy, Stats) :-
	expand_node(Strategy, URL, _Result), % this fills the cache
	stats_cache(URL-Strategy, Stats),
	is_dict(Stats).

reference_counts(Id, Strategy, Stats) :-
	atom_concat(reference_counts, Id, Mutex),
	with_mutex(Mutex, reference_counts_(Id, Strategy, Stats)).

reference_counts_(Id, Strategy, Stats) :-
	(   stats_cache(Id-Strategy, refs(Stats))
	->  true
	;   compute_reference_counts(Id, Strategy, Stats)
	).


%%	mapping_stats(+URL, +Mapping, +Strategy, -Stats) is det.
%
%	Stats are statistics for mapping.

mapping_stats(URL, Mapping, Strategy, Stats) :-
	maplist(align_source, Mapping, Ss0),
	maplist(align_target, Mapping, Ts0),
	sort(Ss0, Ss),
	sort(Ts0, Ts),
	length(Mapping, MN),
	length(Ss, SN),
	length(Ts, TN),
	Stats = mapping_stats_dict{
		    totalCount:MN,
		    vocs:vocs{
			     source:SvocDict,
			     target:TvocDict
			 },
		    mappedSourceConcepts:SN,
		    mappedTargetConcepts:TN,
		    sourcePercentage:SPerc,
		    targetPercentage:TPerc,
		    source_depth:DSstats,
		    target_depth:DTstats,
		    source_child_stats:BSstats,
		    target_child_stats:BTstats,
		    sourcePercentageInput:SiPerc,
		    targetPercentageInput:TiPerc,
		    inputPercentage:IP
		},
	(   mapping_vocab_sources(URL, Strategy, InputS, InputT),
	    node_stats(Strategy, InputS, StatsSin, []),
	    node_stats(Strategy, InputT, StatsTin, [])
	->  option(totalCount(SourceN), StatsSin),
	    option(totalCount(TargetN), StatsTin),
	    save_perc(SN, SourceN, SPerc),
	    save_perc(TN, TargetN, TPerc),
	    js_focus_node(Strategy, InputS, SvocDict),
	    js_focus_node(Strategy, InputT, TvocDict)
	;   SourceN  = 0,	TargetN = 0,
	    SvocDict = voc{},   TvocDict=voc{},
	    SPerc    = 0,	TPerc = 0,
	    SiPerc   = 0,	TiPerc = 0
	),
	(   Smap = StatsSin.get('@private').get(depthMap),
	    Tmap = StatsTin.get('@private').get(depthMap)
	->  structure_stats(depth,    Ss, Smap, DSstats),
	    structure_stats(children, Ss, Smap, BSstats),
	    structure_stats(depth,    Ts, Tmap, DTstats),
	    structure_stats(children, Ts, Tmap, BTstats)
	;   DSstats  = [],      DTstats = [],
	    BSstats  = [],      BTstats = []
	),
	findall(Input, has_mapping_input(URL, Strategy, Input), Inputs),
	(   Inputs \= []
	->  maplist(expand_node(Strategy), Inputs, InputMappings),
	    append(InputMappings, Merged),
	    sort(Merged, Unique),
	    maplist(align_source, Unique, Si0),
	    maplist(align_target, Unique, Ti0),
	    sort(Si0, Si),
	    sort(Ti0, Ti),
	    length(Unique, IML),
	    length(Si, SiN),
	    length(Ti, TiN),
	    save_perc(MN, IML, IP),
	    save_perc(SN, SiN, SiPerc),
	    save_perc(TN, TiN, TiPerc)
	;   CarthesianProductSize is SourceN * TargetN,
	    save_perc(MN,CarthesianProductSize, IP),
	    SiPerc = SPerc,
	    TiPerc = TPerc
	).

structure_stats(_,[],_,[]).
structure_stats(_,[_],_,[]).
structure_stats(Type, Concepts, Map, Stats) :-
	maplist(concept_depth(Type,Map), Concepts, Depths),
	sort(Depths, DepthsSorted),
	list_five_number_summary(DepthsSorted, OptionFormat),
	dict_create(Stats, stats, OptionFormat).

concept_depth(depth, Map, Concept, Depth) :-
	get_assoc(Concept, Map, Depth-_Children).
concept_depth(children, Map, Concept, Children) :-
	get_assoc(Concept, Map, _Depth-Children).
concept_depth(Type, _Map, Concept, -1) :-
	debug(stats, 'ERROR: cannot find ~w for ~p in assoc', [Type, Concept]).

has_mapping_input(URL, Strategy, Input) :-
	rdf_has(URL, amalgame:wasGeneratedBy, Process, RP),
	rdf(URL, RP, Process, Strategy),
	rdf_has(Process, amalgame:input, Input),
	rdfs_individual_of(Input, amalgame:'Mapping').


%%	mapping_vocab_sources(+MappingURI, +Strategy, -Source, -Target)
%
%	Source and Target are the recursive source and target
%	vocabularies of Mapping.

mapping_vocab_sources(Manual, Strategy, SV, TV) :-
	(   rdf_has(Manual, amalgame:evaluationOf, Strategy)
	;   rdfs_individual_of(Manual, amalgame:'LoadedMapping')
	),
	!,
	has_correspondence_chk(align(SC,TC,_), Manual),
	strategy_vocabulary(Strategy, SV),
	vocab_member(SC, scheme(SV)),
	strategy_vocabulary(Strategy, TV),
	vocab_member(TC, scheme(TV)).

mapping_vocab_sources(URL, Strategy, S, T) :-
	rdf_has(URL, amalgame:wasGeneratedBy, Process, RealProp),
	rdf(URL, RealProp, Process, Strategy),
	!,
	(   rdf(Process, amalgame:source, S0, Strategy),
	    rdf(Process, amalgame:target, T0, Strategy)
	->  vocab_source(S0, Strategy, S),
	    vocab_source(T0, Strategy, T)
	;   rdf(Process, amalgame:input, Input, Strategy)
	->  mapping_vocab_sources(Input, Strategy, S, T)
	).



vocab_source(V, Strategy, S) :-
	rdf_has(V, amalgame:wasGeneratedBy, Process, Strategy),
	rdf_has(Process, amalgame:input, Input, Strategy),
	!,
	vocab_source(Input, Strategy, S).
vocab_source(V, _S, V).

align_source(align(S,_,_), S).
align_target(align(_,T,_), T).

compute_reference_counts(Id, Strategy, Stats) :-
	reference_mappings(Strategy, References),
	expand_node(Strategy, Id, Mappings),
	rdf(Id, amalgame:default_relation, Relation),
	compare_against_ref(Mappings, References, Relation,
			    partition([],[],[],[]), Stats),
	assert(stats_cache(Id-Strategy, Stats)).

part_ref_stats(partition(Matches,Conflicts,Unknown, Missing), Stats) :-
	Stats = refs_stats_dict{matching:MLengthS,
				conflicting:CLengthS,
				notInRef:ULengthS,
				missing:MisLengthS
			       },
	length(Matches, MLength),
	length(Conflicts, CLength),
	length(Unknown, ULength),
	length(Missing, MisLength),
	TotalFound is MLength + CLength + ULength,
	TotalEval is MLength + CLength + MisLength,
	save_perc(MLength, TotalFound, MLengthPerc),
	save_perc(CLength, TotalFound, CLengthPerc),
	save_perc(ULength, TotalFound, ULengthPerc),
	save_perc(MisLength, TotalEval, MisLengthPerc),
	format(atom(MLengthS),	 '~d (~2f%)', [MLength, MLengthPerc]),
	format(atom(CLengthS),   '~d (~2f%)', [CLength, CLengthPerc]),
	format(atom(ULengthS),   '~d (~2f%)', [ULength, ULengthPerc]),
	format(atom(MisLengthS), '~d (~2f%)', [MisLength, MisLengthPerc]).

compare_against_ref([], Missing,  _, partition(Ma, Co, Un, Mi), Stats) :-
	append(Mi, Missing, Mi2),
	part_ref_stats(partition(Ma, Co, Un, Mi2), Stats).

compare_against_ref(Unknown, [], _, partition(Ma, Co, Un, Mi), Stats) :-
	append(Un, Unknown, Un2),
	part_ref_stats(partition(Ma, Co, Un2, Mi), Stats).

compare_against_ref([align(S,T,P)|MT],[align(SR,TR,PR)|RT], Rel,
		    partition(Matches,Conflicts,Unknown,Missing), Stats):-
	compare(SOrder, S, SR),
	compare(TOrder, T, TR),
	(   SOrder == <
	->  compare_against_ref(MT, [align(SR,TR,PR)|RT], Rel,
				partition(Matches,Conflicts,[align(S,T,P)|Unknown],Missing), Stats)
	;   SOrder == >
	->  compare_against_ref([align(S,T,P)|MT], RT, Rel,
				partition(Matches,Conflicts, Unknown, [align(SR,TR,PR)|Missing]), Stats)
	;   TOrder == <
	->  compare_against_ref(MT, [align(SR,TR,PR)|RT], Rel,
				partition(Matches,Conflicts,[align(S,T,P)|Unknown],Missing), Stats)
	;   TOrder == >
	->  compare_against_ref([align(S,T,P)|MT], RT, Rel,
				partition(Matches,Conflicts, Unknown, [align(SR,TR,PR)|Missing]), Stats)
	;   member(Manual, PR),
	    (	member(method(manual_evaluation), Manual)
	    ;	member(method(preloaded), Manual)
	    ),
	    option(relation(Rel), Manual)
	->  compare_against_ref(MT, RT, Rel,
				partition([align(S,T,P)|Matches], Conflicts, Unknown, Missing), Stats)
	;   compare_against_ref(MT, RT, Rel,
				partition(Matches, [align(S,T,P)|Conflicts], Unknown, Missing), Stats)
	).

