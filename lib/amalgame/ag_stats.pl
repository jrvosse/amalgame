:- module(ag_stats,[
	      mapping_counts/3,
	      concept_count/3,
	      reference_counts/3,
	      mapping_stats/4,
	      vocab_stats/2
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/expand_graph)).
:- use_module(library(amalgame/caching)).
:- use_module(library(amalgame/vocabulary)).
:- use_module(library(amalgame/ag_evaluation)).
:- use_module(library(amalgame/util)).
:- use_module(library(amalgame/map)).

%%	mapping_counts(+MappingURI,+Strat,?MappingN,?SourceN,?TargetN,?SourcePerc,?TargetPerc)
%	is det.
%
%	Counts for the mappings in MappingURI.
%
%       @param MappingN is the number of total correspondences
%       @param SourceN is the number of source concepts mapped
%       @param TargetN is the number of target concepts mapped

mapping_counts(URL, Strategy, Stats) :-
	with_mutex(URL,	mapping_counts_(URL, Strategy, Stats)).

mapping_counts_(URL, Strategy, Stats) :-
	(   stats_cache(URL-Strategy, _)
	->  true
	;   expand_node(Strategy, URL, _Mapping)
	),
	stats_cache(URL-Strategy, mstats(Stats)).

%%	concept_count(+Vocab, +Strategy, -Count)
%
%	Count is the number of concepts in Vocab when expanded in Strategy

concept_count(Vocab, Strategy, Count) :-
	with_mutex(Vocab, concept_count_(Vocab, Strategy, Count)).

concept_count_(Vocab, Strategy, Count) :-
	(   stats_cache(Vocab-Strategy, _)
	->  true
	;   expand_node(Strategy, Vocab, _Scheme)
	),
	stats_cache(Vocab-Strategy, vstats(Count)).

reference_counts(Id, Strategy, Stats) :-
	with_mutex(Id, reference_counts_(Id, Strategy, Stats)).

reference_counts_(Id, Strategy, Stats) :-
	(   stats_cache(Id-Strategy, refs(Stats))
	->  true
	;   compute_reference_counts(Id, Strategy, Stats)
	).

%%	mapping_stats(+URL, +Mapping, +Strategy, -Stats)
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
	Stats = mstats([
		    total_count(MN),
		    source_count(SN),
		    target_count(TN),
		    source_perc(SPerc),
		    target_perc(TPerc)
		]),
	(   mapping_sources(URL, Strategy, InputS, InputT)
	->  concept_count(InputS, Strategy, SourceN),
	    concept_count(InputT, Strategy, TargetN),
	    rounded_perc(SourceN, SN, SPerc),
	    rounded_perc(TargetN, TN, TPerc)
	;   SPerc = 100, TPerc = 100
	).

vocab_stats(Scheme, Count):-
	findall(C, vocab_member(C, Scheme), Cs),
	length(Cs, Count).



%%	mapping_sources(+MappingURI, Strategy, -Source, -Target)
%
%	Source and Target are the recursive source and target
%	vocabularies of Mapping.

mapping_sources(Manual, Strategy, SV, TV) :-
	rdf_has(Manual, amalgame:evaluationOf, Strategy),
	!,
	has_correspondence_chk(align(SC,TC,_), Manual),
	rdf_has(Strategy, amalgame:includes, SV),
	vocab_member(SC, scheme(SV)),
	rdf_has(Strategy, amalgame:includes, TV),
	vocab_member(TC, scheme(TV)).

mapping_sources(URL, Strategy, S, T) :-
	rdf_has(URL, amalgame:wasGeneratedBy, Process, RealProp),
	rdf(URL, RealProp, Process, Strategy),
	!,
	(   rdf(Process, amalgame:source, S0, Strategy),
	    rdf(Process, amalgame:target, T0, Strategy)
	->  vocab_source(S0, Strategy, S),
	    vocab_source(T0, Strategy, T)
	;   rdf(Process, amalgame:input, Input, Strategy)
	->  mapping_sources(Input, Strategy, S, T)
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
	evaluation_graph_chk(Strategy, Id, Ref),
	expand_node(Strategy, Id, Mappings),
	expand_node(Strategy, Ref, References),
	rdf(Id, amalgame:default_relation, Relation),
	compare_against_ref(Mappings, References, Relation,
			    partition([],[],[],[]), Stats),
	assert(stats_cache(Id-Strategy, refs(Stats))).

part_ref_stats(partition(Matches,Conflicts,Unknown, Missing), Stats) :-
	Stats = [ 'matching with ref. relations'-MLengthS,
		  'conflicting with  ref. relations'-CLengthS,
		  'not yet in reference'-ULengthS,
		  'in ref. but missing here'-MisLengthS
		],
	length(Matches, MLength),
	length(Conflicts, CLength),
	length(Unknown, ULength),
	length(Missing, MisLength),
	TotalFound is MLength + CLength + ULength,
	TotalEval is MLength + CLength + MisLength,
	rounded_perc(TotalFound, MLength, MLengthPerc),
	rounded_perc(TotalFound, CLength, CLengthPerc),
	rounded_perc(TotalFound, ULength, ULengthPerc),
	rounded_perc(TotalEval,  MisLength, MisLengthPerc),
	format(atom(MLengthS), '~d (~w%)', [MLength, MLengthPerc]),
	format(atom(CLengthS), '~d (~w%)', [CLength, CLengthPerc]),
	format(atom(ULengthS), '~d (~w%)', [ULength, ULengthPerc]),
	format(atom(MisLengthS), '~d (~w%)', [MisLength, MisLengthPerc]).

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

