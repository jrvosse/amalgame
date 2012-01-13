
:- module(ag_stats,[
		    mapping_counts/7,
		    concept_count/3,
		    mapping_stats/4,
		    vocab_stats/2
		   ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/expand_graph)).
:- use_module(library(amalgame/caching)).
:- use_module(library(amalgame/vocabulary)).

%%	mapping_counts(+MappingURI,+Strat,?MappingN,?SourceN,?TargetN,?SourcePerc,?TargetPerc)
%	is det.
%
%	Counts for the mappings in MappingURI.
%
%       @param MappingN is the number of total correspondences
%       @param SourceN is the number of source concepts mapped
%       @param TargetN is the number of target concepts mapped

mapping_counts(URL, Strategy, MN, SN, TN, SPerc, TPerc) :-
	with_mutex(URL, mapping_counts_(URL, Strategy, MN, SN, TN, SPerc, TPerc)).

mapping_counts_(URL, Strategy, MN, SN, TN, SPerc, TPerc) :-
	(   stats_cache(URL-Strategy, _)
	->  true
	;   expand_mapping(Strategy, URL, _Mapping)
	),
	stats_cache(URL-Strategy, stats(MN, SN, TN, SPerc, TPerc)).


%%	concept_count(+Vocab, +Strategy, -Count)
%
%	Count is the number of concepts in Vocab when expanded in Strategy

concept_count(Vocab, Strategy, Count) :-
	with_mutex(Vocab, concept_count_(Vocab, Strategy, Count)).

concept_count_(Vocab, Strategy, Count) :-
	(   stats_cache(Vocab-Strategy, _)
	->  true
	;   expand_vocab(Strategy, Vocab, _Scheme)
	),
	stats_cache(Vocab-Strategy, stats(Count)).


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
	Stats = stats(MN, SN, TN, SPerc, TPerc),
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

rounded_perc(0, _, 0.0) :- !.
rounded_perc(_, 0, 0.0) :- !.
rounded_perc(Total, V, Perc) :-
	Perc0 is V/Total,
	dyn_perc_round(Perc0, Perc, 100).

dyn_perc_round(P0, P, N) :-
	P1 is round(P0*N),
	(   P1 == 0
	->  N1 is N*10,
	    dyn_perc_round(P0, P, N1)
	;   P is P1/(N/100)
	).

%%	mapping_sources(+MappingURI, Strategy, -Source, -Target)
%
%	Source and Target are the recursive source and target
%	vocabularies of Mapping.

mapping_sources(URL, Strategy, S, T) :-
	rdf_has(URL, opmv:wasGeneratedBy, Process, RealProp),
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
	rdf_has(V, opmv:wasGeneratedBy, Process, Strategy),
	rdf_has(Process, amalgame:input, Input, Strategy),
	!,
	vocab_source(Input, Strategy, S).
vocab_source(V, _S, V).

align_source(align(S,_,_), S).
align_target(align(_,T,_), T).

