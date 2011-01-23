:- module(gtaa_cornetto,
	 [align/0
	 ]).

:- use_module(library(amalgame/alignment_graph)).
:- use_module(library(amalgame/candidates/source_candidate)).
:- use_module(library(amalgame/candidates/target_candidate)).
:- use_module(library(amalgame/candidates/alignment_element)).
:- use_module(library(amalgame/candidates/prefix_candidate)).
:- use_module(library(amalgame/candidates/carthesian_product)).
:- use_module(library(amalgame/matchers/exact_label_match)).
:- use_module(library(amalgame/matchers/stem_label_match)).
:- use_module(library(amalgame/matchers/jaccard_match)).
:- use_module(library(amalgame/matchers/edit_distance_match)).
:- use_module(library(amalgame/matchers/ancestor_match)).
:- use_module(library(amalgame/matchers/descendant_match)).
:- use_module(library(amalgame/partition/target_ambiguity)).
:- use_module(library(amalgame/partition/best_label)).
:- use_module(library(amalgame/partition/most_labels)).
:- use_module(library(amalgame/partition/most_methods)).
:- use_module(library(amalgame/source/align_exclude)).

align :-
	GTAA_Subjects = scheme('http://data.beeldengeluid.nl/gtaa/Onderwerpen'),
        Cornetto = scheme('http://purl.org/vocabularies/cornetto'),

	% align by exact label match
	align(GTAA_Subjects, Cornetto, source_candidate, exact_label_match, target_candidate, As1, []),
	target_ambiguity:partition(As1, [ambiguous(Ambiguous1),unambiguous(Unambiguous1)], []),
	debug_align_amb(exact_label, As1, Unambiguous1, Ambiguous1),
	materialize_alignment_graph(Unambiguous1, [graph(gtaa_cornetto_exact_label_unambiguous)]),

	% match the remaining part
	align_exclude:source_select(GTAA_Subjects, GTAA_Rest, [exclude(As1)]),
 	align(GTAA_Rest, Cornetto, source_candidate, stem_label_match, target_candidate, As2, []),
	target_ambiguity:partition(As2, [ambiguous(Ambiguous2),unambiguous(Unambiguous2)], []),
	debug_align_amb(stem_label, As2, Unambiguous2, Ambiguous2),
 	materialize_alignment_graph(Unambiguous2, [graph(gtaa_cornetto_stem_label_unambiguous)]),

	merge_graphs([Ambiguous1,Ambiguous2], Ambiguous),
	debug_align(merge, Ambiguous),

	% disambiguate based on provenance (pref label is better than alt label)
	best_label:partition(Ambiguous, Partition1, []),
	memberchk(selected(Selected1), Partition1),
	memberchk(undecided(Undecided1), Partition1),
 	debug_partition(best_label, Partition1),
	materialize_alignment_graph(Selected1, [graph(gtaa_cornetto_disambiguated_best_label)]),

	most_labels:partition(Undecided1, Partition2, []),
	memberchk(selected(Selected2), Partition2),
	memberchk(undecided(Undecided2), Partition2),
 	debug_partition(most_labels, Partition2),
	materialize_alignment_graph(Selected2, [graph(gtaa_cornetto_disambiguated_most_labels)]),

	rdf_equal(P, skos:scopeNote),
	align(Undecided2, _,  alignment_element, jaccard_match, _, As3, [threshold(0.05),sourceprop(P),targetprop(P)]),
	target_ambiguity:partition(As3, [ambiguous(Ambiguous3),unambiguous(Unambiguous3)], []),
  	debug_align_amb(jaccard_gloss, As3, Unambiguous3, Ambiguous3),
	materialize_alignment_graph(Selected2, [graph(gtaa_cornetto_disambiguated_most_labels)]),

	% @TBD add iteration
	align(Undecided2, _, alignment_element, ancestor_match, _, As4, []),
	align(Undecided2, _, alignment_element, descendant_match, _, As5, []),
	debug_align(ancestor, As4),
	debug_align(descendant, As5),
	merge_graphs([As4, As5, Undecided2], As6),
	most_methods:partition(As6, Partition3, []),
	memberchk(selected(Selected3), Partition3),
	memberchk(undecided(Undecided3), Partition3),
 	debug_partition(most_methods, Partition3),
 	materialize_alignment_graph(Selected3, [graph(gtaa_cornetto_disambiguated_most_methods)]),
	materialize_alignment_graph(Undecided3, [graph(gtaa_cornetto_ambiguous_most_methods)]).


	% match the remaining part from the remaining part :)
	/*
	align_exclude:source_select(GTAA_Rest, GTAA_Rest2, [exclude(As2)]),
 	align(GTAA_Rest2, Cornetto, prefix_candidate, edit_distance_match, _, As3, [threshold(10)]),
	target_ambiguity:partition(As3, [ambiguous(Ambiguous3),unambiguous(Unambiguous3)], []),
	source_count(As3, A3N),
 	source_count(Ambiguous3, Amb3N),
	source_count(Unambiguous3, UnAmb3N),
	debug(align, 'edit_distance_match ~w~nunambiguous ~w~n ambiguous~w~n', [A3N, UnAmb3N, Amb3N]),
	materialize_alignment_graph(Unambiguous3, [graph(gtaa_cornetto_edit_distance_unambiguous)]),
	*/

debug_align(Method, As) :-
       	length(As, AsN),
	source_count(As, SN),
	debug(align, '~w ~w ~w~n', [Method, AsN, SN]).

debug_align_amb(Method, As, Unamb, Amb) :-
	length(As, AsN),
	length(Amb, AmbN),
	length(Unamb, UnambN), % just checking
 	source_count(As, SN),
	source_count(Amb, AmbSN),
	source_count(Unamb, UnambSN),
	debug(align, '~w ~w ~w~n  unambiguous ~w ~w~n  ambiguous ~w ~w~n', [Method, AsN, SN, UnambN, UnambSN, AmbN, AmbSN]).

debug_partition(Method, Partitions) :-
	debug(align, '~w', [Method]),
	debug_partitions(Partitions).

debug_partitions([]) :-
	debug(align, '~n', []).
debug_partitions([P|Ps]) :-
	P =.. [Name,As],
	length(As, AsN),
	source_count(As, SN),
	debug(align, '~w ~w ~w', [Name, AsN, SN]),
	debug_partitions(Ps).


source_count(As, N) :-
	maplist(align_source, As, Ss0),
	sort(Ss0, Ss),
	length(Ss, N).
align_source(align(S,_,_), S).


%%	align(+SourceGraph, +TargetGraph, ?CandidateMethod,
%%	+MatchMethod, ?TestMethod, -Output, +Options)
%
%	Output is a list of alignments.

align(Source, Target, Candidate, Match, Test, Output, Options) :-
        (   nonvar(Candidate)
	->  G0 = ( Candidate:candidate(Source, Target, A0, Options),
	           Match:match(A0, A, Options)
		 )
	;   G0 = Match:match(align(_,_,[]), A, Options)
	),
	(   nonvar(Test)
	->  Goal = ( G0,
	             Test:candidate(Source, Target, A, Options)
		   )
	;   Goal = G0
	),
	findall(A, Goal, As0),
 	sort(As0, As),
	merge_provenance(As, Output).
