:- module(gtaa_cornetto,
	 [align/0
	 ]).

:- use_module(library(amalgame/alignment_graph)).
:- use_module(library(amalgame/candidates/source_candidate)).
:- use_module(library(amalgame/candidates/target_candidate)).
:- use_module(library(amalgame/candidates/alignment_element)).
:- use_module(library(amalgame/candidates/prefix_candidate)).
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
 	source_count(As1, A1N),
	source_count(Ambiguous1, Amb1N),
	source_count(Unambiguous1, UnAmb1N),
	debug(align, 'exact_label_match ~w~nunambiguous ~w~n ambiguous~w~n', [A1N, UnAmb1N, Amb1N]),
	materialize_alignment_graph(Unambiguous1, [graph(gtaa_cornetto_exact_label_unambiguous)]),
/*
	% match the remaining part
	align_exclude:source_select(GTAA_Subjects, GTAA_Rest, [exclude(As1)]),
 	align(GTAA_Rest, Cornetto, source_candidate, stem_label_match, target_candidate, As2, []),
	target_ambiguity:partition(As2, [ambiguous(Ambiguous2),unambiguous(Unambiguous2)], []),
  	source_count(As2, A2N),
	source_count(Ambiguous2, Amb2N),
	source_count(Unambiguous2, UnAmb2N),
	debug(align, 'stem_label_match ~w ~nunambiguous ~w~n ambiguous~w~n', [A2N, UnAmb2N, Amb2N]),
	materialize_alignment_graph(Unambiguous2, [graph(gtaa_cornetto_stem_label_unambiguous)]),

	merge_graphs([Ambiguous1,Ambiguous2], Ambiguous),
	source_count(Ambiguous, AmbN),

	% disambiguate based on provenance (pref label is better than alt label)
	best_label:partition(Ambiguous, [selected(Selected1), discarded(_Discarded1), undecided(Undecided1)], []),
	source_count(Selected1, Sel1N),
	source_count(Undecided1, Und1N),
	debug(align, 'best label on ~w~n unambiguous ~w~n ambiguous~w~n', [AmbN, Sel1N, Und1N]),
	materialize_alignment_graph(Selected1, [graph(gtaa_cornetto_disambiguated_best_label)]),

	most_labels:partition(Undecided1, [selected(Selected2), discarded(_Discarded2), undecided(Undecided2)], []),
	source_count(Selected2, Sel2N),
	source_count(Undecided2, Und2N),
	debug(align, 'most labels on ~w~n unambiguous ~w~n ambiguous~w~n', [Und1N, Sel2N, Und2N]),
	materialize_alignment_graph(Selected2, [graph(gtaa_cornetto_disambiguated_most_labels)]),

	% @TBD add iteration
	align(Undecided2, _, alignment_element, ancestor_match, _, As4, []),
	align(Undecided2, _, alignment_element, descendant_match, _, As5, []),
	source_count(As4, As4N),
	source_count(As5, As5N),
	debug(align, 'hierarchy match on ~w~nancestor ~w~ndescendant~w~n ', [Und2N, As4N, As5N]),
	merge_graphs([As4, As5, Undecided2], As6),
	most_methods:partition(As6, [selected(Selected3), discarded(_Discarded3), undecided(Undecided3)], []),
 	source_count(As6, As6N),
	source_count(Selected3, Sel3N),
	source_count(Undecided3, Und3N),
	debug(align, 'most methods ~w ~n unambiguous ~w~n ambiguous~w~n', [As6N, Sel3N, Und3N]),
	materialize_alignment_graph(Selected3, [graph(gtaa_cornetto_disambiguated_most_methods)]),
	materialize_alignment_graph(Undecided3, [graph(gtaa_cornetto_ambiguous_most_methods)]),
*/
	rdf_equal(P, skos:scopeNote),
	align(Ambiguous1, _, alignment_element, jaccard_match, _, As6, [threshold(-1),sourceprop(P),targetprop(P)]),
	target_ambiguity:partition(As6, [ambiguous(Ambiguous6),unambiguous(Unambiguous6)], []),
  	source_count(As6, A6N),
	source_count(Ambiguous6, Amb6N),
	source_count(Unambiguous6, UnAmb6N),
	debug(align, 'gloss_jaanker_match ~w ~nunambiguous ~w~n ambiguous~w~n', [A6N, UnAmb6N, Amb6N]).

	% match the remainging part from the remaining part :)
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
