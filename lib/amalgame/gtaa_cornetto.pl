:- module(gtaa_cornetto,
	 [align/0,
	  noun_label_comparison/0,
	  verb_label_comparison/0
	 ]).

:- use_module(library(amalgame/alignment_graph)).
:- use_module(library(amalgame/candidates/source_candidate)).
:- use_module(library(amalgame/candidates/target_candidate)).
:- use_module(library(amalgame/candidates/alignment_element)).
:- use_module(library(amalgame/candidates/prefix_candidate)).
:- use_module(library(amalgame/candidates/carthesian_product)).
:- use_module(library(amalgame/matchers/exact_label_match)).
:- use_module(library(amalgame/matchers/stem_label_match)).
:- use_module(library(amalgame/matchers/snowball_match)).
:- use_module(library(amalgame/matchers/jaccard_match)).
:- use_module(library(amalgame/matchers/edit_distance_match)).
:- use_module(library(amalgame/matchers/ancestor_match)).
:- use_module(library(amalgame/matchers/descendant_match)).
:- use_module(library(amalgame/partition/target_ambiguity)).
:- use_module(library(amalgame/partition/target_generic)).
:- use_module(library(amalgame/partition/target_sibling)).
:- use_module(library(amalgame/partition/best_label)).
:- use_module(library(amalgame/partition/most_labels)).
:- use_module(library(amalgame/partition/best_numeric)).
:- use_module(library(amalgame/partition/most_methods)).
:- use_module(library(amalgame/source/align_exclude)).
:- use_module(library(amalgame/source/mapped_exclude)).

:- use_module(library(semweb/rdf_label)).
:- use_module(library(semweb/rdfs)).











align :-
	debug(align),
	GTAA_Subjects = scheme('http://data.beeldengeluid.nl/gtaa/Onderwerpen'),
        Cornetto = scheme('http://purl.org/vocabularies/cornetto'),
	rdf_equal(cornetto:'NounSynset',CornettoNounSynset),
	CornettoNoun = type(CornettoNounSynset),
	rdf_equal(cornetto:'VerbSynset',CornettoVerbSynset),
	CornettoVerb = type(CornettoVerbSynset),

	%rdf_equal(skos:prefLabel,PL),
	% Align by stem	match
	align(GTAA_Subjects, CornettoNoun, prefix_candidate, snowball_match, _, As1, []),
	target_ambiguity:partition(As1, [ambiguous(Ambiguous1),unambiguous(Unambiguous1)], []),
	debug_align_amb(noun_snowball_match, As1, Unambiguous1, Ambiguous1, 3932),
	materialize_alignment_graph(Unambiguous1, [graph(gtaa_cornetto_noun_snowball_match)]),

	% select most generic target
	target_generic:partition(Ambiguous1, Partition1, []),
	memberchk(selected(Selected1), Partition1),
	memberchk(undecided(Undecided1), Partition1),
 	debug_partition(target_most_generic, Partition1),
 	materialize_alignment_graph(Selected1, [graph(gtaa_cornetto_most_generic)]),

	% select all target siblings
	target_sibling:partition(Undecided1,  Partition2, []),
	memberchk(selected(Selected2), Partition2),
	memberchk(undecided(Undecided2), Partition2),
 	debug_partition(target_sibling, Partition2),
 	materialize_alignment_graph(Selected2, [graph(gtaa_cornetto_sibling)]),

	hierarchy_loop(Undecided2, 0, Undecided3),

	rdf_equal(P, skos:scopeNote),
	align(Undecided3, _,  alignment_element, jaccard_match, _, Gloss, [threshold(-1),sourceprop(P),targetprop(P)]),
	debug_align(gloss, Gloss, 3932),
 	best_numeric:partition(Gloss, Partition4, []),
	memberchk(selected(Selected4), Partition4),
	memberchk(undecided(Undecided4), Partition4),
 	debug_partition(best_gloss, Partition4),
	materialize_alignment_graph(Selected4, [graph(gtaa_cornetto_disambiguated_gloss)]),

	% disambiguate based on provenance (pref label is better than alt label)
	best_label:partition(Undecided4, Partition5, []),
	memberchk(selected(Selected5), Partition5),
	%memberchk(undecided(Undecided3), Partition3),
 	debug_partition(best_label, Partition5),
	materialize_alignment_graph(Selected5, [graph(gtaa_cornetto_disambiguated_best_label)]),

 	% Match the verbs

 	align_exclude:source_select(GTAA_Subjects, GTAA_Rest, [exclude(As1)]),
	align(GTAA_Rest, CornettoVerb, source_candidate, exact_label_match, target_candidate, As2, []),
	target_ambiguity:partition(As2, [ambiguous(Ambiguous2),unambiguous(Unambiguous2)], []),
	debug_align_amb(verb_exact_label, As2, Unambiguous2, Ambiguous2, 3932),
	materialize_alignment_graph(Unambiguous2, [graph(gtaa_cornetto_verb_label_match)]),
	materialize_alignment_graph(Ambiguous2, [graph(gtaa_cornetto_verb_label_match_ambiguous)]),

	% Match the remaining part
	merge_graphs([As1,As2], As12),
	debug_align(mapped, As12, 3932),
	align_exclude:source_select(GTAA_Subjects, GTAA_Rest2, [exclude(As12)]),
	length(GTAA_Rest2, NotMapped),
	debug(align, 'not mapped ~w', [NotMapped]),
	align(GTAA_Rest2, Cornetto, prefix_candidate, edit_distance_match, _, As3, [prefixLength(3)]),
	target_ambiguity:partition(As3, [ambiguous(Ambiguous3),unambiguous(Unambiguous3)], []),
	debug_align_amb(rest_edit_distance_match, As3, Unambiguous3, Ambiguous3, 3932),
	materialize_alignment_graph(Unambiguous3, [graph(gtaa_cornetto_edit_distance)]).
        %forall(member(R,GTAA_Rest), (rdf_label(R,L),format('~w ~w~n',[R,L]))).


noun_label_comparison :-
	GTAA_Subjects = scheme('http://data.beeldengeluid.nl/gtaa/Onderwerpen'),
 	rdf_equal(cornetto:'NounSynset',CornettoNounSynset),
 	CornettoNoun = type(CornettoNounSynset),
 	rdf_equal(skos:prefLabel,PrefL),

	% preferred label and stemming
	align(GTAA_Subjects, CornettoNoun, prefix_candidate, snowball_match, _, As1, [sourcelabel(PrefL)]),
	target_ambiguity:partition(As1, [ambiguous(Ambiguous1),unambiguous(Unambiguous1)], []),
	debug_align_amb(pref_stem, As1, Unambiguous1, Ambiguous1, 3932),
	materialize_alignment_graph(As1, [graph(stemming_pref)]),
	materialize_alignment_graph(Unambiguous1, [graph(stemming_pref_1n)]),
	materialize_alignment_graph(Ambiguous1, [graph(stemming_pref_11)]),


	% pref+alt label and stemming
	align(GTAA_Subjects, CornettoNoun, prefix_candidate, snowball_match, _, As2, []),
	target_ambiguity:partition(As2, [ambiguous(Ambiguous2),unambiguous(Unambiguous2)], []),
	debug_align_amb(pref_alt_stem, As2, Unambiguous2, Ambiguous2, 3932),
	materialize_alignment_graph(As2, [graph(stemming_pref_alt)]),
       	materialize_alignment_graph(Unambiguous2, [graph(stemming_pref_alt_1n)]),
	materialize_alignment_graph(Ambiguous2, [graph(stemming_pref_alt_11)]),

	% preferred label and exact label
	exact_label_match:matcher(GTAA_Subjects, CornettoNoun, As3, [sourcelabel(PrefL)]),
	target_ambiguity:partition(As3, [ambiguous(Ambiguous3),unambiguous(Unambiguous3)], []),
	debug_align_amb(pref_exaxt, As3, Unambiguous3, Ambiguous3, 3932),
	materialize_alignment_graph(As3, [graph(exact_pref)]),
	materialize_alignment_graph(Unambiguous3, [graph(exact_pref_1n)]),
	materialize_alignment_graph(Ambiguous3, [graph(exact_pref_11)]),

	% pref+alt label and exact label
	exact_label_match:matcher(GTAA_Subjects, CornettoNoun, As4, []),
	target_ambiguity:partition(As4, [ambiguous(Ambiguous4),unambiguous(Unambiguous4)], []),
	debug_align_amb(pref_alt_exact, As4, Unambiguous4, Ambiguous4, 3932),
	materialize_alignment_graph(As4, [graph(exact_pref_alt)]),
	materialize_alignment_graph(Unambiguous4, [graph(exact_pref_alt_1n)]),
	materialize_alignment_graph(Ambiguous4, [graph(exact_pref_alt_11)]),

	true.
	/*maplist(align_source, Ambiguous1, Ss0),
	sort(Ss0, Ss1),
	sort(Ambiguous2, Ambiguous20),
	graph_source_subtract(Ambiguous20, Ss1, AmbAltOnly),
	materialize_alignment_graph(AmbAltOnly, [graph(ambiguous_alt_only)]).*/

verb_label_comparison :-
	GTAA_Subjects0 = scheme('http://data.beeldengeluid.nl/gtaa/Onderwerpen'),
 	rdf_equal(cornetto:'NounSynset',CornettoNounSynset),
 	CornettoNoun = type(CornettoNounSynset),
	rdf_equal(cornetto:'VerbSynset',CornettoVerbSynset),
	CornettoVerb = type(CornettoVerbSynset),
	rdf_equal(skos:prefLabel,PrefL),

	align(GTAA_Subjects0, CornettoNoun, prefix_candidate, snowball_match, _, As, []),
	align_exclude:source_select(GTAA_Subjects0, GTAA_Subjects, [exclude(As)]),

	% preferred label and stemming
	align(GTAA_Subjects, CornettoVerb, prefix_candidate, snowball_match, _, As1, [sourcelabel(PrefL)]),
	target_ambiguity:partition(As1, [ambiguous(Ambiguous1),unambiguous(Unambiguous1)], []),
	debug_align_amb(pref_stem, As1, Unambiguous1, Ambiguous1, 3932),

	% pref+alt label and stemming
	align(GTAA_Subjects, CornettoVerb, prefix_candidate, snowball_match, _, As2, []),
	target_ambiguity:partition(As2, [ambiguous(Ambiguous2),unambiguous(Unambiguous2)], []),
	debug_align_amb(pref_alt_stem, As2, Unambiguous2, Ambiguous2, 3932),

	% preferred label and exact label
	align(GTAA_Subjects, CornettoVerb, source_candidate, exact_label_match, target_candidate, As3, [sourcelabel(PrefL)]),
	target_ambiguity:partition(As3, [ambiguous(Ambiguous3),unambiguous(Unambiguous3)], []),
	debug_align_amb(pref_exaxt, As3, Unambiguous3, Ambiguous3, 3932),

	% pref+alt label and exact label
	align(GTAA_Subjects, CornettoVerb, source_candidate, exact_label_match, target_candidate, As4, []),
	target_ambiguity:partition(As4, [ambiguous(Ambiguous4),unambiguous(Unambiguous4)], []),
	debug_align_amb(pref_alt_exact, As4, Unambiguous4, Ambiguous4, 3932),
	materialize_alignment_graph(As4, [graph(verb_pref_alt_exaxt)]),


	maplist(align_source, Unambiguous4, Ss0),
	sort(Ss0, Ss1),
	sort(Unambiguous2, Unambiguous20),
	graph_source_subtract(Unambiguous20, Ss1, UnambStemOnly),
	materialize_alignment_graph(UnambStemOnly, [graph(unambiguous_stem_only)]).


graph_source_subtract([], _, []) :- !.
graph_source_subtract(As, [], As) :- !.
graph_source_subtract([align(S,_,_)|As], [S|Ss], Rest) :-
	!,
	graph_source_subtract(As, [S|Ss], Rest).
graph_source_subtract([A|As], [S|Ss], [A|Rest]) :-
	A = align(S0,_,_),
	compare(<, S0, S),
	!,
	graph_source_subtract(As, [S|Ss], Rest).
graph_source_subtract(As, [_|Ss], Rest) :-
	graph_source_subtract(As, Ss, Rest).



hierarchy_loop(Ambiguous, Count, Undecided) :-
	align(Ambiguous, _, alignment_element, ancestor_match, _, Ancestor, []),
	align(Ambiguous, _, alignment_element, descendant_match, _, Descendant, []),
	debug_align(ancestor, Ancestor, 3932),
	debug_align(descendant, Descendant, 3932),
	merge_graphs([Ancestor, Descendant, Ambiguous], Enriched),
	most_methods:partition(Enriched, Partition, []),
	memberchk(selected(Selected), Partition),
	memberchk(undecided(Undecided1), Partition),
	debug_partition(most_methods, Partition),
	length(Undecided1, Current),
	(   Current == Count
	->  Undecided = Undecided1
	;   atom_concat(gtaa_cornetto_disambiguated_hierarchy, Count, NGraph),
	    materialize_alignment_graph(Selected, [graph(NGraph)]),
	    hierarchy_loop(Undecided1, Current, Undecided)
	).

debug_align(Method, As, Total) :-
       	length(As, AsN),
	source_count(As, SN),
	Perc is (SN/Total)*100,
	debug(align, '~w ~w ~w (~w%)~n', [Method, AsN, SN, Perc]).

debug_align_amb(Method, As, Unamb, Amb, Total) :-
	length(As, AsN),
	length(Amb, AmbN),
	length(Unamb, UnambN), % just checking
 	source_count(As, SN),
	source_count(Amb, AmbSN),
	source_count(Unamb, UnambSN),
	SPerc is (SN/Total)*100,
	AmbPerc is (AmbSN/Total)*100,
	UnambPerc is (UnambSN/Total)*100,
	debug(align, '~w ~w ~w (~w)~n  unambiguous ~w ~w (~w) ~n  ambiguous ~w ~w (~w)~n',
	      [Method, AsN, SN, SPerc, UnambN, UnambSN, UnambPerc, AmbN, AmbSN, AmbPerc]).

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


align_subtract([], L, L).
align_subtract([align(S,_,_)|As], L1, L) :-
	align_select(S, L1, L0),
 	align_subtract(As, L0, L).

align_select(S, L1, L) :-
	select(align(S,_,_), L1, L0),
	!,
	align_select(S, L0, L).
align_select(_, L, L).


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
