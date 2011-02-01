:- module(wn3020,
	 [
	 ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(applications(align_stats)).
:- use_module(library(amalgame/alignment_graph)).
:- use_module(library(amalgame/candidates/source_candidate)).
:- use_module(library(amalgame/candidates/target_candidate)).
:- use_module(library(amalgame/candidates/alignment_element)).
:- use_module(library(amalgame/candidates/prefix_candidate)).
:- use_module(library(amalgame/matchers/exact_label_match)).
:- use_module(library(amalgame/matchers/stem_label_match)).
:- use_module(library(amalgame/matchers/edit_distance_match)).
:- use_module(library(amalgame/matchers/jaccard_match)).
:- use_module(library(amalgame/matchers/ancestor_match)).
:- use_module(library(amalgame/matchers/related_match)).
:- use_module(library(amalgame/matchers/descendant_match)).
:- use_module(library(amalgame/partition/target_ambiguity)).
:- use_module(library(amalgame/partition/best_label)).
:- use_module(library(amalgame/partition/best_numeric)).
:- use_module(library(amalgame/partition/most_labels)).
:- use_module(library(amalgame/partition/most_methods)).
:- use_module(library(amalgame/source/align_exclude)).
:- use_module(library(amalgame/source/prop_partition)).

myalign1 :-
	delete_alignment_graphs(_),
	WN30=scheme('http://purl.org/vocabularies/princeton/wn30/'),
	WN20=scheme('http://www.w3.org/2006/03/wn/wn20/'),
	rdf_equal(rdf:type, RDF_type),
	Options = [splitprop(RDF_type)],
	prop_partition:source_select(WN30, WN30_Partition, Options),
	Noun='http://www.w3.org/2006/03/wn/wn20/schema/NounSynset',

	forall(member(PoS-_, WN30_Partition),
	       (   member(PoS-WN30Concepts, WN30_Partition),
		   PoS \== Noun,
		   debug(align, '~n~nAligning ~p', [PoS]),
		   myalign(PoS,WN30Concepts, WN20)
	       )
	      ).
/*
	prop_partition:source_select(WN20, WN20_Partition, Options),
	Adverb='http://www.w3.org/2006/03/wn/wn20/schema/AdverbSynset',
	member(Adverb-WN30Adverbs, WN30_Partition),
	member(Adverb-WN20Adverbs, WN20_Partition),
	myalign(Adverb, WN30Adverbs, WN20Adverbs).

*/

myalign2 :-
	Source = 'http://purl.org/vocabularies/princeton/wn30/wn20s:\'AdverbSynset\'',
	Target ='http://www.w3.org/2006/03/wn/wn20/wn20s:\'AdverbSynset\'',
	myalign(_,scheme(Source),scheme(Target)).

debug_partition(Label, Partition) :-
	forall(member(SetPred, Partition),
	       (SetPred =.. [Type, Set],
		source_count(Set, Count),
		debug(align, '~w ~w: ~w', [Label, Type, Count])
	       )
	      ).

graph_name(Prefix, Type, Name) :-
	(   var(Type)
	->  Name = Prefix
	;   rdf_global_id(_NS:Local, Type),
	    format(atom(Suffix), '~p', [Local]),
	    atomic_list_concat([Prefix,'_',Suffix],Name)
	).

myalign(Type, SourceVoc, TargetVoc) :-
	rdf_equal(SkosDef, skos:definition),
	rdf_equal(SkosAlt, skos:altLabel),

	% Step 1a: align by exact label match on skos:definition,  split off ambiguous and count results
	% Ambiguous results will be further processed in step 1b, non matching concepts in step 2.
	OptionsDef = [threshold(-1.0), max_dist(3),
		      sourcelabel(SkosDef), targetlabel(SkosDef),
		      source_type(Type), target_type(Type) ],
	align(SourceVoc, TargetVoc, source_candidate, exact_label_match, target_candidate, GlossMatch, OptionsDef),
	target_ambiguity:partition(GlossMatch, [ambiguous(AmbiguousGloss),unambiguous(UnambiguousGloss)], []),
	debug_partition('Gloss match',  [ambiguous(AmbiguousGloss),unambiguous(UnambiguousGloss)]),

	% Step 1b: align ambiguous gloss matches by exact label match on skos:altLabel, split off ambiguous and count results
	% We assume ambiguous results here are concepts that are splitted or merged in the two versions
	% and thus OK
	OptionsLabel = [ sourcelabel(SkosAlt), targetlabel(SkosAlt), target_type(Type) ],
	align(AmbiguousGloss, _, alignment_element, exact_label_match, _, GlossLabelMatch, OptionsLabel),
	target_ambiguity:partition(GlossLabelMatch, [ambiguous(AmbiguousGlossLabel),unambiguous(UnambiguousGlossLabel)], []),
	debug_partition('Ambiguous gloss + label', [ambiguous(AmbiguousGlossLabel),unambiguous(UnambiguousGlossLabel)]),
	merge_graphs([UnambiguousGloss, UnambiguousGlossLabel, AmbiguousGlossLabel], GoodGlossLabelMatches),

	% Step2a: exact label match on concepts with non-matching glosses
	align_exclude:source_select(SourceVoc, Source_rest, [exclude(GoodGlossLabelMatches)]),
	align(Source_rest, TargetVoc, source_candidate, exact_label_match, target_candidate, LabelMatch, OptionsLabel),
	target_ambiguity:partition(LabelMatch, [ambiguous(AmbiguousLabel),unambiguous(UnambiguousLabel)], []),
	debug_partition('Exact label on no gloss match',  [ambiguous(AmbiguousLabel),unambiguous(UnambiguousLabel)]),

	% Step 2b: First try disambiguation by counting number of matching sense labels (cheap, counting existing matches)
	most_labels:partition(AmbiguousLabel, [selected(MostLabels), discarded(Discarded2), undecided(AmbiguousLabel2)], []),
	debug_partition('Exact label, most label', [selected(MostLabels), discarded(Discarded2), undecided(AmbiguousLabel2)]),

	% Step 2c: Then try disambiguation by jaccard similarity on the glosses (more expensive, need to compute jaccard)
	align(AmbiguousLabel2, TargetVoc, alignment_element, jaccard_match, target_candidate, JaccardMatch, OptionsDef),
	best_numeric:partition(JaccardMatch, JaccardPartition, []),
	debug_partition(jaccard_gloss, JaccardPartition),
	member(selected(BestGlossAndLabel), JaccardPartition),
	member(undecided(AmbiLabel), JaccardPartition),

	% Step 3: Materialize the good stuff found so far, so we can use this to do structural matching later
	debug(align, '~p: Materializing alignments found so far.', [Type]),
	graph_name(unambiguous_gloss,   Type, UnambiguousGlossName),
	graph_name(ambiguous_gloss,     Type, AmbiguousGlossName),
	graph_name(gloss_unique_label,	Type, GlossLabelName),
	graph_name(nogloss_uniquelabel,	Type, NoGlossLabelName),
	graph_name(nogloss_mostlabel,	Type, NoGlossMostLabelName),
	graph_name(best_gloss_label,    Type, BestGlossLabelName),
	materialize_alignment_graph(UnambiguousGloss,      [graph(UnambiguousGlossName)]),
	materialize_alignment_graph(AmbiguousGlossLabel,   [graph(AmbiguousGlossName)]),
	materialize_alignment_graph(UnambiguousGlossLabel, [graph(GlossLabelName)]),
	materialize_alignment_graph(UnambiguousLabel,      [graph(NoGlossLabelName)]),
	materialize_alignment_graph(MostLabels,	           [graph(NoGlossMostLabelName)]),
	materialize_alignment_graph(BestGlossAndLabel,	   [graph(BestGlossLabelName)]),

	% Disambiguate remaining with structural properties
	merge_graphs([AmbiLabel], ToBeAmbiguated),
	source_count(ToBeAmbiguated, AmbN),
	debug(align, 'To be ambiguated: ~w', [AmbN]),
	align(ToBeAmbiguated, _, alignment_element, ancestor_match,  _, AncMatch, OptionsDef),
	align(ToBeAmbiguated, _, alignment_element, descendant_match, _, DecMatch, OptionsDef),
	align(ToBeAmbiguated, _, alignment_element, related_match,   _, RelMatch, OptionsDef),
	merge_graphs([ToBeAmbiguated, AncMatch, DecMatch, RelMatch], DisambResults),
	most_methods:partition(DisambResults, MostMethods, []),
	debug_partition('Most methods: ', MostMethods),
	member(selected(MostMethodsLabelMatch), MostMethods),
	member(undecided(LeftOvers), MostMethods),
	graph_name(disamb_label, Type, MostMethodLabelName),
	graph_name(ambiguous_undecided, Type, LeftOversName),
	materialize_alignment_graph(MostMethodsLabelMatch, [graph(MostMethodLabelName)]),
	materialize_alignment_graph(LeftOvers, [graph(LeftOversName)]),

	true.
/*

	align_exclude:source_select(SourceVoc, Source_rest, [exclude(GoodGlossLabelMatches)]),
 	align(Source_rest, TargetVoc, source_candidate, stem_label_match, target_candidate, StemMatch, []),
	target_ambiguity:partition(StemMatch, [ambiguous(Ambiguous2),unambiguous(Unambiguous2)], []),
  	source_count(StemMatch, A2N),
	source_count(Ambiguous2, Amb2N),
	source_count(Unambiguous2, UnAmb2N),
	debug(align, 'stem_label_match ~w ~nunambiguous ~w~nambiguous ~w~n', [A2N, UnAmb2N, Amb2N]),
	materialize_alignment_graph(Unambiguous2, [graph(gtaa_cornetto_stem_label_unambiguous)]),


	true.

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

	% match the remainging part from the remaining part :)
	align_exclude:source_select(GTAA_Rest, GTAA_Rest2, [exclude(As2)]),
 	align(GTAA_Rest2, Cornetto, prefix_candidate, edit_distance_match, _, As3, [threshold(10)]),
	target_ambiguity:partition(As3, [ambiguous(Ambiguous3),unambiguous(Unambiguous3)], []),
	source_count(As3, A3N),
 	source_count(Ambiguous3, Amb3N),
	source_count(Unambiguous3, UnAmb3N),
	debug(align, 'edit_distance_match ~w~nunambiguous ~w~n ambiguous~w~n', [A3N, UnAmb3N, Amb3N]),
	materialize_alignment_graph(Unambiguous3, [graph(gtaa_cornetto_edit_distance_unambiguous)]),

	% @TBD add iteration
	align(Undecided2, _, alignment_element, ancestor_match, _, As4, []),
	align(Undecided2, _, alignment_element, descendant_match, _, As5, []),
	source_count(As4, As4N),
	source_count(As5, As5N),
	debug(align, 'hierarchy match on ~w~nancestor ~w~ndescendant~w~n ', [Und2N, As4N, As5N]),
	merge_graphs([As4, As5, Undecided2,Ambiguous3], As6),
	most_methods:partition(As6, [selected(Selected3), discarded(_Discarded3), undecided(Undecided3)], []),
 	source_count(As6, As6N),
	source_count(Selected3, Sel3N),
	source_count(Undecided3, Und3N),
	debug(align, 'most methods ~w ~n unambiguous ~w~n ambiguous~w~n', [As6N, Sel3N, Und3N]),
	materialize_alignment_graph(Selected3, [graph(gtaa_cornetto_disambiguated_most_methods)]),
	materialize_alignment_graph(Undecided3, [graph(gtaa_cornetto_ambiguous_most_methods)]).
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
	debug(align, 'Running matcher ~w', [Match]),
	(   option(target_type(TType), Options) -> true; rdf_equal(rdfs:'Resource', TType)),
	(   option(source_type(SType), Options) -> true; rdf_equal(rdfs:'Resource', SType)),
        (   nonvar(Candidate)
	->  G0 = ( Candidate:candidate(Source, Target, A0, Options),
	           A0 = align(S,_,_),
		   rdfs_individual_of(S,SType),
	           Match:match(A0, A, Options),
		   A = align(_,T,_),
		   rdfs_individual_of(T,TType)
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


spyme.
