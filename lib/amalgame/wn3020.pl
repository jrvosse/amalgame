:- module(wn3020,
	 [
	 ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(applications(align_stats)).
:- use_module(library(amalgame/alignment_graph)).
:- use_module(library(amalgame/candidates/unmapped_candidate)).
:- use_module(library(amalgame/candidates/source_candidate)).
:- use_module(library(amalgame/candidates/target_candidate)).
:- use_module(library(amalgame/candidates/alignment_element)).
:- use_module(library(amalgame/candidates/prefix_candidate)).
:- use_module(library(amalgame/matchers/exact_label_match)).
:- use_module(library(amalgame/matchers/stem_label_match)).
:- use_module(library(amalgame/matchers/edit_distance_match)).
:- use_module(library(amalgame/matchers/dwim_match)).
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
:- use_module(library(amalgame/map)).

:- debug(align).

delete_created_alignments :-
	rdf_transaction(
			forall(rdfs_individual_of(A, amalgame:'AmalgameAlignment'),
			       rdf_unload(A)
			      )
		       ).

% :- delete_created_alignments.

disamb :-
	findall(T, (rdf(S1,skos:exactMatch, T, _Graph1),
		    rdf(S2,skos:exactMatch,T, _Graph2),
		    S1 \== S2
		   ),
		AmbTargets0
	       ),
	sort(AmbTargets0, AmbTargets),
	fixambtargets(AmbTargets).

fixambtargets([]).
fixambtargets([T|Ts]) :-
	findall(Graph-S-T,
		(rdf(S, skos:exactMatch,T,Graph)
		),
		Targets),
	sort(Targets, [Keep|Discard]),
	debug(align, 'disamb: keep: ~p', [Keep]),
	disgard(Discard),

	fixambtargets(Ts).

disgard([]).
disgard([Graph-S-T|Tail]) :-
	debug(align, 'disamb: del: ~p -> ~p from ~p', [S,T,Graph]),
	rdf_retractall(S,_,T,Graph),
	disgard(Tail).

myalign1 :-
	align_stats:delete_alignment_graphs(_),
	WN30=scheme('http://purl.org/vocabularies/princeton/wn30/'),
	WN20=scheme('http://www.w3.org/2006/03/wn/wn20/'),
	rdf_equal(rdf:type, RDF_type),
	Options = [splitprop(RDF_type)],
	prop_partition:source_select(WN30, WN30_Partition, Options),
	Noun='http://www.w3.org/2006/03/wn/wn20/schema/NounSynset',

	forall((member(PoS-_, WN30_Partition), PoS \== Noun),
	       (   member(PoS-WN30Concepts, WN30_Partition),
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

myalign3 :-
	WN30=scheme('http://purl.org/vocabularies/princeton/wn30/'),
	WN20=scheme('http://www.w3.org/2006/03/wn/wn20/'),
	myalign(_, WN30, WN20).

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
	align(SourceVoc, TargetVoc, source_candidate, exact_label_match, target_candidate, GlossMatch0, OptionsDef),
	sort(GlossMatch0, GlossMatch),
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
	graph_name(a_unambiguous_gloss,   Type, UnambiguousGlossName),
	graph_name(b_ambiguous_gloss,     Type, AmbiguousGlossName),
	graph_name(c_gloss_unique_label,	Type, GlossLabelName),
	graph_name(d_nogloss_uniquelabel,	Type, NoGlossLabelName),
	graph_name(e_nogloss_mostlabel,	Type, NoGlossMostLabelName),
	graph_name(f_best_gloss_label,    Type, BestGlossLabelName),
	graph_name(z_ambiguous_label,     Type, _AmbiLabelName),
	materialize_alignment_graph(UnambiguousGloss,      [graph(UnambiguousGlossName)]),
	materialize_alignment_graph(AmbiguousGlossLabel,   [graph(AmbiguousGlossName)]),
	materialize_alignment_graph(UnambiguousGlossLabel, [graph(GlossLabelName)]),
	materialize_alignment_graph(UnambiguousLabel,      [graph(NoGlossLabelName)]),
	materialize_alignment_graph(MostLabels,	           [graph(NoGlossMostLabelName)]),
	materialize_alignment_graph(BestGlossAndLabel,	   [graph(BestGlossLabelName)]),
	% materialize_alignment_graph(AmbiLabel,             [graph(AmbiLabelName)]),

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
	graph_name(g_disamb_label, Type, MostMethodLabelName),
	materialize_alignment_graph(MostMethodsLabelMatch, [graph(MostMethodLabelName)]),

	% graph_name(ambiguous_undecided, Type, LeftOversName),
	% member(undecided(LeftOvers), MostMethods),
	% materialize_alignment_graph(LeftOvers, [graph(LeftOversName)]),

	true.

near(_, SourceVoc, TargetVoc) :-
	rdf_equal(SkosAlt, skos:altLabel),
	OptionsLabel = [ sourcelabel(SkosAlt), targetlabel(SkosAlt)],
	align(SourceVoc, TargetVoc, unmapped_candidate, dwim_match, target_candidate, _DwimMatch, OptionsLabel),
	true.


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
	% (   option(target_type(TType), Options) -> true; rdf_equal(rdfs:'Resource', TType)),
	% (   option(source_type(SType), Options) -> true; rdf_equal(rdfs:'Resource', SType)),
	(   nonvar(Candidate)
	->  G0 = (
		 Candidate:candidate(Source, Target, A0, Options),
		  Match:match(A0, A, Options),
		  A0 = align(S,_,_),
		  A = align(_,T,_),
		  rdf(S, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', SType),
		  rdf(T, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', TType),
		  type_match(SType, TType)
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

type_match(T,T).
type_match(A,S) :-
	A='http://www.w3.org/2006/03/wn/wn20/schema/AdjectiveSynset',
	S='http://www.w3.org/2006/03/wn/wn20/schema/AdjectiveSatelliteSynset'.
type_match(S,A) :-
	A='http://www.w3.org/2006/03/wn/wn20/schema/AdjectiveSynset',
	S='http://www.w3.org/2006/03/wn/wn20/schema/AdjectiveSatelliteSynset'.
