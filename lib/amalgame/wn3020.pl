:- module(wn3020,
	 [
	 ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(applications(align_stats)).
:- use_module(library(amalgame/alignment_graph)).
:- use_module(library(amalgame/select/select1_1)).
:- use_module(library(amalgame/select/arity_select)).
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
:- use_module(library(amalgame/matchers/isub_match)).
:- use_module(library(amalgame/matchers/ancestor_match)).
:- use_module(library(amalgame/matchers/related_match)).
:- use_module(library(amalgame/matchers/descendant_match)).
:- use_module(library(amalgame/partition/target_ambiguity)).
:- use_module(library(amalgame/partition/best_label)).
:- use_module(library(amalgame/partition/best_numeric)).
:- use_module(library(amalgame/partition/most_labels_jacco)).
:- use_module(library(amalgame/partition/most_methods)).
:- use_module(library(amalgame/source/align_exclude)).
:- use_module(library(amalgame/source/voc_exclude)).
:- use_module(library(amalgame/source/prop_partition)).
:- use_module(library(amalgame/map)).
:- use_module(library(amalgame/edoal)).

:- debug(align).

delete_created_alignments :-
	rdf_transaction(
			forall(rdfs_individual_of(A, amalgame:'AmalgameAlignment'),
			       rdf_unload(A)
			      )
		       ).

% :- delete_created_alignments.

disamb :-
	findall(S, (has_map([S,T1],edoal,_),
		    has_map([S,T2],edoal,_),
		    T1 \== T2
		   ),
		One_to_many	       ),
	sort(One_to_many, One_to_many_sorted),
	disamb_one_to_many(One_to_many_sorted).

disamb_one_to_many([]) :-!.
disamb_one_to_many([S|Ss]) :-
	findall(Graph-[S,T,O],
		(has_map([S,T],edoal,O,Graph)
		),
		Targets),
	group_pairs_by_key(Targets, Grouped),
	keysort(Grouped, [_Key-First|_]),
	(   First = [S,T,O]
	->  debug(align, 'disamb: keep: ~p/~p', [S,T]),
	    selectchk(First, Targets, Discarded),
	    moveto(Discarded, discarded_1ton_mappings)
	;   moveto(Targets, undecided_1ton_mappings)
	),
	disamb_one_to_many(Ss).

fixambtargets([]).
fixambtargets([T|Ts]) :-
	findall(Graph-[S,T,O],
		(has_map([S,T],edoal,O,Graph)
		),
		Targets),
	group_pairs_by_key(Targets, Grouped),
	keysort(Grouped, [_Key-First|_]),
	(   First = [S,T,O]
	->  debug(align, 'disamb: keep: ~p/~p', [S,T]),
	    selectchk(First, Targets, Discarded),
	    moveto(Discarded, discarded_nto1_mappings)
	;   moveto(Targets, undecided_nto1_mappings)
	),
	fixambtargets(Ts).

moveto([],_) :- !.
moveto([Graph-[S,T,O]|Tail], Target) :-
	debug(align, 'move: ~p -> ~p from ~p to ~p', [S,T,Graph,Target]),
	delete_map([S,T],O,Graph),
	assert_cell(S,T,[graph(Target)|O]),
	moveto(Tail, Target).

delete_map([S,T], _O, Graph) :-
	rdf(Bnode, align:entity1, S, Graph),
	rdf(Bnode, align:entity2, T, Graph),!,
	rdf_retractall(Bnode, _, _, Graph).

d1:-
	T = [
	     align(a1, b1, []),
	     align(a2, b2,[]),
	     align(a1, b2,[]),
	     align(a2, b1,[]),
	     align(a2, b3, [])
	     ],
	select1_1:partition(T, P, []),
	debug_partition(debug, P).

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
	Source = 'http://purl.org/vocabularies/princeton/wn30/wn20schema:\'AdverbSynset\'',
	Target = 'http://www.w3.org/2006/03/wn/wn20/',
	 % Target ='http://www.w3.org/2006/03/wn/wn20/wn20s:\'AdverbSynset\'',
	myalign(_,scheme(Source),scheme(Target)).

myalign3 :-
	WN30=scheme('http://purl.org/vocabularies/princeton/wn30/'),
	WN20=scheme('http://www.w3.org/2006/03/wn/wn20/'),
	myalign(_, WN30, WN20).



graph_name(Prefix, Type, Name) :-
	(   var(Type)
	->  Name = Prefix
	;   rdf_global_id(_NS:Local, Type),
	    format(atom(Suffix), '~p', [Local]),
	    atomic_list_concat([Prefix,'-x-',Suffix],Name)
	).

myalign(Type, SourceVoc, TargetVoc) :-
	rdf_equal(SkosDef, skos:definition),
	rdf_equal(SkosAlt, skos:altLabel),
	OptionsLabel = [ sourcelabel(SkosAlt), targetlabel(SkosAlt), target_type(Type) ],
	OptionsDef = [threshold(-1.0), max_dist(3),
		      sourcelabel(SkosDef), targetlabel(SkosDef),
		      source_type(Type), target_type(Type) ],


	% Step 1a: align by exact label match on skos:definition,  split off ambiguous and count results
	% Ambiguous results will be further processed in step 1b, non matching concepts in step 2.
	align(SourceVoc, TargetVoc, source_candidate, exact_label_match, target_candidate, GlossMatch, OptionsDef, _A1a),
	% materialize_alignment_graph(GlossMatch, [graph(rawgloss)]), % fail,
	arity_select:selecter(GlossMatch, UnambiguousGloss, AmbiguousGloss, _, []),
	debug_partition('Gloss match',  [ambiguous(AmbiguousGloss),unambiguous(UnambiguousGloss)]),
	% materialize_alignment_graph(AmbiguousGloss, [graph(glossNM)]), % fail,

	% Step 1b: align ambiguous gloss matches by exact label match on skos:altLabel,
	% split off ambiguous and count results.
	% We assume ambiguous results here are concepts that are splitted or merged in the two versions
	% and thus OK
	align(AmbiguousGloss, _, alignment_element, exact_label_match, _, GlossLabelMatch, OptionsLabel, _A1b),
	% materialize_alignment_graph(GlossLabelMatch, [graph(rawglosslabel)]),
	arity_select:selecter(GlossLabelMatch, UnambiguousGlossLabel, AmbiguousGlossLabel, _, []),
	% materialize_alignment_graph(UnambiguousGlossLabel, [graph(glosslabel11)]),
	% materialize_alignment_graph(AmbiguousGlossLabel, [graph(glosslabelNM)]),

	debug_partition('Ambiguous gloss + label', [ambiguous(AmbiguousGlossLabel),unambiguous(UnambiguousGlossLabel)]),
	merge_graphs([UnambiguousGloss, UnambiguousGlossLabel, AmbiguousGlossLabel], GoodGlossLabelMatches),

	% Step2a: exact label match on concepts with non-matching glosses
	% align_exclude:source_select(SourceVoc, Source_rest, [exclude(GoodGlossLabelMatches)]),
	gtrace,
	voc_exclude:concept_selecter(SourceVoc, Source_rest, [exclude_sources(GoodGlossLabelMatches)]),
	align(Source_rest, TargetVoc, alignment_element, exact_label_match, target_candidate, LabelMatch0, OptionsLabel, _A2a),
	voc_exclude:concept_selecter(LabelMatch0, LabelMatch, [exclude_targets(GoodGlossLabelMatches)]),
	arity_select:selecter(LabelMatch, UnambiguousLabel, AmbiguousLabel, _, []),


	% Step 2b: First try disambiguation by counting number of matching sense labels (cheap, counting existing matches)
	% most_labels_jacco:partition(LabelMatch,
	%		 [one_to_one(UnambiguousLabel), selected(MostLabels), discarded(Discarded2), undecided(AmbiguousLabel2)], []),
	% debug_partition('Exact label, most label', [selected(MostLabels), discarded(Discarded2), undecided(AmbiguousLabel2)]),

	% Step 2c: Then try disambiguation by jaccard similarity on the glosses (more expensive, need to compute jaccard)
	% align(AmbiguousLabel, TargetVoc, alignment_element, jaccard_match, target_candidate, JaccardMatch, OptionsDef, A2c),
	align(AmbiguousLabel, TargetVoc, alignment_element, isub_match, target_candidate, JaccardMatch, OptionsDef, _A2c),
	best_numeric:partition(JaccardMatch, JaccardSPartition, [disamb(source)]),
	debug_partition(jaccard_gloss_source, JaccardSPartition),
	memberchk(selected(BestGlossAndLabelS), JaccardSPartition),
	memberchk(undecided(AmbiLabelS), JaccardSPartition),
	best_numeric:partition(BestGlossAndLabelS, JaccardTPartition, [disamb(target)]),
	memberchk(selected(BestGlossAndLabel), JaccardTPartition),
	memberchk(undecided(AmbiLabelT), JaccardSPartition),

	rdf_equal(amalgame:untyped,Type),
	% Step 3: Materialize the good stuff found so far, so we can use this to do structural matching later
	debug(align, '~p: Materializing alignments found so far.', [Type]),
	% graph_name('1a_unambiguous_gloss',   Type, UnambiguousGlossName),
	% graph_name('1b_gloss_unique_label',  Type, GlossLabelName),
	% graph_name('1c_ambiguous_gloss',     Type, AmbiguousGlossName),
	graph_name('2a_nogloss_uniquelabel', Type, NoGlossLabelName),
	% graph_name('2b_nogloss_mostlabel',   Type, NoGlossMostLabelName),
	graph_name('2c_best_gloss_label',    Type, BestGlossLabelName),
	% graph_name('xz_ambiguous_label',     Type, AmbiLabelName),
	% materialize_alignment_graph(UnambiguousGloss,      [graph(UnambiguousGlossName), process(A1a)]),
	%materialize_alignment_graph(AmbiguousGlossLabel,   [graph(AmbiguousGlossName), process(A1b)]),
	% materialize_alignment_graph(UnambiguousGlossLabel, [graph(GlossLabelName), process(A1a)]),
	materialize_alignment_graph(UnambiguousLabel,      [graph(NoGlossLabelName)]),
	% materialize_alignment_graph(MostLabels,	           [graph(NoGlossMostLabelName)]),
	materialize_alignment_graph(BestGlossAndLabel,	   [graph(BestGlossLabelName)]),
	% materialize_alignment_graph(AmbiLabel,             [graph(AmbiLabelName)]),

	% Disambiguate remaining with structural properties
	merge_graphs([AmbiLabelS, AmbiLabelT], ToBeAmbiguated),
	align(ToBeAmbiguated, _, alignment_element, ancestor_match,  _, AncMatch, OptionsDef, _A3a),
	align(ToBeAmbiguated, _, alignment_element, descendant_match, _, DecMatch, OptionsDef, _A3b),
	align(ToBeAmbiguated, _, alignment_element, related_match,   _, RelMatch, OptionsDef, _A3c),
	merge_graphs([ToBeAmbiguated, AncMatch, DecMatch, RelMatch], DisambResults),
	most_methods:partition(DisambResults, MostMethods, []),
	debug_partition('Most methods: ', MostMethods),
	memberchk(selected(MostMethodsLabelMatch), MostMethods),
	graph_name('2d_disamb_label', Type, MostMethodLabelName),
	materialize_alignment_graph(MostMethodsLabelMatch, [graph(MostMethodLabelName)]),

	% graph_name(ambiguous_undecided, Type, LeftOversName),
	% member(undecided(LeftOvers), MostMethods),
	% materialize_alignment_graph(LeftOvers, [graph(LeftOversName)]),

	true.

near(_, SourceVoc, TargetVoc) :-
	rdf_equal(SkosAlt, skos:altLabel),
	OptionsLabel = [ sourcelabel(SkosAlt), targetlabel(SkosAlt)],
	align(SourceVoc, TargetVoc, unmapped_candidate, dwim_match, target_candidate, _DwimMatch, OptionsLabel,_),
	true.


%%	align(+SourceGraph, +TargetGraph, ?CandidateMethod,
%%	+MatchMethod, ?TestMethod, -Output, +Options)
%
%	Output is a list of alignments.

align(Source, Target, Candidate, Match, Test, Output, Options, Process) :-
	rdf_global_id(amalgame:Match, MatchURL),
	(   (nonvar(Source), Source = scheme(SourceURL)) -> true; SourceURL = unknown_source),
	(   (nonvar(Target), Target = scheme(TargetURL)) -> true; TargetURL = unknown_target),
	Process=[
		 matcher(MatchURL),
		 match_options(Options),
		 was_derived_from([SourceURL, TargetURL])],
	debug(align, 'Running matcher ~w', [Match]),
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

type_match(_,_) :-!.
type_match(A,S) :-
	A='http://www.w3.org/2006/03/wn/wn20/schema/AdjectiveSynset',
	S='http://www.w3.org/2006/03/wn/wn20/schema/AdjectiveSatelliteSynset',
	!.
type_match(S,A) :-
	A='http://www.w3.org/2006/03/wn/wn20/schema/AdjectiveSynset',
	S='http://www.w3.org/2006/03/wn/wn20/schema/AdjectiveSatelliteSynset',
	!.
