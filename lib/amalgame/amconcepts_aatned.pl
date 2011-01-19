:- module(amc2aatned,
	 [align/0
	 ]).

:- use_module(library(amalgame/alignment_graph)).
:- use_module(library(amalgame/candidates/source_candidate)).
:- use_module(library(amalgame/candidates/target_candidate)).
:- use_module(library(amalgame/candidates/alignment_element)).
:- use_module(library(amalgame/candidates/prefix_candidate)).
:- use_module(library(amalgame/matchers/exact_label_match)).
:- use_module(library(amalgame/matchers/stem_label_match)).
:- use_module(library(amalgame/matchers/edit_distance_match)).
:- use_module(library(amalgame/matchers/ancestor_match)).
:- use_module(library(amalgame/matchers/descendant_match)).
:- use_module(library(amalgame/partition/target_ambiguity)).
:- use_module(library(amalgame/partition/best_label)).
:- use_module(library(amalgame/partition/most_labels)).
:- use_module(library(amalgame/partition/most_methods)).
:- use_module(library(amalgame/partition/voc_objprop_split)).
:- use_module(library(amalgame/source/align_exclude)).

align :-
	AM_Thesaurus = scheme('http://purl.org/collections/nl/am/AM_ConceptScheme'),
      %  AATNed = scheme('http://purl.org/vocabularies/rkd/aatned/aatned'),

	OP = 'http://purl.org/collections/nl/am/termType',
	OV = 'http://purl.org/collections/nl/am/t-termtypeSUBJECT',
	voc_objprop_split:partition(AM_Thesaurus,
				    [selected(Sel), discarded(Disc)],
				    [splitprop(OP),splitval(OV)]),


	materialize_vocabulary_graph(Sel, [graph(am_subj)]),
	materialize_vocabulary_graph(Disc, [graph(am_not_subj)]).


materialize_vocabulary_graph(Voc, Options):-
	option(graph(Graph),Options),
	rdf_transaction(
			(
			rdf_assert(Graph, rdf:type, amalgame:'DerivedConceptScheme', Graph),
			forall(graph_member(Mem, Voc), rdf_assert(Mem, skos:inScheme, Graph, Graph))
			)
		       ).

