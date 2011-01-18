:- module(align,
	  [
	  ]).

:- use_module(library(amalgame/candidates/source_candidates)).
:- use_module(library(amalgame/candidates/target_candidates)).
:- use_module(library(amalgame/matchers/exact_label_match)).
:- use_module(library(amalgame/edoal)).

test_align :-
	WN30='http://purl.org/vocabularies/princeton/wn30/',
	WN20='http://www.w3.org/2006/03/wn/wn20/',
	rdf_equal(SkosDef, skos:definition),
	Input = schemes(WN30, WN20),
	Options = [candidate(source_candidates),
		   match(exact_label_match),
		   test(target_candidates),
		   sourcelabel(SkosDef),
		   targetlabel(SkosDef)
		  ],
	align(Input, Output, Options),
	materialize_alignment_graph(Output, foobar, Options).

align(Input, Output, Options) :-
	option(candidate(CModule), Options),
	option(match(MModule), Options),
	option(test(TModule), Options, skip), % Optional
	findall(Alignment,
		(   CModule:candidate(Input, Candidate, Options),
		    MModule:match(Candidate, Alignment, Options),
		    (	TModule \= skip
		    ->	TModule:candidate(Input, Alignment, Options)
		    ;	true
		    )
		),
		Output),
	length(Output, N_output),
	debug(align, 'align/3 found ~w alignments', [N_output]).


save_component_provenance(Module, Input, Output, Options) :-
	module_options(Module, Options, ModuleOptions),
 	option(graph(Graph), Options, align),
	rdf_bnode(Process),
	rdf_assert(Process, rdf:type, Module, Graph),
	opm_component_inputs(Input, Process, Graph),
	opm_component_outputs(Output, Process, Graph),
	opm_component_options(ModuleOptions, Graph),
	rdf_assert(Process, rdfs:label, literal('amalgame label matcher'), Graph),
	opm_was_generated_by(Process, Graph, Graph, Options).


materialize_alignment_graph(Input, Graph, Options) :-
	rdf_assert(Graph, rdf:type, amalgame:'AmalgameAlignment', Graph),
 	save_alignment_graph(Input, Options).

save_alignment_graph([], _).
save_alignment_graph([Align-ProvList|Groups], Options) :-
	Align = align(Source, Target, _OldProvList),
	assert_cell(Source, Target,  [ provenance(ProvList) | Options ]),
	save_alignment_graph(Groups, Options).

