:- module(align,
	  [align/3
	  ]).

:- use_module(library(amalgame/candidates/source_candidates)).
:- use_module(library(amalgame/candidates/target_candidates)).
:- use_module(library(amalgame/matchers/exact_label_match)).


test_align :-
	WN30='http://purl.org/vocabularies/princeton/wn30/',
	WN20='http://www.w3.org/2006/03/wn/wn20/',
	Input = schemes(WN30, WN20),
	Options = [candidate(source_candidates),
		   match(exact_label_match),
		   test(target_candidates)
		  ],
	align(Input, Output, Options),
	materialize_alignment_graph(Output, foobar, []).

align(Input, Output, Options) :-
	option(candidate(CModule), Options),
	option(match(MModule), Options),
	option(test(TModule), Options, skip), % Optional
	findall(Candidate-Evidence,
		(   CModule:candidate(Input, Candidate, Options),
		    MModule:match(Candidate, Evidence, Options),
		    (	TModule \= skip
		    ->	TModule:candidate(Input, Candidate, Options)
		    ;	true
		    )
		),
		Output).


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
save_alignment_graph([Candidate-Graph|Groups], Options) :-
	Candidate = candidate(Source, Target),
	assert_cell(Source, Target,  [ provenance(Graph) | Options ]),
	save_alignment_graph(Groups, Options).
