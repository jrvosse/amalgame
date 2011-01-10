align(Input, Output, Options) :-
	option(candidate(CModule), Options),
	option(match(MModule), Options),
	findall(Candidate-Evidence,
		CModule:candidate_generator(Input, Candidate, Options),
		MModule:match(Candidate, Evidence, Options),
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
