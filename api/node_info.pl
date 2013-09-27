:- module(ag_api_node_info,
	  [
	  ]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(amalgame/amalgame_modules)).
:- use_module(library(amalgame/ag_stats)).
:- use_module(library(amalgame/voc_stats)).
:- use_module(library(amalgame/util)).
:- use_module(components(label)). % we need rdf_link//1 from this module

:- use_module(library(amalgame/ag_controls)).

% http handlers for this applications

:- http_handler(amalgame(private/info), http_node_info, []).

%%	http_eq_info(+Request)
%
%	Emit HTML snippet with information about an amalgame URI

http_node_info(Request) :-
	http_parameters(Request,
			[ url(URL,
			      [description('URL of a node (mapping,vocab,process,strategy)')]),
			  alignment(Strategy,
				    [description('URL of the alignment strategy')])
		       ]),
	amalgame_info(URL, Strategy, Stats),
	amalgame_provenance(URL, Strategy, Prov),
	append(Prov, Stats, Info),
	amalgame_parameters(URL, Strategy, Params),
	phrase(html([\html_prop_table(Info),
		     \html_form(Params, URL)
		    ]),
	       HTML),
	html_current_option(content_type(Type)),
	format('Content-type: ~w~n~n', [Type]),
	print_html(HTML).

%%	html_prop_table(+Pairs)
%
%	Emit an HTML table with key-value pairs.

html_prop_table(Pairs) -->
	html(table(tbody(\html_rows(Pairs)))).

html_rows([]) --> !.
html_rows([Key-Value|Ss]) -->
	html_row(Key, Value),
	html_rows(Ss).

html_row(Key, set(Values)) -->
	 html(tr([th(Key),
		  td([])
		 ])),
	 html_rows(Values).
html_row(Key, Value) -->
	 html(tr([th(Key),
		  td(\html_cell(Value))
		])).

html_cell([]) --> !.
html_cell(Vs) -->
	{ is_list(Vs)
	},
	!,
	html_cell_list(Vs).
html_cell(V) -->
	html(V).

html_cell_list([V]) -->
	html_cell(V).
html_cell_list([V|Vs]) -->
	html_cell(V),
	html(', '),
	html_cell_list(Vs).


%%	html_form(+Parameters, +URI)
%
%	Emit HTML with parameter form.

html_form([], _) --> !.
html_form(Params, URI) -->
	html(div(class(parameters),
		 table([input([type(hidden), name(process), value(URI)]),
			input([type(hidden), name(update), value(true)]),
			\html_parameter_form(Params)
		       ]))).



%%	amalgame_info(+MappingId, +Strategy, -Info)
%
%	Stats of a resource (mapping)

amalgame_info(URL, Strategy, Stats) :-
	rdfs_individual_of(URL, amalgame:'Mapping'),
	!,
	BasicStats = [
	    'matched source concepts'-SN,
	    'matched source top concepts'-TopSatom,
	    'avg. source depth' - DepthSatom,
	    'max. source depth' - MaxDepthS,
	    'avg. # children source' - ChildSatom,
	    'avg. # children target' - ChildTatom,
	    'matched target concepts'-TN,
	    'matched target top concepts'-TopTatom,
	    'avg. target depth' - DepthTatom,
	    'max. target depth' - MaxDepthT,
	    'max. # children source' - MaxChildS,
	    'max. # children target' - MaxChildT
	],
	node_stats(Strategy, URL, MStats),
	% option(sourceVoc(VocS), MStats),
	% option(targetVoc(VocT), MStats),
	% voc_property(VocS, nrOfTopConcepts(TotalTopsS)),
	% voc_property(VocT, nrOfTopConcepts(TotalTopsT)),
	option(totalCount(MN), MStats),
	option(mappedSourceConcepts(SN0), MStats),
	option(mappedTargetConcepts(TN0), MStats),
	option(sourcePercentage(SPerc), MStats),
	option(targetPercentage(TPerc), MStats),
	option(sourcePercentageInput(SiPerc), MStats, 0),
	option(targetPercentageInput(TiPerc), MStats, 0),

	option(source_depth(DepthS), MStats),
	option(target_depth(DepthT), MStats),
	option(mean(MeanDepthS), DepthS, 0),
	option(mean(MeanDepthT), DepthT, 0),
	option(max(MaxDepthS), DepthS, 0),
	option(max(MaxDepthT), DepthT, 0),
	option(standard_deviation(DepthStdS), DepthS, 0),
	option(standard_deviation(DepthStdT), DepthT, 0),

	option(source_child_stats(ChildS), MStats),
	option(target_child_stats(ChildT), MStats),
	option(nrOfTopConcepts(STop), ChildS, 0),
	option(nrOfTopConcepts(TTop), ChildT, 0),
	option(mean(MeanChildS), ChildS, 0),
	option(mean(MeanChildT), ChildT, 0),
	option(max(MaxChildS), ChildS, 0),
	option(max(MaxChildT), ChildT, 0),
	option(standard_deviation(ChildStdS), ChildS, 0),
	option(standard_deviation(ChildStdT), ChildT, 0),

	save_perc(STop, SN0, STopP),
	save_perc(TTop, TN0, TTopP),

	format(atom(SN), '~d (i ~2f%, v ~2f%)', [SN0, SiPerc, SPerc]),
	format(atom(TN), '~d (i ~2f%, v ~2f%)', [TN0, TiPerc, TPerc]),
	format(atom(TopSatom), '~d (~2f%)', [STop, STopP]),
	format(atom(TopTatom), '~d (~2f%)', [TTop, TTopP]),
	format(atom(DepthSatom), '~2f (\u03C3 = ~2f)', [MeanDepthS, DepthStdS]),
	format(atom(DepthTatom), '~2f (\u03C3 = ~2f)', [MeanDepthT, DepthStdT]),
	format(atom(ChildSatom), '~2f (\u03C3 = ~2f)', [MeanChildS, ChildStdS]),
	format(atom(ChildTatom), '~2f (\u03C3 = ~2f)', [MeanChildT, ChildStdT]),


	(   rdf(URL, amalgame:default_relation, _R),
	    reference_counts(URL, Strategy, ReferenceStats)
	->  true
	;   ReferenceStats = []
	),
	(   option(inputPercentage(IP), MStats)
	->  format(atom(TmA), '~d (~5f%)', [MN, IP]),
	    IpStats = [ 'total matches'-span([TmA])]
	;   IpStats = [ 'total matches'-MN ]
	),
	append([IpStats, BasicStats, ReferenceStats], Stats).

amalgame_info(Scheme, _Strategy, Stats) :-
	is_vocabulary(Scheme),
	!,
	Stats =
	['Total concepts'-Total,
	 '# top concepts:'  - span([TopConA]),
	 'average depth:'   - span([Depth]),
	 'maximum depth:'   - span([DepthMax]),
	 'average # children:'   - span([Branch]),
	 'maximum # children:'   - span([BranchMax]),
	 '# prefLabels' -span([PrefCount, ' (',
			       \(ag_util_components:html_showlist(PrefLangs)), ')']),
	 '# altLabels' - span([AltCount,' (',
			       \(ag_util_components:html_showlist(AltLangs)), ')']),
	 '# ambiguous concepts (pref):'-span([PrefHomsCA]),
	 '# ambiguous pref labels:'-span([PrefHomsLA]),
	 '# ambiguous concepts (alt):'-span([AltHomsCA]),
	 '# ambiguous alt  labels:'-span([AltHomsLA])
	],
	voc_property(Scheme, numberOfConcepts(Total)),
	voc_property(Scheme, numberOfPrefLabels(PrefCount)),
	voc_property(Scheme, numberOfAltLabels(AltCount)),
	voc_property(Scheme, languages(skos:prefLabel, PrefLangs)),
	voc_property(Scheme, languages(skos:altLabel, AltLangs)),
	voc_property(Scheme, numberOfHomonyms(skos:prefLabel, PrefHomsL, PrefHomsC)),
	voc_property(Scheme, numberOfHomonyms(skos:altLabel,  AltHomsL,	 AltHomsC )),

	voc_property(Scheme, depth(DepthStats)),
	option(mean(DepthM), DepthStats, 0),
	option(standard_deviation(DepthStd), DepthStats, 0),
	option(max(DepthMax), DepthStats, 0),

	voc_property(Scheme, branch(BranchStats)),
	option(mean(BranchM), BranchStats, 0),
	option(standard_deviation(BranchStd), BranchStats, 0),
	option(max(BranchMax), BranchStats, 0),
	option(nrOfTopConcepts(TopConcepts), BranchStats, 0),

	save_perc(PrefHomsL, PrefCount, PrefHomsLP),
	save_perc(AltHomsL,  AltCount,  AltHomsLP),
	save_perc(PrefHomsC, Total, PrefHomsCP),
	save_perc(AltHomsL, Total, AltHomsCP),
	save_perc(TopConcepts, Total, TopConceptsP),

	format(atom(Depth), '~2f (\u03C3 = ~2f)', [DepthM, DepthStd]),
	format(atom(Branch), '~2f (\u03C3 = ~2f)', [BranchM, BranchStd]),
	format(atom(PrefHomsLA), '~d (~2f%)', [PrefHomsL, PrefHomsLP]),
	format(atom(AltHomsLA),  '~d (~2f%)', [AltHomsL, AltHomsLP]),
	format(atom(PrefHomsCA), '~d (~2f%)', [PrefHomsC, PrefHomsCP]),
	format(atom(AltHomsCA),  '~d (~2f%)', [AltHomsC, AltHomsCP]),
	format(atom(TopConA),    '~d (~2f%)', [TopConcepts, TopConceptsP]).



amalgame_info(URL, Strategy,
	       ['type'   - \(cp_label:rdf_link(Type)) | Optional ]) :-
	rdfs_individual_of(URL, amalgame:'Process'),
	rdf(URL, rdf:type, Type, Strategy),
	(   rdf_has(URL, amalgame:input, InputMapping)
	->  Input = [input - \(cp_label:rdf_link(InputMapping))]
	;   Input = []
	),
	(   rdf_has(Type, skos:definition, literal(DefLit))
	->  Definition = [about - DefLit]
	;   Definition = []
	),
	append([Definition, Input],Optional).

amalgame_info(_URL, _Strategy, []).


%%	amalgame_provenance(+R, +Alignment, -Provenance:[key-value])
%
%	Provenance is a list of key-value pairs with provenance about
%	node R as defined by strategy Alignment

amalgame_provenance(R, Alignment, Provenance) :-
	findall(Key-Value, ag_prov(R, Alignment, Key, Value), Provenance0),
	sort(Provenance0,Provenance).

ag_prov(R, A, 'defined by', \rdf_link(Agent)) :-
	(   rdf_has(R, dc:creator, Agent, RealProp),
	    rdf(R, RealProp, Agent, A)
	*->  true
	;   rdf_has(R, dc:creator, Agent)
	).

ag_prov(R, _A, 'latest run controlled by', \rdf_link(Agent)) :-
	rdf_has(R,  prov:wasControlledBy, Agent),
	\+ rdfs_individual_of(Agent, prov:'SoftwareAgent').

ag_prov(R, _A, 'generated by', \rdf_link(Agent)) :-
	rdf(R, prov:wasGeneratedBy, Process),
	rdf_has(Process, prov:wasControlledBy, Agent),
	\+ rdfs_individual_of(Agent, prov:'SoftwareAgent').

ag_prov(R, _A, 'generated at', \rdf_link(Time)) :-
	rdf(R, prov:wasGeneratedBy, Process),
	rdf_has(Process, prov:endedAtTime,Time).
	% literal_text(Time, TS).


ag_prov(R, A, 'defined at', \rdf_link(V)) :-
	(   rdf_has(R, dc:date, V, RealProp),
	    rdf(R, RealProp, V, A)
	->  true
	;   rdf_has(R, dc:date, V)
	).
ag_prov(R, A, owl:'version', V) :-
	(   rdf_has(R, owl:versionInfo, literal(V), RealProp),
	    rdf(R, RealProp, literal(V), A)
	->  true
	;   rdf(R, owl:versionInfo, literal(V))
	).
ag_prov(Graph, Graph, contributors, Vs) :-
	rdfs_individual_of(Graph, amalgame:'AlignmentStrategy'),
	findall(\rdf_link(V),
		(   rdf_has(R, dc:creator, V, RP),
		    rdf(R, RP, V, Graph),
		    \+ R == Graph,
		    \+ rdf(Graph, dc:creator, V)
		), Vs0),
	Vs0 \== [],
	!,
	sort(Vs0, Vs).

%%	amalgame_parameters(+URI, -Parmas)
%
%	Params is a list of parameters for URI.

amalgame_parameters(Process, Strategy, Params) :-
	rdfs_individual_of(Process, amalgame:'Process'),
	!,
	rdf(Process, rdf:type, Type, Strategy),
	amalgame_module_id(Type, Module),
	amalgame_module_parameters(Module, DefaultParams),
	process_options(Process, Module, CurrentValues),
	override_options(DefaultParams, CurrentValues, Params).
amalgame_parameters(_, _Strategy, []).

override_options([], _, []).
override_options([H|T], Current, [V|Results]) :-
	override_options(T, Current, Results),
	H=parameter(Id, Type, Default, Desc),
	V=parameter(Id, Type, Value,   Desc),
	Opt =.. [Id, Value],
	option(Opt, Current, Default).
