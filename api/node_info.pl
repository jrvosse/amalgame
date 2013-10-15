:- module(ag_api_node_info,
	  [
	  ]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/html_write)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(amalgame/amalgame_modules)).
:- use_module(library(amalgame/ag_stats)).
:- use_module(library(amalgame/voc_stats)).
:- use_module(library(amalgame/caching)).
:- use_module(library(amalgame/util)).
:- use_module(components(label)). % we need rdf_link//1 from this module

:- use_module(library(amalgame/ag_controls)).

% http handlers for this applications

:- http_handler(amalgame(api/node_info), http_node_info, []).
:- http_handler(amalgame(analyse/deep_voc_stats), http_deep_voc_stats, []).

:- setting(amalgame:vocabulary_statistics, oneof([all,fast]), fast,
	   'Compute all (takes long) or only the cheap (fast) vocabulary statistics').

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

http_deep_voc_stats(Request) :-
	http_parameters(Request,
			[ url(Voc,
			      [description('URL of a vocabulary or concept scheme')]),
			  strategy(Strategy,
				   [description('URL of the alignment strategy')])
		       ]),
	flush_dependent_caches(Voc, Strategy),
	voc_property(Voc, depth(D), [compute(yes)]),
	voc_property(Voc, branch(B), [compute(yes)]),
	reply_json(json([url=Voc, depth=json(D), branch=json(B)])).

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
	node_stats(Strategy, URL, MStats),
	option(totalCount(MN), MStats),
	(   option(inputPercentage(IP), MStats)
	->  format(atom(TmA), '~d (~5f%)', [MN, IP]),
	    IpStats = [ 'total matches'-span([TmA])]
	;   IpStats = [ 'total matches'-MN ]
	),

	option(mappedSourceConcepts(SN0), MStats),
	option(mappedTargetConcepts(TN0), MStats),
	option(sourcePercentage(SPerc), MStats),
	option(targetPercentage(TPerc), MStats),
	option(sourcePercentageInput(SiPerc), MStats, 0),
	option(targetPercentageInput(TiPerc), MStats, 0),
	format(atom(SN), '~d (i ~2f%, v ~2f%)', [SN0, SiPerc, SPerc]),
	format(atom(TN), '~d (i ~2f%, v ~2f%)', [TN0, TiPerc, TPerc]),
	BasicStats = [
	    'matched source concepts'-SN,
	    'matched target concepts'-TN
	],

	option(source_depth(DepthS), MStats),
	(   DepthS \= []
	->  option(mean(MeanDepthS), DepthS, 0),
	    option(max(MaxDepthS), DepthS, 0),
	    option(standard_deviation(DepthStdS), DepthS, 0),
	    format(atom(DepthSatom), '~2f (\u03C3 = ~2f)', [MeanDepthS, DepthStdS]),
	    DepthSStats = ['avg. source depth' - DepthSatom,
			   'max. source depth' - MaxDepthS]
	;   DepthSStats = []
	),

	option(target_depth(DepthT), MStats),
	(   DepthT \= []
	->  option(mean(MeanDepthT), DepthT, 0),
	    option(max(MaxDepthT), DepthT, 0),
	    option(standard_deviation(DepthStdT), DepthT, 0),
	    format(atom(DepthTatom), '~2f (\u03C3 = ~2f)', [MeanDepthT, DepthStdT]),
	    DepthTStats = [ 'avg. target depth' - DepthTatom,
			    'max. target depth' - MaxDepthT]
	;   DepthTStats = []

	),

	option(source_child_stats(ChildS), MStats),
	(   ChildS \= []
	->  option(nrOfTopConcepts(STop), ChildS, 0),
	    option(mean(MeanChildS), ChildS, 0),
	    option(max(MaxChildS), ChildS, 0),
	    option(standard_deviation(ChildStdS), ChildS, 0),
	    save_perc(STop, SN0, STopP),
	    format(atom(TopSatom), '~d (~2f%)', [STop, STopP]),
	    format(atom(ChildSatom), '~2f (\u03C3 = ~2f)', [MeanChildS, ChildStdS]),
	    ChildSStats = ['avg. # children source' - ChildSatom,
			   'max. # children source' - MaxChildS,
			   'matched source top concepts'-TopSatom
			  ]
	;   ChildSStats = []
	),
	option(target_child_stats(ChildT), MStats),
	(   ChildT \= []
	->  option(nrOfTopConcepts(TTop), ChildT, 0),
	    option(mean(MeanChildT), ChildT, 0),
	    option(max(MaxChildT), ChildT, 0),
	    option(standard_deviation(ChildStdT), ChildT, 0),
	    save_perc(TTop, TN0, TTopP),
	    format(atom(TopTatom), '~d (~2f%)', [TTop, TTopP]),
	    format(atom(ChildTatom), '~2f (\u03C3 = ~2f)', [MeanChildT, ChildStdT]),
	    ChildTStats = ['avg. # children target' - ChildTatom,
			   'matched target top concepts'-TopTatom,
			   'max. # children target' - MaxChildT
			  ]
	;   ChildTStats = []
	),

	(   rdf(URL, amalgame:default_relation, _R),
	    reference_counts(URL, Strategy, ReferenceStats)
	->  true
	;   ReferenceStats = []
	),
	append([IpStats, BasicStats, ReferenceStats,
		DepthSStats, ChildSStats,
		DepthTStats, ChildTStats
	       ], Stats).

amalgame_info(Scheme, _Strategy, Stats) :-
	is_vocabulary(Scheme),
	!,
	BasicStats = [
	    'Version:' - Version,
	    'Total concepts: '-Total
	],
	voc_property(Scheme, numberOfConcepts(Total)),
	voc_property(Scheme, version(Version)),

	label_stats(Scheme, skos:prefLabel, PrefLabelStats),
	label_stats(Scheme, skos:altLabel,  AltLabelStats),



	(   setting(amalgame:vocabulary_statistics, fast) ->  C = no; C = yes),
	(   voc_property(Scheme, depth(DepthStats0), [compute(C)])
	->  option(mean(DepthM), DepthStats0, 0),
	    option(standard_deviation(DepthStd), DepthStats0, 0),
	    option(max(DepthMax), DepthStats0, 0),
	    format(atom(Depth), '~2f (\u03C3 = ~2f)', [DepthM, DepthStd]),
	    DepthStats = [ 'average depth:'	- span([Depth]),
			   'maximum depth:'	- span([DepthMax])
			 ]
	;   DepthStats = [ a([href('#'), class(compute_deep_stats)],
			      ['compute additional statistics'])
			   -
			   a([href('#'), class(compute_deep_stats)], ['?'])
			     ]
	),

	(   voc_property(Scheme, branch(BranchStats0), [compute(C)])
	->  option(mean(BranchM), BranchStats0, 0),
	    option(standard_deviation(BranchStd), BranchStats0, 0),
	    option(max(BranchMax), BranchStats0, 0),
	    option(nrOfTopConcepts(TopConcepts), BranchStats0, 0),
	    save_perc(TopConcepts, Total, TopConceptsP),
	    format(atom(TopConA),    '~d (~2f%)', [TopConcepts, TopConceptsP]),
	    format(atom(Branch), '~2f (\u03C3 = ~2f)', [BranchM, BranchStd]),
	    BranchStats = [
		'# top concepts:'  - span([TopConA]),
		'average # children:'   - span([Branch]),
		'maximum # children:'   - span([BranchMax])
	    ]
	;   BranchStats = []
	),
	append([DepthStats, BranchStats],       StructureStats),
	append([PrefLabelStats, AltLabelStats], LabelStats),
	append([BasicStats, StructureStats, LabelStats], Stats).


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


label_stats(Scheme, Property, Stats) :-
	voc_property(Scheme, languages(Property, Langs0)),
	voc_property(Scheme, numberOfConcepts(Total)),
	(   Langs0 == []
	->  Langs = [_UnknownLang]
	;   Langs = Langs0
	),
	findall([CountL-span([A]),
		 '... # labeled concepts'    - span([CCountA]),
		 '... # labels/labeled concept' - span([LPA]),
		 '... # ambiguous labels:'   - span([HomsLA]),
		 '... # ambiguously labeled concepts:' - span([HomsCA])
		],
		(   member(Lang, Langs),
		    voc_property(Scheme, numberOfLabels(Property, Lang, LCount, CCount)),
		    CCount > 0,
		    % voc_property(Scheme, numberOfUniqueLabels(Property, Lang, UniqL, UniqC)),
		    voc_property(Scheme, numberOfHomonyms(Property, Lang, HomsL, HomsC)),
		    save_perc(HomsL, LCount, HomsLP),
		    save_perc(HomsC, CCount, HomsCP),
		    save_perc(CCount, Total, CCountP),
		    LP is LCount/CCount,

		    (	CCount == Total
		    ->  format(atom(CCountA), '100%', [])
		    ;	format(atom(CCountA), '~d (~2f%)', [CCount, CCountP])
		    ),
		    format(atom(CountL), '# ~p @~w', [Property, Lang]),
		    format(atom(A), '~d', [LCount]),
		    format(atom(LPA), '~2f', [LP]),
		    format(atom(HomsLA), '~d (~2f%)', [HomsL, HomsLP]),
		    format(atom(HomsCA), '~d (~2f%)', [HomsC, HomsCP])
		), PrefLabelStatsLoL),
	append(PrefLabelStatsLoL, Stats).


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

%%	amalgame_parameters(+URI, +Strategy, -Parmas)
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
