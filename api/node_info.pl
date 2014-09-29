:- module(ag_api_node_info,
	  [
	  ]).

:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(settings)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(library(skos/util)).

:- use_module(library(amalgame/amalgame_modules)).
:- use_module(library(amalgame/ag_strategy)).
:- use_module(library(amalgame/ag_stats)).
:- use_module(library(amalgame/util)).
:- use_module(components(label)). % we need rdf_link//1 from this module

:- use_module(components(amalgame/util)).

% http handlers for this applications

:- http_handler(amalgame(api/node_info), http_node_info, []).

:- setting(amalgame:vocabulary_statistics, oneof([all,fast]), fast,
	   'Compute all (takes long) or only the cheap (fast) vocabulary statistics').

:- rdf_meta
	label_stats(r, r, r, -).

%%	http_node_info(+Request)
%
%	Emit HTML snippet with information about an amalgame URI

http_node_info(Request) :-
	http_parameters(Request,
			[ url(URL,
			      [description('URL of a node (mapping,vocab,process,strategy)')]),
			  strategy(Strategy,
				    [description('URL of the alignment strategy')])
		       ]),
	amalgame_info(URL, Strategy, Stats),
	amalgame_provenance(URL, Strategy, Prov),
	amalgame_parameters(URL, Strategy, Params),
	phrase(html([\html_prop_table(prov, Prov),
		     \html_prop_table(stats, Stats),
		     \html_form(Params, URL)
		    ]),
	       HTML),
	html_current_option(content_type(Type)),
	format('Content-type: ~w~n~n', [Type]),
	print_html(HTML).

%%	html_prop_table(Class, +Pairs)
%
%	Emit an HTML table with key-value pairs.

html_prop_table(Class, Pairs) -->
	html(table([class(Class)],
		   tbody(\html_rows('', Pairs)))
	    ).

html_rows(_,[]) --> !.
html_rows(Prefix,[[]|Tail]) --> !,
	html_rows(Prefix,Tail).
html_rows(Prefix,[_Key-[]|Tail]) -->
	html_rows(Prefix, Tail).
html_rows(Prefix, [Key-Value|Tail]) -->
	html_row(Prefix, Key, Value),
	html_rows(Prefix, Tail).

html_row(Prefix, Key, set(Values)) -->
	{ atomic_concat('... ', Prefix, NewPrefix) },
	html(tr([th([Prefix,Key]),
		 td([])
		])),
	html_rows(NewPrefix, Values).
html_row(Prefix, Key, Value) -->
	 html(tr([th([Prefix, Key]),
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
	node_stats(Strategy, URL, MStats, []),
	option(totalCount(MN), MStats),
	(   option(inputPercentage(IP), MStats)
	->  format(atom(TmA), '~d (~5f%)', [MN, IP]),
	    IpStats = [ 'total matches'-span([TmA])]
	;   IpStats = [ 'total matches'-MN ]
	),

	option(mappedSourceConcepts(SN0), MStats),
	option(mappedTargetConcepts(TN0), MStats),
	option(sourcePercentage(SPerc), MStats, 0),
	option(targetPercentage(TPerc), MStats, 0),
	option(sourcePercentageInput(SiPerc), MStats, 0),
	option(targetPercentageInput(TiPerc), MStats, 0),
	format(atom(SN), '~d (i ~2f%, v ~2f%)', [SN0, SiPerc, SPerc]),
	format(atom(TN), '~d (i ~2f%, v ~2f%)', [TN0, TiPerc, TPerc]),
	BasicStats = [
	    'matched source concepts'-SN,
	    'matched target concepts'-TN
	],
	(   option(source_depth(DepthS), MStats)
	->  format_5numsum('Depth of source concepts', DepthS, DepthSStats)
	;   DepthSStats = []
	),
	(   option(target_depth(DepthT), MStats)
	->  format_5numsum('Depth of target concepts', DepthT, DepthTStats)
	;   DepthTStats = []
	),

	(   option(source_child_stats(ChildS), MStats)
	->  format_5numsum('# of children (of source concepts)', ChildS, ChildSStats)
	;   ChildSStats = []
	),
	(   option(target_child_stats(ChildT), MStats)
	->  format_5numsum('# of children (of target concepts)', ChildT, ChildTStats0),
	    option(nrOfTopConcepts(TTop), ChildT, 0),
	    save_perc(TTop, TN0, TTopP),
	    format(atom(TopTatom), '~d (~2f%)', [TTop, TTopP]),
	    ChildTStats = ['# top concepts'-TopTatom, ChildTStats0 ]
	;   ChildTStats = []
	),

	(   rdf(URL, amalgame:default_relation, _R),
	    reference_counts(URL, Strategy, D),
	    ReferenceStats = [ 'matching with ref. relations'    - D.matching,
			       'conflicting with  ref. relations'- D.conflicting,
			       'not yet in reference'		 - D.notInRef,
			       'in ref. but missing here'	 - D.missing
			     ]
	->  true
	;   ReferenceStats = []
	),
	append([IpStats, BasicStats, ReferenceStats,
		[DepthSStats], [ChildSStats],
		[DepthTStats], ChildTStats
	       ], Stats).

amalgame_info(Scheme, Strategy, Stats) :-
	skos_is_vocabulary(Scheme),
	!,
	node_stats(Strategy, Scheme, NStats, []),
	option(structure(DDict), NStats, _{}),
	option(properties(PDict), NStats, _{}),

	BasicStats = [
	    'type:'	       - Virtual,
	    '# concepts:'      - Total
	],
	option(totalCount(Total), NStats, 0),
	option(virtual(V), NStats),

	(   option(formats(Formats), NStats),
	    option(totalLabelCount(TotalLabelCount), NStats, 0),
	    option(uniqueLabelCount(UniqueLabelCount), NStats, 0)
	->  ExtraStats = [
		'formats:'   - Formats,
		'# labels:'        - TotalLabelCount,
		'# unique labels:' - UniqueLabelCount,
		'# counted top concepts:'  - NrTopConcepts | DTops
	    ],
	    option(topConceptCount(NrTopConcepts), DDict, [])
	;   ExtraStats = []
	),
	(   V == true
	->  Virtual = virtual,
	    DTops = []
	;   Virtual = materialized,
	    findall(Top, skos_top_concept(Scheme, Top), Tops),
	    length(Tops, NrDeclaredTops),
	    DTops = ['# declared top concepts:'  - NrDeclaredTops]
	),
	label_property_stats(PDict, PStats),
	depth_stats(DDict, DStats),
	append([BasicStats, ExtraStats, DStats, PStats], Stats).

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

amalgame_info(Strategy, Strategy, Results) :-
	findall(vocabulary- \(cp_label:rdf_link(V)),
		strategy_vocabulary(Strategy, V), Vocs),
	rdf_equal(amalgame:final, FinalStatus),
	findall(F, strategy_entity_status(Strategy, F, FinalStatus), Finals),
	length(Finals, NrFinals),
	append([ Vocs,
		 [  '# finalized mappings'-NrFinals ]
	       ], Results).


label_property_stats(Dict, Stats) :-
	findall(\rdf_link(Property)-set(Pairs),
		( get_dict(Property, Dict, LCounts),
		  dict_pairs(LCounts, _, Pairs)
		),
		Stats).

depth_stats(Dict, Stats) :-
	findall(Set,
		(    get_dict(Property, Dict, S),
		     is_dict(S, stats),
		     format_5numsum(Property, S, Set)
		),
		Stats).



%%	amalgame_provenance(+R, +Strategy, -Provenance:[key-value])
%
%	Provenance is a list of key-value pairs with provenance about
%	node R as defined by Strategy.


amalgame_provenance(R, Strategy, Provenance) :-
	findall(Key-Value, ag_prov(R, Strategy, Key, Value), Provenance0),
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

format_5numsum(Key, Stats, Formatted) :-
	option(median(Median), Stats, 0),
	option(max(Max), Stats, 0),
	option(min(Min), Stats, 0),
	option(q1(Q1), Stats, 0),
	option(q3(Q3), Stats, 0),

	Formatted = Key-set([
			    'minimum:'	- span([Min]),
			    'first quartile:'	- span('~1f'-[Q1]),
			    'median:'		- span([Median]),
			    'third quartile:'	- span('~1f'-[Q3]),
			    'maximum:'	- span([Max])
			]).
