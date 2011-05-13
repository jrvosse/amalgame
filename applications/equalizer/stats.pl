:- module(eq_stats,
	  [ flush_stats_cache/0
	  ]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(amalgame/amalgame_modules)).
:- use_module(library(amalgame/expand_graph)).
:- use_module(library(amalgame/vocabulary)).
:- use_module(cliopatria(components/label)).

:- use_module(controls).

% http handlers for this applications

:- http_handler(amalgame(private/nodeinfo), http_eq_nodeinfo, []).
:- http_handler(amalgame(private/info), http_eq_info, []).

:- dynamic
	stats_cache/2.

flush_stats_cache :-
	retractall(stats_cache(_,_)),
	flush_expand_cache.

%%	http_eq_nodeinfo(+Request)
%
%	Emit HTML snippet with statistics for an amalgame URI

http_eq_nodeinfo(Request) :-
	http_parameters(Request,
			[ url(URL,
			      [description('URL of a node (mapping,vocab,process)')])
		       ]),
	html_current_option(content_type(Type)),
	format('Content-type: ~w~n~n', [Type]),
	(   rdfs_individual_of(URL, amalgame:'Mapping')
	->  with_mutex(URL, mapping_counts(URL, _MN, _SN, _TN, SPerc, TPerc)),
	    format('s:~w\% t:~w\%',
		   [SPerc,TPerc])
	;   true
	).

%%	http_eq_info(+Request)
%
%	Emit HTML snippet with information about an amalgame URI

http_eq_info(Request) :-
	http_parameters(Request,
			[ url(URL,
			      [description('URL of a node (mapping,vocab,process)')])
		       ]),
	amalgame_provenance(URL, Prov),
	amalgame_info(URL, Stats),
	amalgame_parameters(URL, Params),
	phrase(html([\html_prop_table(Prov),
		     \html_prop_table(Stats),
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
	 html(tr([td(Key),
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
	html(table([input([type(hidden), name(process), value(URI)]),
		    input([type(hidden), name(update), value(true)]),
		    \html_parameter_form(Params)
		   ])).


%%	mapping_counts(+MappingURI, -MappingN, -SourceN, -TargetN)
%
%	Counts for the mappings in MappingURI.
%
%       @param MappingN is the number of total correspondences
%       @param SourceN is the number of source concepts mapped
%       @param TargetN is the number of target concepts mapped

mapping_counts(URL, MN, SN, TN, SPerc, TPerc) :-
	stats_cache(URL, stats(MN, SN, TN, SPerc, TPerc)),
	!.
mapping_counts(URL, MN, SN, TN, SPerc, TPerc) :-
	expand_mapping(URL, Mapping),
	mapping_sources(URL, InputS, InputT),
	concept_count(InputS, SourceN),
	concept_count(InputT, TargetN),

	maplist(align_source, Mapping, Ss0),
	maplist(align_target, Mapping, Ts0),
	sort(Ss0, Ss),
	sort(Ts0, Ts),
	length(Mapping, MN),
	length(Ss, SN),
	length(Ts, TN),

	rounded_perc(SourceN, SN, SPerc),
	rounded_perc(TargetN, TN, TPerc),
	retractall(stats_cache(_,_)),
	assert(stats_cache(URL, stats(MN, SN, TN, SPerc, TPerc))).

rounded_perc(0, _, 0.0) :- !.
rounded_perc(_, 0, 0.0) :- !.
rounded_perc(Total, V, Perc) :-
	Perc0 is V/Total,
	dyn_perc_round(Perc0, Perc, 100).

dyn_perc_round(P0, P, N) :-
	P1 is round(P0*N),
	(   P1 == 0
	->  N1 is N*10,
	    dyn_perc_round(P0, P, N1)
	;   P is P1/(N/100)
	).

%%	concept_count(+Vocab, -Count)
%
%	Count is the number of concepts in Vocab

concept_count(Vocab, Count) :-
	stats_cache(Vocab, stats(Count)),
	!.
concept_count(Vocab, Count) :-
	expand_vocab(Vocab, Scheme),
	findall(C, vocab_member(C, Scheme), Cs),
	length(Cs, Count),
	retractall(stats_cache(_,_)),
	assert(stats_cache(Vocab, stats(Count))).


%%	mapping_sources(+MappingURI, -Source, -Target)
%
%	Source and Target are the recursive source and target
%	vocabularies of Mapping.

mapping_sources(URL, S, T) :-
	rdf_has(URL, opmv:wasGeneratedBy, Process),
	(   rdf(Process, amalgame:source, S0),
	    rdf(Process, amalgame:target, T0)
	->  vocab_source(S0, S),
	    vocab_source(T0, T)
	;   rdf(Process, amalgame:input, Input)
	->  mapping_sources(Input, S, T)
	).

vocab_source(V, S) :-
	rdf_has(V, opmv:wasGeneratedBy, Process),
	rdf_has(Process, amalgame:input, Input),
	!,
	vocab_source(Input, S).
vocab_source(V, V).


%%	amalgame_info(+Mapping, -Info)
%
%	Stats of a resourcemapping

amalgame_info(URL, Stats) :-
	rdfs_individual_of(URL, amalgame:'Mapping'),
	!,
	Stats = ['total mappings'-MN,
		 'mapped source concepts'-SN,
		 'mapped target concepts'-TN
		],
	with_mutex(URL, mapping_counts(URL, MN, SN0, TN0, SPerc, TPerc)),
	concat_atom([SN0, ' (',SPerc,'%)'], SN),
	concat_atom([TN0, ' (',TPerc,'%)'], TN).
amalgame_info(Scheme,
	    ['Total concepts'-Total
	    ]) :-
	rdfs_individual_of(Scheme, skos:'ConceptScheme'),
	!,
	concept_count(Scheme, Total).
amalgame_info(URL,
	       ['type'   - \(cp_label:rdf_link(Type)),
		'about'   - Definition
	       ]) :-
	rdfs_individual_of(URL, amalgame:'Process'),
	rdf(URL, rdf:type, Type),
	(   rdf_has(Type, skos:definition, literal(Definition))
	->  true
	;   Definition = '-'
	).
amalgame_info(_URL, []).


%%	amalgame_provenance(+R, -Provenance:[key-value])
%
%	Provenance is a list of key-value pairs with provenance about R.

amalgame_provenance(R, Provenance) :-
	findall(Key-Value, ag_prov(R, Key, Value), Provenance).

ag_prov(R, 'created by', V) :-
	rdf(R, dc:creator, V).
ag_prov(R, 'created at', V) :-
	rdf(R, dc:date, V).
ag_prov(Graph, contributors, Vs) :-
	rdfs_individual_of(Graph, amalgame:'Alignment'),
	findall(V,
		(   rdf(R, _, _, Graph),
		    \+ R == Graph,
		    rdf(R, dc:creator, V),
		    \+ rdf(Graph, dc:creator, V)
		), Vs0),
	Vs0 \== [],
	!,
	sort(Vs0, Vs).

/*ag_prov(Mapping, Key, Value) :-
	rdfs_individual_of(Mapping, amalgame:'Mapping'),
	!,
	rdf_has(Mapping, opmv:wasGeneratedBy, Process),
	ag_prov(Process, Key, Value).
*/


%%	amalgame_parameters(+URI, -Parmas)
%
%	Params is a list of parameters for URI.

amalgame_parameters(Process, Params) :-
	rdfs_individual_of(Process, amalgame:'Process'),
	!,
	rdf(Process, rdf:type, Type),
	amalgame_module_id(Type, Module),
	amalgame_module_parameters(Module, DefaultParams),
	process_options(Process, Module, CurrentValues),
	override_options(DefaultParams, CurrentValues, Params).
amalgame_parameters(_, []).

override_options([], _, []).
override_options([H|T], Current, [V|Results]) :-
	override_options(T, Current, Results),
	H=parameter(Id, Type, Default, Desc),
	V=parameter(Id, Type, Value,   Desc),
	Opt =.. [Id, Value],
	option(Opt, Current, Default).



align_source(align(S,_,_), S).
align_target(align(_,T,_), T).


