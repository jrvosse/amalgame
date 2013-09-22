:- module(strategy_graph_viz,
	  [ html_strategy_viz//1,
	    reply_alignment_graph/2
	  ]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).
:- use_module(components(label)).
:- use_module(components(graphviz)).
:- use_module(library(amalgame/caching)).
:- use_module(library(amalgame/voc_stats)).
:- use_module(library(amalgame/ag_stats)).
:- use_module(library(amalgame/ag_evaluation)).
:- use_module(library(amalgame/map)).

:- http_handler(amalgame(strategy_viz), http_strategy_viz, []).

:- setting(amalgame:secondary_input, oneof([show,hide]), show,
	   'Show or hide arrows for amalgame:secondary_input').

strategy_viz_options(Alignment,
	       [edge_links(false),
		shape_hook(amalgame_shape),
		label_hook(amalgame_label(Alignment)),
		graph_attributes([rankdir('BT')])
	       ]).

is_meta(shape_hook).
is_meta(label_hook).


%%	http_graphviz(+Request)
%
%	Emit html page with a visualization of a graph.

http_strategy_viz(Request) :-
	http_parameters(Request,
			[ format(Format,
				 [default(html),
				  oneof([xdot,svg,html]),
				  description('Return svg graph or html page with embedded object')
				 ]),
			  alignment(Alignment,
				 [description('URI from which we request the context')]),
			  selected(Selected,
			      [uri,
			       optional(true),
			       description('URI of a node that should be expanded first')
			      ])
			]),
	% When a node has been selected, make sure we know the stats
	(   ground(Selected) -> node_stats(Alignment, Selected, _); true),

	(   Format \== html
	->  reply_alignment_graph(Alignment, Format)
	;   strategy_viz_options(Alignment, Options),
	    reply_html_page(cliopatria(default),
			    [ title(['Graph for ', \turtle_label(Alignment)])
			    ],
			    [ \graphviz_graph(amalgame_triples(Alignment), Options)
			    ])
	).

%%	reply_alignment_graph(+AlignmentURI)
%
%	Emit an alignment graph.

reply_alignment_graph(Alignment, Format) :-
	strategy_viz_options(Alignment, Options),
	amalgame_triples(Alignment, Triples),
	meta_options(is_meta, Options, QOptions),
	reply_graphviz_graph(Triples, Format, QOptions).


%%	html_strategy_viz(+Graph)
%
%	Emit html component with a visualization of Graph.

html_strategy_viz(Graph) -->
	{ strategy_viz_options(Graph, Options)
	},
	graphviz_graph(amalgame_triples(Graph), Options).

html_strategy_viz(Graph) -->
	{ http_link_to_id(http_strategy_viz, [graph(Graph),format(svg)], HREF)
	},
	html([ object([ id(strategy_viz),
			data(HREF),
			type('image/svg+xml')
		      ],
		      [])
	     ]).


%%	amalgame_triples(Graph, Triples)
%
%	Triples are all rdf(S,P,O) in Graph.

amalgame_triples(Graph, Triples) :-
	findall(rdf(S,P,O), amalgame_graph_triple(Graph,S,P,O), Triples).

% hack, to get a better layout we reverse the arrow :(
amalgame_graph_triple(Graph,Graph,P,Scheme) :-
	rdf_equal(amalgame:includedIn, P),
	rdf(Graph,amalgame:includes,Scheme,Graph).
amalgame_graph_triple(Graph,O,P,S) :-
	rdf(S,P,O,Graph),
	is_amalgame_property(P),
	\+ empty_result(Graph, S).


is_amalgame_property(P) :-
	rdfs_subproperty_of(P, prov:used),
	(   setting(amalgame:secondary_input, hide)
	->  \+ rdf_equal(amalgame:secondary_input, P)
	;   true
	),
	!.

is_amalgame_property(P) :-
	rdfs_subproperty_of(P, amalgame:wasGeneratedBy),
	!.
is_amalgame_property(P) :-
	rdfs_subproperty_of(P, amalgame:wasDerivedFrom),
	!.
is_amalgame_property(P) :-
	rdfs_subproperty_of(P, amalgame:wasTriggeredBy),
	!.
% filter out empty results ...

empty_result(Strategy, M) :-
	rdfs_individual_of(M, amalgame:'Mapping'),
	stats_cache(M-Strategy, mstats(Stats)),
	option(totalCount(0), Stats),!.

empty_result(_Strategy,M) :-
	is_empty_eval_graph(M).

% and processes resulting in empty evals
empty_result(Strategy,Process) :-
	rdfs_individual_of(Process, amalgame:'EvaluationProcess'),
	rdf(Empty, amalgame:wasGeneratedBy, Process, Strategy),
	empty_result(Strategy, Empty).


%%	amalgame_shape(+Resource, -Shape)
%
%	Defines graph node shape for different types of resources.

amalgame_shape(R, [shape(box),
	      style(filled),
	      fillcolor(Color),
	      fontsize(10)]) :-
	atom(R),
	rdfs_individual_of(R, amalgame:'Process'),
	!,
	process_color(R, Color).
amalgame_shape(R, [shape(ellipse),
	      style(filled),
	      fillcolor('#EEEEEE'),
	      fontsize(10)]) :-
	atom(R),
	rdfs_individual_of(R, skos:'ConceptScheme').
amalgame_shape(R, [shape(ellipse),
	      fillcolor(Color),
	      style(filled),
              fontsize(10)]) :-
	atom(R),
	rdfs_individual_of(R, amalgame:'Alignment'),
	!,
	artifact_color(R, Color).
amalgame_shape(_R, [shape(box),
	       fontsize(10)]).

process_color(R, '#FFCC99') :-
	rdfs_individual_of(R, amalgame:'Subtracter'),
	!.
process_color(R, '#99CCFF') :-
	rdfs_individual_of(R, amalgame:'Selecter'),
	!.
process_color(R, '#CC99FF') :-
	rdfs_individual_of(R, amalgame:'Matcher'),
	!.
process_color(R, '#FF99CC') :-
	rdfs_individual_of(R, amalgame:'EvaluationProcess'),
	!.
process_color(_, '#DDDDDD').

artifact_color(R, '#ACFF89') :-
	rdf(R, amalgame:status, amalgame:final),
	!.
artifact_color(R, '#FFFFFF') :-
	rdf(R, amalgame:status, amalgame:discarded),
	!.
artifact_color(_R, '#EEFFEE').


%%	amalgame_label(+Alignment, +Resource, +Lang, +MaxLenth, -Label)
%
%	Defines the node label of Resource.

amalgame_label(Alignment, Resource, Lang, MaxLen, Label) :-
	rdf_display_label(Resource, Lang, Text),
	truncate_atom(Text, MaxLen, Label0),
	stats_label_list(Alignment, Resource, Stats),
	(   rdfs_individual_of(Resource, amalgame:'Mapping')
	->  nickname(Alignment, Resource, Abbreviation),
	    atomic_list_concat([Abbreviation, '.', Label0, '\n'|Stats], Label)
	;   atomic_list_concat([Label0, '\n'|Stats], Label)
	).

stats_label_list(_Alignment, Resource, [Count]) :-
	is_vocabulary(Resource),
	voc_property(Resource, numberOfConcepts(Count), [compute(no)]),
	!.
stats_label_list(Alignment, Resource, [IPercA]) :-
	stats_cache(Resource-Alignment, mstats(Stats)),
	option(inputPercentage(IPerc), Stats, 0),
	IPerc > 1,
	format(atom(IPercA), '~0f%', [IPerc]),
	!.

stats_label_list(Alignment, Resource, [ConceptStats]) :-
	stats_cache(Resource-Alignment, mstats(Stats)),
	option(sourcePercentageInput(SPerc), Stats, 0),
	option(targetPercentageInput(TPerc), Stats, 0),
	format(atom(ConceptStats), '~0f% ~0f%', [SPerc, TPerc]),
	!.


stats_label_list(_, _, []).

