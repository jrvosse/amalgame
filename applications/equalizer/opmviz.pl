:- module(eq_opmviz,
	  [ html_opmviz//1,
	    reply_alignment_graph/2
	  ]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_request_value)).
:- use_module(library(http/http_session)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_host)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).
:- use_module(components(label)).
:- use_module(components(graphviz)).
:- use_module(library(yui3)).
:- use_module(library(amalgame/expand_graph)).
:- use_module(library(amalgame/alignment)).
:- use_module(eq_util).

:- http_handler(amalgame(opmviz), http_opmviz, []).

:- setting(secondary_input, atom, show, 'Show or hide arrows for amalgame:secondary_input').


opmviz_options(Alignment,
	       [edge_links(false),
		shape_hook(opm_shape),
		label_hook(opm_label(Alignment)),
		graph_attributes([])
	       ]).

is_meta(shape_hook).
is_meta(label_hook).


%%	http_graphviz(+Request)
%
%	Emit html page with a visualization of a graph.

http_opmviz(Request) :-
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
	expand_node(Selected, Alignment),
	(   Format \== html
	->  reply_alignment_graph(Alignment, Format)
	;   opmviz_options(Alignment, Options),
	    reply_html_page(cliopatria(default),
			    [ title(['Graph for ', \turtle_label(Alignment)])
			    ],
			    [ \graphviz_graph(opm_triples(Alignment), Options)
			    ])
	).

/* Fixme, compute the stats for all outputs of a process */

expand_node(URI, _Alignment) :-
	var(URI),
	!.
expand_node(URI, Alignment) :-
	rdfs_individual_of(URI, amalgame:'Mapping'),
	!,
	mapping_counts(URI, Alignment, _, _, _, _, _).
%expand_node(URI, Alignment) :-
%	rdfs_individual_of(URI, amalgame:'Process'),
%	!,
%	expand_process(Alignment, URI, _).
expand_node(_, _).


%%	reply_alignment_graph(+AlignmentURI)
%
%	Emit an alignment graph.

reply_alignment_graph(Alignment, Format) :-
	opmviz_options(Alignment, Options),
	opm_triples(Alignment, Triples),
	meta_options(is_meta, Options, QOptions),
	reply_graphviz_graph(Triples, Format, QOptions).


%%	html_opmviz(+Graph)
%
%	Emit html component with a visualization of Graph.

html_opmviz(Graph) -->
	{ opmviz_options(Graph, Options)
	},
	graphviz_graph(opm_triples(Graph), Options).

html_opmviz(Graph) -->
	{ http_link_to_id(http_opmviz, [graph(Graph),format(svg)], HREF)
	},
	html([ object([ id(opmviz),
			data(HREF),
			type('image/svg+xml')
		      ],
		      [])
	     ]).


%%	opm_triples(Graph, Triples)
%
%	Triples are all rdf(S,P,O) in Graph.

opm_triples(Graph, Triples) :-
	findall(rdf(S,P,O), opm_graph_triple(Graph,S,P,O), Triples).

% hack, to get a better layout we reverse the arrow :(
opm_graph_triple(Graph,Scheme,P,Graph) :-
	rdf_equal(amalgame:includedIn, P),
	rdf(Graph,amalgame:includes,Scheme,Graph).
opm_graph_triple(Graph,S,P,O) :-
	rdf(S,P,O,Graph),
	is_opm_property(P),
	\+ empty_evaluation(Graph, S).


is_opm_property(P) :-
	rdfs_subproperty_of(P, opmv:used),
	(   setting(secondary_input, hide)
	->  \+ rdf_equal(amalgame:secondary_input, P)
	;   true
	),
	!.

is_opm_property(P) :-
	rdfs_subproperty_of(P, opmv:wasGeneratedBy),
	!.
is_opm_property(P) :-
	rdfs_subproperty_of(P, opmv:wasDerivedFrom),
	!.
is_opm_property(P) :-
	rdfs_subproperty_of(P, opmv:wasTriggeredBy),
	!.
% filter out empty evaluations ...

empty_evaluation(Strategy,M) :-
	rdfs_individual_of(M, amalgame:'Mapping'),
	stats_cache(M-Strategy, stats(0,0,0,_,_)),!.

empty_evaluation(Strategy,M) :-
	rdfs_individual_of(M, amalgame:'EvaluatedMapping'),
	with_mutex(M, mapping_counts(M,Strategy,0,0,0,_,_)), !.

% and processes resulting in empty evals
empty_evaluation(Strategy,Process) :-
	rdfs_individual_of(Process, amalgame:'EvaluationProcess'),
	rdf(Empty, opmv:wasGeneratedBy, Process, Strategy),
	empty_evaluation(Strategy, Empty).


%%	opm_shape(+Resource, -Shape)
%
%	Defines graph node shape for different types of OPM resources.

opm_shape(R, [shape(box),
	      style(filled),
	      fillcolor(Color),
	      fontsize(10)]) :-
	atom(R),
	rdfs_individual_of(R, opmv:'Process'),
	!,
	process_color(R, Color).
opm_shape(R, [shape(ellipse),
	      style(filled),
	      fillcolor('#EEEEEE'),
	      fontsize(10)]) :-
	atom(R),
	rdf(R, rdf:type, skos:'ConceptScheme').
opm_shape(R, [shape(ellipse),
	      fillcolor(Color),
	      style(filled),
              fontsize(10)]) :-
	atom(R),
	rdfs_individual_of(R, opmv:'Artifact'),
	!,
	artifact_color(R, Color).
opm_shape(_R, [shape(box),
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

artifact_color(R, '#CCFF99') :-
	rdf(R, amalgame:status, amalgame:final),
	!.
artifact_color(_R, '#FFFFFF').


%%	opm_label(+Alignment, +Resource, +Lang, +MaxLenth, -Label)
%
%	Defines the node label of Resource.

opm_label(Alignment, Resource, Lang, MaxLen, Label) :-
	rdf_display_label(Resource, Lang, Text),
	truncate_atom(Text, MaxLen, Label0),
	stats_label_list(Alignment, Resource, Stats),
	(   rdfs_individual_of(Resource, amalgame:'Mapping')
	->  nickname(Alignment, Resource, Abbreviation),
	    concat_atom([Abbreviation, '.', Label0, '\n'|Stats], Label)
	;   concat_atom([Label0, '\n'|Stats], Label)
	).

stats_label_list(Alignment, Resource, [Count]) :-
	stats_cache(Resource-Alignment, stats(Count)),
	!.
stats_label_list(Alignment, Resource, [SPerc, '% - ', TPerc, '%']) :-
	stats_cache(Resource-Alignment, stats(_,_,_, SPerc, TPerc)),
	!.
stats_label_list(_, _, []).

