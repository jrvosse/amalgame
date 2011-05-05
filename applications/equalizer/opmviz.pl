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

:- http_handler(amalgame(opmviz), http_opmviz, []).


opmviz_options([edge_links(false),
 		shape_hook(opm_shape),
		graph_attributes([])
	       ]).

is_meta(shape_hook).


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
			  graph(Graph,
				 [description('URI from which we request the context')])
 			]),
	(   Format \== html
	->  reply_alignment_graph(Graph, Format)
	;   opmviz_options(Options),
	    reply_html_page(cliopatria(default),
			    [ title(['Graph for ', \turtle_label(Graph)])
			    ],
			    [ \graphviz_graph(opm_triples(Graph), Options)
			    ])
	).

%%	reply_alignment_graph(+AlignmentURI)
%
%	Emit an alignment graph.

reply_alignment_graph(Alignment, Format) :-
	opmviz_options(Options),
	opm_triples(Alignment, Triples),
	meta_options(is_meta, Options, QOptions),
	reply_graphviz_graph(Triples, Format, QOptions).


%%	html_opmviz(+Graph)
%
%	Emit html component with a visualization of Graph.

html_opmviz(Graph) -->
	{ opmviz_options(Options)
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
	is_opm_property(P).

is_opm_property(P) :-
	rdfs_subproperty_of(P, opmv:used),
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

%%	opm_shape(+Resource, -Shape)
%
%	Defines graph node shape for different types of OPM resources.

opm_shape(R, [shape(octagon),
 	      style(filled),
	      fillcolor(Color),
	      fontsize(10)]) :-
	atom(R),
	rdfs_individual_of(R, opmv:'Process'),
	!,
	process_color(R, Color).
opm_shape(R, [shape(box),
	      style(filled),
	      fillcolor('#EEEEEE'),
	      fontsize(10)]) :-
	atom(R),
	rdf(R, rdf:type, skos:'ConceptScheme').
opm_shape(R, [shape(ellipse),
	      fontsize(10)]) :-
	atom(R),
	rdfs_individual_of(R, opmv:'Artifact'),
	!.
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
process_color(_, '#DDDDDD').
