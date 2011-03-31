:- module(opmviz,
	  [ html_opmviz//1
	  ]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_request_value)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_host)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(components(label)).
:- use_module(components(graphviz)).

:- http_handler(root(opmviz), http_opmviz, []).


opmviz_options([wrap_url(opm_url)]).


%%	http_graphviz(+Request)
%
%	Emit html page with a visualization of a graph.

http_opmviz(Request) :-
	http_parameters(Request,
			[ graph(Graph,
				 [description('URI from which we request the context')])
 			]),
	opmviz_options(Options),
	reply_html_page(cliopatria(default),
			[ title(['Graph for ', \turtle_label(Graph)])
			],
			[ \graphviz_graph(opm_triples(Graph), Options)
			]).


%%	html_opmviz(+Graph)
%
%	Emit html component with a visualization of Graph.

html_opmviz(Graph) -->
	{ opmviz_options(Options)
	},
	graphviz_graph(opm_triples(Graph), Options).


%%	opm_triples(Graph, Triples)
%
%	Triples are all rdf(S,P,O) in Graph.

opm_triples(Graph, Triples) :-
	findall(rdf(S,P,O),
		(   rdf(S,P,O,Graph),
		    is_opm_property(P)
		),
		Triples).

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


%%	opm_url(+Resource, -URL)
%
%	URL becomes a javascript link.

opm_url(R, HREF) :-
	format(atom(HREF), 'javascript:nodeSelect("~w")', [R]).
