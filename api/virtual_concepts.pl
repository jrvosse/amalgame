:- module(virtual_concepts,
	  []
	 ).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(semweb/rdf_label)).

:- use_module(library(amalgame/voc_stats)).
:- use_module(library(amalgame/vocabulary)).
:- use_module(library(amalgame/util)).
:- use_module(api(skos_concepts)).


:- http_handler(amalgame(data/virtualconcepts), http_virtual_concepts, []).

http_virtual_concepts(Request) :-
	http_parameters(Request,
			[ parent(Parent,
				 [description('Concept or concept scheme from which we request the concepts')]),
			  type(Type,
			       [oneof(topconcept,inscheme,child,descendant,related),
				default(inscheme),
				description('Method to determine the concepts')]),
			  offset(Offset,
				[integer, default(0),
				 description('Start of the results returned')]),
			  limit(Limit,
				[integer, default(20),
				 description('maximum number of results returned')]),
			  query(Query,
				[optional(true),
				 description('keyword query to filter the results by')]),
			  graph(Graphs,
				[zero_or_more,
				 description('Named graph to restrict the concepts by')
				])
			]),
	(   ( voc_property(Parent, virtual(true)),
	      var(Query),
	      Type == inscheme)
	->  virtual_inscheme(Parent, [graphs(Graphs), limit(Limit), offset(Offset)])
	;   ( voc_property(Parent, virtual(true)),
	      var(Query),
	      Type == topconcept)
	->  virtual_topconcepts(Parent, [graphs(Graphs), limit(Limit), offset(Offset)])
	;   http_concepts(Request)
	).

virtual_topconcepts(Scheme, Options) :-
	virtual_inscheme(Scheme, Options).

virtual_inscheme(Scheme, Options) :-
	option(graphs(Graphs), Options),
	option(limit(Limit), Options),
	option(offset(Offset), Options),
	findall(Label-C, (
		    vocab_member(C, Scheme),
		    rdf_display_label(C, Label)
		), Concepts),
	    sort(Concepts, Sorted),
	    length(Sorted, Total),
	    list_offset(Sorted, Offset, OffsetResults),
	    list_limit(OffsetResults, Limit, LimitResults, _),
	    concept_results(LimitResults, Graphs, JSONResults),
	    reply_json(json([parent=Scheme,
			 offset=Offset,
			 limit=Limit,
			 totalNumberOfResults=Total,
			 results=JSONResults])).
