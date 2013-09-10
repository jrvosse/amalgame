:- module(virtual_concepts,
	  []
	 ).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).

:- use_module(library(amalgame/expand_graph)).
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
	(   ( is_virtual_scheme(Parent), var(Query), Type == inscheme)
	->  findall(Label-C, (
		    vocab_member(C, Parent),
		    rdf_display_label(C, Label)
		), Concepts),
	    sort(Concepts, Sorted),
	    length(Sorted, Total),
	    list_offset(Sorted, Offset, OffsetResults),
	    list_limit(OffsetResults, Limit, LimitResults, _),
	    concept_results(LimitResults, Graphs, JSONResults),
	    reply_json(json([parent=Parent,
			 offset=Offset,
			 limit=Limit,
			 totalNumberOfResults=Total,
			 results=JSONResults]))
	;   http_concepts(Request)
	).

