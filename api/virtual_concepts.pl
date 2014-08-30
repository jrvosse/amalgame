:- module(virtual_concepts,
	  []
	 ).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).

:- use_module(library(amalgame/voc_stats)).
:- use_module(library(amalgame/vocabulary)).
:- use_module(library(amalgame/util)).

:- use_module(library(skos/util)).
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
	(   voc_property(Parent, virtual(false))
	->  http_concepts(Request)
	;   findall(Label-Concept, concept_of(Type, Parent, Query, Concept, Label), Concepts),
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
	).

concept_of(Type, Parent, Query, Concept, Label) :-
	var(Query),
	!,
	concept(Type, Parent, Concept),
	rdf_display_label(Concept, Label).

concept_of(Type, Parent, Query, Concept, Label) :-
	concept(Type, Parent, Concept),
	once(label_prefix(Query, Concept, Lit)),
	literal_text(Lit, Label).

concept(inscheme, ConceptScheme, Concept) :- !,
	vocab_member(Concept, ConceptScheme).
concept(topconcept, ConceptScheme, Concept) :- !,
	vocab_member(Concept, ConceptScheme),
	\+ (skos_parent_child(_, Concept)).
concept(child, Parent, Concept) :-
	skos_parent_child(Parent, Concept).
concept(descendant, Parent, Concept) :-
	skos_descendant_of(Parent, Concept).
concept(related, Parent, Concept) :-
	skos_related_concept(Parent, Concept).

%%	label_prefix(+Query, -R, -Lit)
%
%	True if Query matches a literal value of R.

label_prefix(Query, R, Lit) :-
	rdf_has(R, rdfs:label, literal(prefix(Query), Lit)).
label_prefix(Query, R, Lit) :-
	rdf_has(O, rdf:value, literal(prefix(Query), Lit)),
	rdf_has(R, rdfs:label, O).
