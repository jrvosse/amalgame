:- module(eq_voc_api,
	  [
	  ]).

:- use_module(library(semweb/rdf_db)).  % for rdf_meta
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(count)).
:- use_module(library(semweb/rdf_label)).

:- use_module(library(amalgame/util)).
:- use_module(library(amalgame/vocabulary)).
:- use_module(library(amalgame/voc_stats)).
:- use_module(library(amalgame/expand_graph)).

:- http_handler(amalgame(data/voc), http_data_voc, []).
:- http_handler(amalgame(data/mappinglist), http_mapping_list, []).

%%	http_mapping_list(+Request)
%
%	Return a JSON object with the mappings in an alignment

http_mapping_list(Request) :-
	http_parameters(Request,
			[ alignment(Alignment, [description('URL of strategy')]),
			  status(Status, [default(finalized)])
			]),
	Obj = json([uri=URI, label=Label]),
	findall(Obj, mapping_in_alignment(Alignment, URI, Label, [status(Status)]), Mappings),
	reply_json(Mappings).

mapping_in_alignment(Alignment, MappingId, Label, Options) :-
	rdf(MappingId, rdf:type, amalgame:'Mapping', Alignment),
	(   option(status(finalized), Options)
	->  rdf_graph(MappingId)
	;   true
	),
	rdf_display_label(MappingId, Label).


http_data_voc(Request) :-
	setting(amalgame:rows_per_page, RowsPerPage),
	http_parameters(Request,
			[ url(URL,
			      [description('URL of scheme or vocabulary')]),
			  alignment(Strategy, [description('URL of strategy')]),
			  limit(Limit,
				[default(RowsPerPage), number,
				 description('limit number of concepts returned')]),
			  offset(Offset,
				 [default(0), number,
				  description('first result that is returned')])
		       ]),
	expand_node(Strategy, URL, Scheme),
	voc_property(URL, numberOfConcepts(Count)),
	Max is Limit + Offset,
	answer_set(C, vocab_member(C, Scheme), Max, Concepts),

	list_offset(Concepts, Offset, COffset),
	list_limit(COffset, Limit, CLimit, _),
	maplist(concept_data,CLimit, RichConcepts),
	reply_json(json([url=URL,
			 limit=Limit,
			 offset=Offset,
			 concepts=json(RichConcepts),
			 total=Count])).


concept_data(Concept, Concept=json(Data)) :-
	findall(Prop, concept_prop(Concept, Prop), DataS),
	sort(DataS, Data).

concept_prop(C, prefLabel=L) :-
	rdf_lang(C, skos:prefLabel, L).

concept_prop(C, altLabel=L) :-
	rdf_lang(C, skos:altLabel, L).

concept_prop(C, example=L) :-
	rdf_has(C, skos:example, L).


