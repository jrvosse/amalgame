:- module(ag_voc_api,
	  [
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(semweb/rdf_label)).

:- http_handler(amalgame(data/mappinglist), http_mapping_list, []).

%%	http_mapping_list(+Request)
%
%	Return a JSON object with the (finalize) mappings in an
%	alignment strategy. This is a hack that needs fixing.

http_mapping_list(Request) :-
	http_parameters(Request,
			[ strategy(Strategy,
				   [description('URL of strategy')]),
			  status(Status,
				 [default(finalized)])
			]),
	Obj = json([uri=URI, label=Label]),
	findall(Obj, mapping_in_strategy(Strategy, URI, Label, [status(Status)]), Mappings),
	reply_json(Mappings).

mapping_in_strategy(Strategy, MappingId, Label, Options) :-
	rdf(MappingId, rdf:type, amalgame:'Mapping', Strategy),
	(   option(status(finalized), Options)
	->  rdf_graph(MappingId)
	;   true
	),
	rdf_display_label(MappingId, Label).
