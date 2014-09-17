:- module(ag_voc_api,
	  [
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(amalgame/json_util)).

:- http_handler(amalgame(data/mappinglist), http_mapping_list, []).
:- http_handler(amalgame(data/nodelist), http_node_list, []).

%%	http_mapping_list(+Request)
%
%	Return a JSON object with the (final) mappings in an
%	alignment strategy.

http_mapping_list(Request) :-
	http_parameters(Request,
			[ strategy(Strategy,
				   [description('URL of strategy')]),
			  status(Status,
				 [default(final)])
			]),
	Obj = json([uri=URI, label=Label]),
	findall(Obj,
		mapping_in_strategy(Strategy, URI, Label,
				    [status(Status)]),
		Mappings),
	reply_json(Mappings).

mapping_in_strategy(Strategy, MappingId, Label, Options) :-
	option(status(StatusRequired), Options),
	rdf(MappingId, rdf:type, amalgame:'Mapping', Strategy),
	rdf(MappingId, amalgame:status, MappingStatus),
	rdf_global_id(_NS:StatusRequired, MappingStatus),
	rdf_display_label(MappingId, Label).

http_node_list(Request) :-
	http_parameters(Request,
			[ strategy(Strategy,
				   [description('URL of strategy')])
			]),
	js_strategy_nodes(Strategy, Nodes),
	reply_json(Nodes).

