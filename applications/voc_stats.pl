:- module(voc_stats,
	  [
	   % HTTP entry points:
	   http_list_skos_vocs/1
	  ]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(semweb/rdf_db)).
:- use_module('../skos/vocabularies').

:- http_handler(amalgame(list_skos_vocs),     http_list_skos_vocs,     []).

%%	http_list_skos_vocs(+Request) is det.
%
%	HTTP handler returning an HTML page listing of all skos vocabularies with statistics.

http_list_skos_vocs(_Request) :-
	reply_html_page(cliopatria(default),
			[title('SKOS vocabularies'),
			 style('#skosvoctable { border-collapse: collapse; border: solid #CCCCCC; }'),
			 style('#skosvoctable td, th { border-left: solid #CCCCCC; }'),
			 style('#finalrow td { border-top: solid #AAAAAA; }')
			],
			[ h4('SKOS concept schemes in the RDF store'),
			  \show_schemes
			]).

