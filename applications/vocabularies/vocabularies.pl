:- module(voc_stats,
	  [
	  ]).

/** <module> Amalgame vocabulary services

This module provides (all private) HTTP handlers for vocabulary-centric
pages and services.

*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).

:- use_module(user(user_db)).
:- use_module(components(messages)).

:- use_module(amalgame(skos/vocabularies)).
:- use_module(amalgame_apps(alignment/alignment)).

:- use_module(components).

:- http_handler(amalgame(list_skos_vocs),       http_list_skos_vocs,     []).
:- http_handler(amalgame(list_skos_voc),        http_list_skos_voc,      []).
:- http_handler(amalgame(compute_voc_stats),    http_compute_voc_stats,  []).
:- http_handler(amalgame(clear_voc_stats),      http_clear_voc_stats,    []).
:- http_handler(amalgame(partition_voc),        http_partition_voc,      []).
:- http_handler(amalgame(delete_partition_voc),	http_delpart_voc,        []).

%%	http_list_skos_vocs(+Request) is det.
%
%	HTTP handler returning an HTML page listing of all skos
%	vocabularies with statistics.

http_list_skos_vocs(_Request) :-
	reply_html_page(cliopatria(default),
			[title('SKOS vocabularies')
			],
			[ h4('SKOS concept schemes in the RDF store'),
			  \show_schemes
			]).

%%	http_list_skos_voc(+Request) is det.
%
%	HTTP handler returning an HTML page providing info about and
%	offering actions upon a skos vocabulary (ConceptScheme).

http_list_skos_voc(Request) :-
	http_parameters(Request,
			[voc(Scheme, [])
			]),
	reply_html_page(cliopatria(default),
			[title('SKOS concept scheme')
			],
			[ \show_scheme(Scheme)
			]).

%%	http_compute_voc_stats(+Request) is det.
%
%	HTTP handler to compute all missing statistics for a given
%	vocabulary or for all vocabularies.
%	Returns a page with progress messages using
%	call_showing_messages/2.

http_compute_voc_stats(Request) :-
	http_parameters(Request, [voc(all, [])]),
	authorized(write(amalgame_cache, write)),
	http_link_to_id(http_list_skos_vocs, [], Link),
	call_showing_messages(voc_ensure_stats(all),
			      [head(title('Amalgame: calculating vocabulary stats')),
			       footer(div([class(readymeassage)],
					  [h4('All computations done'),
					   'See ', a([href(Link)],['vocabulary overview']),
					   ' to inspect results.']))
			      ]).

http_compute_voc_stats(Request) :-
	http_parameters(Request,
			[voc(Graph, []),
			 stat(Stats, [list(atom)])
			]),
	forall(member(Stat, Stats),
	       (   Type =.. [Stat, Graph],
		   voc_ensure_stats(Type)
	       )
	      ),
	http_redirect(moved, location_by_id(http_list_skos_vocs), Request).


%%	http_clear_voc_stats(?Request) is det.
%
%	Clears all named graphs containing cached amalgame results.

http_clear_voc_stats(_Request):-
	authorized(write(amalgame_cache, clear)),
	http_link_to_id(http_list_skos_vocs, [], Link),
	call_showing_messages(voc_clear_stats(amalgame_vocs),
			      [head(title('Amalgame: clearing caches')),
			       footer(div([class(readymeassage)],
					  [h4('All caches cleared'),
					   'See ', a([href(Link)],['vocabulary overview']),
					   ' to inspect results.']))
			      ]).

%%	http_partition_voc(+Request) is det.
%
%	Partition a graph into one or more other graphs.
%	Currently only partitioning into a mapped/unmapped graph is
%	supported.

http_partition_voc(Request) :-
	authorized(write(default, partition(sample))),
	http_parameters(Request,
			[voc(Graph, [description('URI of the vocabulary to be partitioned')]),
			 partition_method(Method, [oneof([mapped,type]), default(mapped)])
			]),
	voc_partition(Request, Graph, Method, Partitioning),
	forall(member(PVoc, Partitioning), voc_ensure_stats(all(PVoc))),
	reply_html_page(cliopatria(default),
			[title('Amalgame: partitioned vocabulary')
			],
			[ h4('Amalgame: partitioned vocabulary (mapped vs unmapped concepts).'),
			  table([
				 id(skosvoctable)],
				[
				 \voctable_header,
				 \show_schemes(Partitioning, 1, [0, 0, 0, 0, 0])
				])
			]).

%%	http_delpart_voc(+Request) is det.
%
%	Removes all graphs derived from partitioning using
%	http_partition_voc/1.

http_delpart_voc(_Request):-
	authorized(write(amalgame_cache, clear)),
	http_link_to_id(http_list_skos_vocs, [], Link),
	call_showing_messages(voc_delete_derived,
			      [head(title('Amalgame: deleting partioning results')),
			       footer(div([class(readymeassage)],
					  [h4('All computations done'),
					   'See ', a([href(Link)],['vocabulary overview']),
					   ' to inspect results.']))
			      ]
			     ).
