:- module(eq_stats,
	  []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(amalgame/expand_graph)).

% http handlers for this applications

:- http_handler(amalgame(data/stats), http_eq_stats, []).

%%	http_eq_stats(+Request)
%
%	Emit HTML snippet with statistics for an amalgame URI

http_eq_stats(Request) :-
 	http_parameters(Request,
			[ url(URL,
			      [description('URL of mapping graph')])
 		       ]),
	amalgame_stats(URL, Stats),
	html_current_option(content_type(Type)),
	phrase(html_stats(Stats), HTML),
	format('Content-type: ~w~n~n', [Type]),
	print_html(HTML).

html_stats(Stats) -->
 	html(table(tbody(\html_rows(Stats)))).

html_rows([]) --> !.
html_rows([Key=Value|Ss]) -->
	html(tr([td(Key),
		 td(Value)
		])),
	html_rows(Ss).


%%	amalgame_stats(+Mapping, -Stats)
%
%	Stats of a resourcemapping

amalgame_stats(URL, ['total mappings'=Total,
		     'mapped source concepts'=SN,
		     'mapped target concepts'=TN
		    ]) :-
	rdfs_individual_of(URL, amalgame:'Mapping'),
	!,
	expand_mapping(URL, Mapping),
 	maplist(align_source, Mapping, Ss0),
	maplist(align_target, Mapping, Ts0),
	sort(Ss0, Ss),
	sort(Ts0, Ts),
	length(Mapping, Total),
	length(Ss, SN),
	length(Ts, TN).

amalgame_stats(Scheme,
	    ['total concepts'=Total
	    ]) :-
	rdfs_individual_of(Scheme, skos:'ConceptScheme'),
	!,
 	findall(C,rdf(C,skos:inScheme,Scheme), Cs),
	length(Cs, Total).

amalgame_stats(_, []).

align_source(align(S,_,_), S).
align_target(align(_,T,_), T).
