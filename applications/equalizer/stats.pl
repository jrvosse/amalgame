:- module(eq_stats,
	  []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(amalgame/expand_graph)).

:- use_module(cliopatria(components/label)).

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
	amalgame_provenance(URL, Prov),
	amalgame_stats(URL, Stats),
	append(Prov, Stats, Info),
	html_current_option(content_type(Type)),
	phrase(html_table(Info), HTML),
	format('Content-type: ~w~n~n', [Type]),
	print_html(HTML).

html_table(Stats) -->
 	html(table(tbody(\html_rows(Stats)))).

html_rows([]) --> !.
html_rows([Key-Value|Ss]) -->
	html_row(Key, Value),
	html_rows(Ss).

html_row(Key, set(Values)) -->
	 html(tr([th(Key),
		  td([])
		 ])),
	 html_rows(Values).
html_row(Key, Value) -->
	 html(tr([td(Key),
		 td(\html_cell(Value))
		])).

html_cell([]) --> !.
html_cell(Vs) -->
	{ is_list(Vs)
	},
	!,
	html_cell_list(Vs).
html_cell(V) -->
 	html(V).

html_cell_list([V]) -->
	html_cell(V).
html_cell_list([V|Vs]) -->
	html_cell(V),
	html(', '),
	html_cell_list(Vs).


%%	amalgame_stats(+Mapping, -Stats)
%
%	Stats of a resourcemapping

amalgame_stats(URL, ['total mappings'-Total,
		     'mapped source concepts'-SN,
		     'mapped target concepts'-TN,
		     'browse' - a(href(HREF), URL)
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
	length(Ts, TN),
	resource_link(URL, HREF).

amalgame_stats(Scheme,
	    ['Total concepts'-Total,
	     'browse' - a(href(HREF), Scheme)
	    ]) :-
	rdfs_individual_of(Scheme, skos:'ConceptScheme'),
	!,
 	findall(C,rdf(C,skos:inScheme,Scheme), Cs),
	length(Cs, Total),
	resource_link(Scheme,HREF).

amalgame_stats(URL, ['browse' - a(href(HREF), URL)]) :-
	resource_link(URL,HREF).
%:-
%	phrase(rdf_link(URL), Tokens),
%	with_output_to(atom(Link), print_html(Tokens)).




%%	amalgame_provenance(+R, -Provenance:[key-value])
%
%	Provenance is a list of key-value pairs with provenance about R.

amalgame_provenance(R, Provenance) :-
	findall(Key-Value, ag_prov(R, Key, Value), Provenance).




ag_prov(R, 'created by', V) :-
	rdf(R, dc:creator, V).
ag_prov(R, 'created at', V) :-
	rdf(R, dc:date, V).
ag_prov(Graph, contributors, Vs) :-
	rdfs_individual_of(Graph, amalgame:'Alignment'),
 	findall(V,
		(   rdf(R, _, _, Graph),
		    \+ R == Graph,
		    rdf(R, dc:creator, V),
		    \+ rdf(Graph, dc:creator, V)
		), Vs0),
	Vs0 \== [],
	!,
	sort(Vs0, Vs).
ag_prov(Process, parameters, set(Params)) :-
	rdfs_individual_of(Process, opmv:'Process'),
	!,
	rdf(Process, amalgame:parameters, literal(SearchString)),
	concat_atom(Ps, '&', SearchString),
	maplist(param_to_prov, Ps, Params).

param_to_prov(P, Key-Value) :-
 	concat_atom([Key,Value], '=', P).


/*ag_prov(Mapping, Key, Value) :-
	rdfs_individual_of(Mapping, amalgame:'Mapping'),
	!,
	rdf_has(Mapping, opmv:wasGeneratedBy, Process),
	ag_prov(Process, Key, Value).
*/

align_source(align(S,_,_), S).
align_target(align(_,T,_), T).


