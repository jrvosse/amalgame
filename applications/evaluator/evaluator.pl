:- module(valuator, []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_request_value)).
:- use_module(library('http/http_session')).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_host)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/yui_resources)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_litindex)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(settings)).

:- use_module(components(label)).

:- use_module(amalgame(mappings/map)).
:- use_module(amalgame(util/util)).
:- use_module(amalgame(util/json_graph)).

:- setting(evaluator:maxMappings, nonneg, 1000, 'Max number of mappings per mapping file to put in the todo list of the mapping evaluator').

% add local web directories from which static files are served.

:- prolog_load_context(directory, Dir),
   asserta(user:file_search_path(ev, Dir)).
:- asserta(user:file_search_path(css, ev(web))).
:- asserta(user:file_search_path(js, ev(web))).

% http handlers for this applications

:- http_handler(amalgame(evaluator), http_evaluator, []).
:- http_handler(amalgame(api/evaluator/get), json_get_mapping, []).


:- html_resource(evaluator,
                 [ virtual(true),
		   requires([
			     js('evaluator.js'),
			     css('evaluator.css')
			    ])
		 ]).

:- html_resource(js('evaluator.js'),
                 [ requires([
                             yui('treeview/treeview.js'),
                             yui('treeview/assets/skins/sam/treeview.css'),
                             yui('datasource/datasource.js'),
			     yui('button/button.js'),
                             js('parameters.js')
                            ])
                 ]).


%%	http_evaluator(+Request)
%
%	HTTP handler for web page with the alignment evaluator

http_evaluator(Request) :-
	http_parameters(Request, [graph(Graph, [])]),
  	reply_html_page(cliopatria(default),
			[ title(['Vocabulary browser']),
			  script([type('text/javascript'),
				  src('http://yui.yahooapis.com/2.7.0/build/yahoo/yahoo.js')
				 ],[]),
			  script([type('text/javascript'),
				  src('http://yui.yahooapis.com/2.7.0/build/json/json.js')
				 ], []),
			  link([rel(stylesheet),
				type('text/css'),
				href('http://yui.yahooapis.com/2.7.0/build/reset-fonts-grids/reset-fonts-grids.css')
			       ],[]),
			  \html_requires(evaluator)
			],
			[
 			   div(id(main),
			       div(class('main-content'),
				   [ div(id(evaluator), [])
				   ])),
			   script(type('text/javascript'),
				  [ \yui_script(Graph)
 				  ])
			]).

yui_script(Graph) -->
	{
	 setting(http:prefix, Prefix)
	},
	html(['function serverPrefix() { return "', Prefix, '";}\n',
	      'var evaluator = new YAHOO.mazzle.MapCheck(',
	      '\'evaluator\', ',
	      '{graph:\'',
		Graph,
		'\'});'
	     ]
	    ).



json_get_mapping(Request) :-
	http_parameters(Request, [graph(Graph, []), method(Method, [])]),
	(   Method = head
	->  get_first_mappings(Graph,Mappings, Nr)
        ;   get_next_mappings(Graph, Mappings, Nr)
        ),
	(   Mappings = error
	->  reply_json(json([nr_to_go=0, mappings='Error, no mappings found']))
	;   compose_json_answer(Nr, [Mappings], Json),
	    reply_json(Json)
	).

get_first_mappings(Graph, Mappings, Nr) :-
	ensure_todo_list(Graph),
	http_session_data(mappings_to_do(Graph,Todo)),
	Todo = [Mappings|_],
	length(Todo,Nr),!.

get_first_mappings(_,error, 0).

get_next_mappings(Graph, Mappings, Nr) :-
	ensure_todo_list(Graph),
	http_session_data(mappings_to_do(Graph,[_|ToDo])),!,
	http_session_retractall(mappings_to_do(Graph,_)),
	http_session_assert(mappings_to_do(Graph,ToDo)),
	length(ToDo,Nr),
        (   Nr = 0
        ->  Mappings = done-done
        ;   [Mappings|_] = ToDo
        ).


ensure_todo_list(Graph) :-
	http_session_data(mappings_to_do(Graph,Todo)),
	ground(Todo),!.
ensure_todo_list(Graph) :-
	rdf_equal(skos:closeMatch, CM),
	setting(evaluator:maxMappings, N),
	find_unique(rdf(Subject,CM,Object),
		    (
		    has_map([Subject, Object], _, Graph)
		    ),
		    N,
		    List
		   ),
	http_session_assert(mappings_to_do(Graph, List)).



compose_json_answer(Nr, Mappings, Json) :-
	make_display_graph(Mappings, Display),
	graph_to_json(triples,Mappings,Jmappings),
	graph_to_json(spo, Display, Jdisplay),
	Json = json([nr_to_go=Nr,
		     mappings=Jmappings,
		     display=Jdisplay
		    ]).



make_display_graph([], []).
make_display_graph([H|Tail], Out) :-
        make_display_graph(Tail, TailGraph),
        H = rdf(S,P,O),
	iface_key_resource_graph([S,P,O],
				 [(skos:altLabel)-altlabel,
				  (skos:prefLabel)-preflabel,
				  (skos:definition)-prelabel,
				  registered_ns-ns,
                                  abbreviation-prelabel,
                                  iconclassCluster-sublabel,
                                  description-sublabel,
                                  example_thumbs-examples
                                 ],
                                 Display,
				 []),
        append(TailGraph, Display, All),
        sort(All,Out). % just sort to remove duplicates ...

