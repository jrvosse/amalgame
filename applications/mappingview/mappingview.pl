:- module(mapping_view,
	  [ html_mapping_view//1,    % +Id
 	    js_mapping_view//3       % +Mapping_URL, +Id, +HTML_Element
	  ]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_request_value)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_host)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_json)).
:- use_module(library(http/js_write)).
:- use_module(library(yui3)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_litindex)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(settings)).
:- use_module(user(user_db)).
:- use_module(components(label)).

:- use_module(library(skos/vocabularies)).
:- use_module(library(amalgame/alignment_graph)).
:- use_module(library(amalgame/edoal)).
:- use_module(library(ag_util)).

:- setting(rows_per_page, integer, 10,
	   'Maximum number of mappings shown.').

% add local web directories from which static files are served.

:- prolog_load_context(directory, Dir),
   asserta(user:file_search_path(mv, Dir)).
:- asserta(user:file_search_path(css, mv(web))).
:- asserta(user:file_search_path(js, mv(web))).

% http handlers for this applications

:- http_handler(amalgame(mappingview), http_mapping_view, []).
:- http_handler(amalgame(data/mapping), http_data_mapping, []).
:- http_handler(amalgame(data/mappingevaluate), http_data_mapping_evaluate, []).
:- http_handler(amalgame(data/resourcetree), http_data_resource_tree, []).

%%	http_mapping_view(+Request)
%
%	HTTP handler for web page with concept finder widget.

http_mapping_view(Request) :-
	http_parameters(Request,
			[ url(URL,
			      [description('URL of an alignment graph')])
			]),
	html_page(URL).


		 /*******************************
		 *		HTML		*
		 *******************************/

html_page(URL) :-
   	reply_html_page(test,%cliopatria(default),
			[ title(['mapping -- ', URL])
 			],
			[ div(class('yui3-skin-sam'),
			      [ div(id(header), []),
				div(id(main), \html_body(URL))
			      ])
			]).

html_body(URL) -->
	html_requires(css('gallery-paginator.css')),
	html_mapping_view(mappingview),
	html(script(type('text/javascript'),
		    \js_yui3([{modules:{gallery: 'gallery-2010.05.21-18-16'}}
			     ],
			     [node,event,widget,datasource,datatable,'datatable-sort',
			      'gallery-paginator'
			     ],
			     [\js_mapping_view(URL, mappingview, '#mappingview')
			     ])
		   )).

%%	html_mapping_view
%
%	Emit HTML container for mapping view

html_mapping_view(Id) -->
	html_requires(css('mappingview.css')),
	html([div(id(sourcetree), []),
	      div(id(Id), []),
	      div(id(targettree), []),
	      div(id(paginator), [])
	     ]).

%%	js_mapping_view
%
%	Emit JavaScript to initialize a mapping view

js_mapping_view(URL, Id, El) -->
	{ atom_concat(Id, 'DS', DS),
	  atom_concat(Id, 'Paginator', P)
	},
	js_format_resource,
	js_mapping_view_datasource(DS),
	js_datatable(Id, El, DS),
	js_paginator(P),
	js_yui3_on(DS, response,
		   \js_function([e],
				\[ P,'.setTotalRecords(e.response.meta.totalNumberOfResults, true);\n',
				   P,'.render();'
				 ])),
	js_yui3_on(P, changeRequest,
		   \js_function([state],
				\[ 'this.setPage(state.page, true);\n',
				   Id,'.datasource.load({request:"?url=',URL,'&offset="+state.recordOffset});\n'
				 ])),
 	html([Id,'.datasource.load({request:"?url=',URL,'"});\n']).
	%js_tree(source),
	%js_tree(target).

js_mapping_view_datasource(Id) -->
	{ http_location_by_id(http_data_mapping, Server)
	},
	js_new(Id,
	       'Y.DataSource.IO'({source:Server})),
	js_yui3_plug(Id,
		'Y.Plugin.DataSourceJSONSchema',
		{ schema:
		  { resultListLocator: mapping,
		    resultFields: [source, target],
		    metaFields: {totalNumberOfResults:totalNumberOfResults}
		  }
		}).

js_datatable(Id, El, Datasource) -->
	js_new(Id,
	       'Y.DataTable.Base'({columnset:[{key:source,
					       formatter:symbol(formatResource),
					        sortable:symbol(true)
					      },
					      {key:target,
					       formatter:symbol(formatResource),
					       sortable:symbol(true)
					      }],
				   plugins: [ symbol('Y.Plugin.DataTableSort') ]
				  })),
	js_yui3_plug(Id,
		     'Y.Plugin.DataTableDataSource',
		     { datasource: symbol(Datasource) }),
	html([Id,'.render("',El,'");\n']).

js_format_resource -->
	js_function_decl(formatResource, [o],
			 \[
'   return o.value.label;\n'
			  ]).

js_paginator(Id) -->
	{ setting(rows_per_page, Rows)
	},
	js_new(Id, 'Y.Paginator'({rowsPerPage:Rows,
				  template: '{FirstPageLink} {PreviousPageLink} {PageLinks} {NextPageLink} {LastPageLink}',
				  firstPageLinkLabel:'|&lt;',
				  previousPageLinkLabel: '&lt;',
				  nextPageLinkLabel: '&gt;',
				  lastPageLinkLabel: '&gt;|'
				 })),
	js_yui3_render(Id, '#paginator').


		 /*******************************
		 *		API		*
		 *******************************/

%%	http_data_mapping(+Request)
%
%	Emit JSON object with mappings for a URL.

http_data_mapping(Request) :-
	setting(rows_per_page, RowsPerPage),
	http_parameters(Request,
			[ url(URL,
			      [description('URL of mapping graph')]),
			  sort(SortBy,
			       [default(source),
				oneof([source,target]),
				description('Sort by')]),
			  limit(Limit,
				[default(RowsPerPage), number,
				 description('limit number of mappings returned')]),
			  offset(Offset,
				 [default(0), number,
				  description('first result that is returned')])
		       ]),
	e(URL, Mapping0),
	length(Mapping0, Length),
	maplist(mapping_label, Mapping0, Mapping1),
	sort_key(SortBy, SortKey),
	sort_by_arg(Mapping1, SortKey, MSorted),
 	list_offset(MSorted, Offset, MOffset),
	list_limit(MOffset, Limit, MLimit, _),
	mapping_data(MLimit, Mapping),
	reply_json(json([url=URL,
			 totalNumberOfResults=Length,
			 limit=Limit,
			 offset=Offset,
			 mapping=Mapping])).

sort_key(source, 2).
sort_key(target, 4).

mapping_label(align(S, T, Prov), align(S,SL,T,TL,Prov)) :-
	resource_label_text(S, SL),
	resource_label_text(T, TL).

mapping_data([], []).
mapping_data([Align|As], [Obj|Os]) :-
	Align = align(Source, SLabel, Target, TLabel, _Prov),
	Obj = json([source=SourceObj, target=TargetObj]),
	resource_data(Source, SLabel, SourceObj),
	resource_data(Target, TLabel, TargetObj),
	mapping_data(As, Os).

resource_data(R, Label, json([uri=R,
			      label=Label,
			      definition=Def,
			      scope=Scope,
			      alt=Alt])) :-
	resource_definition(R, Def),
	resource_scope(R, Scope),
	resource_alternative_labels(R, Label, Alt).

resource_definition(R, Def) :-
	(   rdf_has(R, skos:definition, Lit)
	->  literal_text(Lit, Def)
	;   Def = ''
	).
resource_scope(R, Scope) :-
	(   rdf_has(R, skos:scopeNote, Lit)
	->  literal_text(Lit, Scope)
	;   Scope = ''
	).
resource_alternative_labels(R, Label, Alt) :-
	findall(L, resource_label_text(R, L), Ls),
	delete(Ls, Label, Alt1),
 	sort(Alt1, Alt).

resource_label_text(R, L) :-
	(   rdf_label(R, Lit)
	->  literal_text(Lit, L)
	;   L = R
	).

%%	http_data_mapping_evaluate(+Request)
%
%	Accept/reject a mapping.

http_data_mapping_evaluate(Request) :-
	logged_on(User0, anonymous),
	user_property(User0, url(User)),
	rdf_equal(skos:closeMatch, CloseMatch),
	http_parameters(Request,
			[  source(Source,
				  [description('Source of mapping')]),
			   target(Target,
				  [descript('Target of mapping')]),
 			   relation(Relation,
				     [default(CloseMatch),
				      description('Relation between source and target')]),
			   graph(Graph,
				 [description('Graph to store user actions')]),
			   comment(Comment,
				   [default(''),
				    description('Explanation of action')])
			]),
	Options = [user(User),
		   relation(Relation),
		   graph(Graph),
		   comment(Comment)
		  ],
	assert_cell(Source, Target, Options),
 	reply_json(json([source=Source,
			 target=Target,
 			 relation=Relation])).


%%	http_data_resource_tree(+Request)
%
%	Tree context in which a resource occurs

http_data_resource_tree(Request) :-
	http_parameters(Request,
			[ uri(URI,
			      [description('Concept from which we request the tree')])
			]),
	Node = node(URI, [hit], Children),
	rdf_equal(skos:broader, Rel),
	ancestor_tree(Node, Rel, Tree, []),
        children(URI, Rel, Children, []),
	tree_to_json(Tree, [], JSONTree),
        JSON = json([ uri=URI,
                      tree=JSONTree
                    ]),
        reply_json(JSON).


ancestor_tree(Node, Rel, Tree, Options) :-
        Node = node(URI,_,_),
        rdf_has(URI, Rel, Parent),
        URI \== Parent,
        (   select_option(sibblings(true), Options, Options1)
        ->  ancestor_tree(node(Parent, [], [Node|Siblings]), Rel, Tree, Options1),
            children(Parent, Rel, Children, Options),
            select(node(URI,_,_), Children, Siblings)
        ;   ancestor_tree(node(Parent, [], [Node]), Rel, Tree, Options)
        ).
ancestor_tree(Tree, _Rel, Tree, _).

children(R, Rel, Children, _Options) :-
        findall(node(Child, [], HasChild),
		(   rdf_has(Child, Rel, R),
		    has_child(Child, Rel, HasChild)
		),
		Children).

has_child(R, Rel, true) :-
        rdf_has(_, Rel, R),
        !.
has_child(_, _, false).

%%  tree_to_json(+Tree:node(uri,nodeList), +DisplayProperties, -JSON)
%
%   Tree to JSON term.

tree_to_json(node(R,Attr,Children), Ps, json(Data)) :-
        attr_params(Attr, Params),
        append(Params, Ps, Data0),
        (   is_list(Children)
        ->  Data1 = [children=Nodes|Data0],
            nodes_to_json(Children, Ps, Nodes)
        ;   bool_to_json(Children,HasChildren)
        ->  Data1 = [hasChildren=HasChildren|Data0]
        ;   Data1 = Data0
        ),
	rdf_label(R,Lit),
	literal_text(Lit, L),
        Data = [uri=R, label=L|Data1].

nodes_to_json([], _, []) :- !.
nodes_to_json([Node|Nodes], Ps, [JNode|JSON]) :- !,
        tree_to_json(Node, Ps, JNode),
        nodes_to_json(Nodes, Ps, JSON).
nodes_to_json(Bool, _, JSON) :-
        bool_to_json(Bool, JSON).

bool_to_json(false, @false).
bool_to_json(true, @true).

attr_params([], []).
attr_params([H|T], [P|Ps]) :-
        attr_param(H, P), !,
        attr_params(T, Ps).
attr_params([_|T], Ps) :-
        attr_params(T, Ps).

attr_param(Term, Key=Value) :-
        Term =.. [Key,Value],
        !.
attr_param(hit, hit=Bool) :-
        bool_to_json(true, Bool).
