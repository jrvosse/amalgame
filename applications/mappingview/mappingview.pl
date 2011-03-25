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
:- use_module(library(amalgame/map)).
:- use_module(library(ag_util)).

:- setting(rows_per_page, integer, 20,
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
:- http_handler(amalgame(private/resourcecontext), http_resource_context, []).

mapping_relations({'http://www.w3.org/2004/02/skos/core#exactMatch':exact,
		   'http://www.w3.org/2004/02/skos/core#closeMatch':close,
		   'http://www.w3.org/2004/02/skos/core#narrowMatch':narrower,
		   'http://www.w3.org/2004/02/skos/core#broadMatch':broader,
		   'http://www.w3.org/2004/02/skos/core#related':related
		  }).


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
		    \js_yui3([{modules:{gallery: 'gallery-2011.01.03-18-30'}
 			      }],
			     [node,event,widget,datasource,datatable,'datatable-sort',
			      'gallery-paginator','querystring-stringify-simple'
 			     ],
			     [\js_mapping_view(URL, mappingview, '#mappingview')
			     ])
		   )).

%%	html_mapping_view
%
%	Emit HTML container for mapping view

html_mapping_view(Id) -->
	html_requires(css('mappingview.css')),
	html([ \html_resource_info(source),
	       div(class(content),
		   [ div(id(Id), []),
		     div(id(paginator), [])
		   ]),
	       \html_resource_info(target)
	     ]).

html_resource_info(Which) -->
	html(div([id(Which+info), class('resource-info')], [])).


%%	js_mapping_view
%
%	Emit JavaScript to initialize a mapping view

js_mapping_view(URL, Id, El) -->
	{ atom_concat(Id, 'DS', DS),
	  atom_concat(Id, 'Paginator', P),
	  mapping_relations(Relations)
	},
	html(['Y.relations=',\js_args([Relations]),';']),
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

js_mapping_view_datasource(Id) -->
	{ http_location_by_id(http_data_mapping, Server)
	},
	js_new(Id,
	       'Y.DataSource.IO'({source:Server})),
	js_yui3_plug(Id,
		'Y.Plugin.DataSourceJSONSchema',
		{ schema:
		  { resultListLocator: mapping,
		    resultFields: [source, target, relation],
		    metaFields: {totalNumberOfResults:totalNumberOfResults}
		  }
		}).

js_datatable(Id, El, Datasource) -->
 	js_set_info,
	js_cell_format,
	js_new(Id,
	       'Y.DataTable.Base'({columnset:[{key:source,
					       formatter:symbol(formatResource),
					       sortable:symbol(true)
					      },
					      {key:relation,
					       formatter:symbol(formatRelation),
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
	js_yui3_on(Id, tbodyCellClick, \js_row_select(Id)),
	html([Id,'.render("',El,'");\n']).

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

js_cell_format -->
 	js_function_decl(formatResource, [o],
			 \[
'    return o.value.label;'
 			  ]),
	js_function_decl(formatRelation, [o],
			 \[
'    var relations = Y.relations,
	 active = o.value ? relations[o.value] : "",
	 name = o.rowindex;
     var html = "<div class=activerelations><div><span>"+active+"</span></div></div>";
     html += "<div class=otherrelations>";\n',
'    for(var uri in relations) {
	 var label = relations[uri],
	 checked = active ? "CHECKED" : "";
	 html += "<div><input name=name class=rcheck autocomplete=off value="+uri+" type=radio "+checked+"><span>"+label+"</span></div>";
     }
     html += "</div>";\n',
'    return html;'
			  ]).

js_row_select(Id) -->
	{ http_location_by_id(http_resource_context, Server),
	  http_location_by_id(http_data_mapping_evaluate, EvaluateServer)
	},
	js_function([e],
			 \[
'    var row = e.currentTarget.get("parentNode"),
         records = ',Id,'.get("recordset"),
         current = records.getRecord( row.get("id")),
	 source = current.getValue("source").uri,
	 target = current.getValue("target").uri;\n',
'    if(e.target.hasClass("rcheck")) {
	 var t = e.target,
	     relation = t.get("value"),
	     rlabel = Y.relations[relation],
	     active = {uri:relation, label:rlabel},
 	     data = current.get("data");\n',
'	 Y.io("',EvaluateServer,'", {data:{source:source,
					   target:target,
					   relation:relation,
					   graph:"test"
					  }});\n',
'	 data.relation = active;
	 current.set("data",data);
	 e.currentTarget.one(".activerelations").setContent("<div><span>"+rlabel+"</span></div>");
     }\n',
'    else {
     var add = (e.ctrlKey||e.metaKey) ? true : false;\n',
'    if(!add) {
	  Y.all(".yui3-datatable tr").removeClass("yui3-datatable-selected");
	  Y.selected = {};
     };
     row.addClass("yui3-datatable-selected");\n',
'    if(!Y.selected[source]) {
	  Y.io("',Server,'", {data:{uri:source},
			      on:{success:setInfo},
			      arguments:{node:"#sourceinfo",add:add}
			     });
     }',
'    if(!Y.selected[target]) {
	  Y.io("',Server,'", {data:{uri:target},
			      on:{success:setInfo},
			      arguments:{node:"#targetinfo",add:add}
			     });
     }\n',
'    Y.selected[source] = true;
     Y.selected[target] = true;
     }'
 			  ]).

js_set_info -->
	js_function_decl(setInfo, [e,o,args],
			 \[
'     var node = Y.one(args.node);
      if(args.add) { node.append(o.responseText) }
      else { node.setContent(o.responseText) };\n',
'     node.all(".moretoggle").on("click", function(e) {
	   p = e.currentTarget.get("parentNode");
	   p.all(".moretoggle").toggleClass("hidden");
	   p.one(".morelist").toggleClass("hidden");
     });\n'
			  ]).

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
	Obj = json([source=json([uri=Source, label=SLabel]),
		    target=json([uri=Target, label=TLabel]),
		    relation=Relation
		   ]),
	(   has_map([Source, Target], _, Properties, test), %@TBD check in right graph
	    memberchk(relation(Relation), Properties)
	->  true
	;   Relation = ''
	),
 	mapping_data(As, Os).


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


%%	http_resource_context(+Request)
%
%	Returns	HTML with the Context in which the resource occurs

http_resource_context(Request) :-
	http_parameters(Request,
			[ uri(URI,
				 [description('URI from which we request the context')])
 			]),
	resource_label_text(URI, Label),
	resource_alternative_labels(URI, Label, Alt),
	resource_definition(URI, Def),
	resource_scope(URI, Scope),
	resource_tree(URI, Tree),
	related_resources(URI, Related),
	html_current_option(content_type(Type)),
	phrase(html_resource_context(URI, Label, Alt, Def, Scope, Tree, Related), HTML),
	format('Content-type: ~w~n~n', [Type]),
	print_html(HTML).

resource_definition(R, Def) :-
	(   rdf_has(R, skos:definition, Lit)
	->  literal_text(Lit, Def)
	;   Def = []
	).
resource_scope(R, Scope) :-
	(   rdf_has(R, skos:scopeNote, Lit)
	->  literal_text(Lit, Scope)
	;   Scope = []
	).
resource_alternative_labels(R, Label, Alt) :-
	findall(L, resource_label_text(R, L), Ls),
	delete(Ls, Label, Alt1),
 	sort(Alt1, Alt).

resource_label_text(R, L) :-
	rdf_label(R, Lit),
	literal_text(Lit, L).

%%	related_resources(+Resource, -Related)
%
%	Related resources are linked by skos:related to Resource.

related_resources(S, Rs) :-
	findall(R, skos_related(S, R), Rs0),
	sort(Rs0, Rs).

skos_related(R1, R2) :-
	rdf_has(R1, skos:related, R2).
skos_related(R2, R1) :-
	rdf_has(R2, skos:related, R1).

%%	resource_tree(+Resource, -Tree)
%
%	Tree contains the ancesestors and children from Resource.

resource_tree(R, Tree) :-
	Node = node(R, [hit], Children),
	rdf_equal(skos:broader, Rel),
	ancestor_tree(Node, Rel, Tree, []),
        children(R, Rel, Children, []).

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


%%	html_resource_context(+Resource, +Label, +AlternativeLabels,
%%	+Definition, +Scope, +Tree, +Related).
%
%	Emit html with info about a resource.

html_resource_context(R, Label, Alt, Def, Scope, Tree, Related) -->
	html(div(class('resource-info-content'),
		 [div(class(label), Label),
		  div(class(alt), \html_alt_labels(Alt)),
		  div(class(uri),
		      \rdf_link(R, [resource_format(plain)])),
		  \html_desc(Def, definition),
		  \html_desc(Scope, scope),
		  \html_resource_tree(Tree),
		  \html_related_list(Related)
		 ])).

html_alt_labels([]) --> !.
html_alt_labels(Alt) -->
        html_label_list(Alt).

html_label_list([L]) -->
	html(L).
html_label_list([L|Ls]) -->
	html([L, ', ']),
	html_label_list(Ls).

html_desc([], _) --> !.
html_desc(Txt, Name) -->
	html([div(class(hd), Name),
	      div(class('bd '+Name), Txt)
	     ]).

html_related_list([]) --> !.
html_related_list(Rs) -->
	html([div(class(hd), related),
	      div(class(bd),
		  ul(\html_resource_list(Rs, 3)))
	     ]).


%%	html_resource_list(+Resources, +Max)
%
%	Emit HTML with a list of resources.

html_resource_list(Rs, Max) -->
	{ length(Rs, N),
	  (   N > Max+2
	  ->  list_limit(Rs, Max, Visible, Rest)
	  ;   Visible = Rs,
	      Rest = []
	  )
	},
	html([ul(\html_resource_list(Visible)),
	      \html_more_list(Rest)
	     ]).

html_resource_list([]) --> !.
html_resource_list([R|Rs]) -->
	html(li(\html_resource(R))),
	html_resource_list(Rs).

html_more_list([]) --> !.
html_more_list(Rs) -->
	html(div([div(class(moretoggle), more),
		  div(class('morelist hidden'),
		      ul(\html_resource_list(Rs))),
		  div(class('moretoggle hidden'), less)
		 ])).

html_resource(node(R,_,_)) --> !,
	rdf_link(R, [resource_format(label)]).
html_resource(R) -->
	rdf_link(R, [resource_format(label)]).

%%	html_resource_tree(+Tree:node(uri,attr,children))
%
%       Tree to HTML.

html_resource_tree(node(_,_,[])) --> !.
html_resource_tree(Tree) -->
	html([div(class(hd), hierarchy),
	      div(class(bd),
		  ul(\html_tree(Tree)))
	     ]).

html_tree(node(R,[hit],Children)) -->
	html([li(class(hit), \html_resource(R)),
	      ul(\html_resource_list(Children, 3))
	     ]).
 html_tree(node(R,_,Children)) -->
	html([li(\html_resource(R)),
	      ul(\html_tree_children(Children))
	     ]).

html_tree_children([]) --> !.
html_tree_children([C|Cs]) -->
	html_tree(C),
	html_tree_children(Cs).
