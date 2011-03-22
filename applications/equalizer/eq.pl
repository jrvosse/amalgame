:- module(eq, []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_request_value)).
:- use_module(library(http/http_host)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_json)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(yui3)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_litindex)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(settings)).
:- use_module(components(label)).
:- use_module(library(skos/vocabularies)).

:- use_module(applications(concept_finder/concept_finder)).
:- use_module(applications(mappingview/mappingview)).

% add local web directories from which static files are served.

:- prolog_load_context(directory, Dir),
   asserta(user:file_search_path(eq, Dir)).
:- asserta(user:file_search_path(css, eq(web))).
:- asserta(user:file_search_path(js, eq(web))).

% http handlers for this applications

:- http_handler(amalgame(eq), http_equalizer, []).

%%	http_equalizer(+Request)
%
%	HTTP handler for web page for interactive vocabulary alignment.

http_equalizer(Request) :-
	http_parameters(Request,
		     [ workflow(Workflow,
				[optional(true),
				 description('URI of a workflow')])
		     ]),
 	html_page(Workflow).


		 /*******************************
		 *	      HTML		*
		 *******************************/

%%	html_page(+Worklow)
%
%	Emit html page with layout for the appliation.

html_page(Workflow) :-
  	reply_html_page(equalizer(main),
			[ title(['Align vocabularies'])
 			],
			[ \html_requires(css('equalizer.css')),
			  \html_requires('http://github.com/mattparker/yui3-gallery/raw/master/build/gallery-resize/assets/resize-core.css'),
			  \html_requires('http://github.com/mattparker/yui3-gallery/raw/master/build/gallery-resize/assets/skin/sam/resize-skin.css'),
 			  div(id(header),
			      []),
			  div(id(main),
			      [ div(id(navigator), []),
 				div(id(workflow), []),
				div(id(mappings), [])
			      ]),
 			  script(type('text/javascript'),
				 [ \yui_script(Workflow)
				 ])
			]).

%%	yui_script(+Workflow)
%
%	Emit YUI object.

yui_script(Workflow) -->
	{ http_absolute_location(js('resourcelist.js'), ResourceList, []),
	  http_absolute_location(js('columnbrowser.js'), ColumnBrowser, [])
  	},
	js_yui3([{modules:{gallery: 'gallery-2010.05.21-18-16',
			   resourcelist:{fullpath:ResourceList},
			   columnbrowser:{fullpath:ColumnBrowser}
 			  }}
		],
		[node,event,widget,datasource,
		 'gallery-resize','gallery-value-change',
   		 resourcelist,columnbrowser
 		],
		[ \js_navigator('#navigator', navigator),
		  \js_workflow(Workflow, '#workflow', workflow),
		  \js_results('#results', results)
 		]).

%%	js_navigator(+HTMLElement, +Id)
%
%	Emit javascript to initialize a Y.Mazzle.columnbrowser.

js_navigator(El, Id) -->
	js_navigator_format,
	js_navigator_datasource(ds),
	js_navigator_widget(Id, ds),
	js_yui3_render(Id, El),
	js_navigator_extend(Id).

js_navigator_datasource(Id) -->
	js_new(Id,
	       'Y.DataSource.IO'({source:''})),
	js_yui3_plug(Id,
		'Y.Plugin.DataSourceJSONSchema',
		{ schema:
		  { resultListLocator: results,
		    resultFields: [id, label, hasNext, matches, scheme],
		    metaFields: {totalNumberOfResults:totalNumberOfResults}
		  }
		}),
	js_yui3_plug(Id,
		'Y.Plugin.DataSourceCache',
		{ cfg:{max:20}}).

js_navigator_widget(Id, DataSource) -->
	{ http_location_by_id(http_concept_schemes, ConceptSchemes),
	  http_location_by_id(http_concepts, Concepts)
	  %http_location_by_id(http_concept_info, ConceptInfo)
	},
	js_new(Id,
	       'Y.mazzle.ColumnBrowser'(
		{ datasource: symbol(DataSource),
		  title:navigator,
		  maxNumberItems: 100,
		  columns:
		  [ { request: ConceptSchemes,
		      label: conceptscheme,
		      formatter: symbol(formatItem)
		    },
		  { request: Concepts,
		      label: concept,
		      params:
		      {type:topconcept,
		       parent:voc
		      },
 		      options:
		      [ {value:inscheme,
			 label:'concepts in scheme'
			},
	                {value:topconcept,
			 selected:true,
			 label: 'top concepts'
			}
		      ]
		    },
		    { request: Concepts,
		      params: {type:child},
		      options:
		      [ {value:descendant,
			 label:descendants
			},
			{value:child,
			 selected:true,
			 label:children
			}
		      ],
		      repeat: true
		    }
		  ]
		})).

js_navigator_format -->
	js_function_decl(formatItem, [o],
			 \[
'    var label = o["label"],
	 uri   = o["id"],
	 value = (label&&!Y.Lang.isObject(label)) ? label : uri;\n',
'    var HTML = "";
	 if(o.hasNext) { HTML += "<div class=\'more\'>&gt;</div>"; }
	 HTML += "<div class=\'resourcelist-item-value\' title=\'"+uri+"\'>"+value+"</div>";
	 return HTML;'
			  ]).

js_navigator_extend(Id) -->
	html(\[Id,'.setTitle = function() {};\n',
	       Id,'.setFooter = function(o) {this.statusNode.one(".value").setContent(o.label);};\n',
	       Id,'.statusNode.setContent("<input class=add type=submit value=add><span class=value></span>");\n',
	       'Y.one(".add").on("click", function(o) {Y.log(navigator._selectedItem.id)});\n'
	      ]).


%%	js_workflow(+Workflow, El, Id)
%
%	Emit javascript to initialize an Y.mazzle.Workflow

js_workflow(_Workflow, _El, _Id) --> !.

%%	js_results
%
%	Emit javascript to initialize an Y.mazzle.MappingView

js_results(_El, _Id) --> !.



		 /*******************************
		 *	  data handlers		*
		 *******************************/
