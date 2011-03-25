:- module(start_page,
	  [ html_start_page/0
	  ]).

:- use_module(library(http/http_dispatch)).
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
:- use_module(library(semweb/rdf_label)).
:- use_module(components(label)).
:- use_module(applications(concept_finder/concept_finder)).
:- use_module(library(amalgame/alignment)).

%%	html_start_page
%
%	Emit html page to start a new or load an existing alignment
%	project.

html_start_page :-
  	reply_html_page(equalizer(start),
			[ title(['Amalgame - projects'])
 			],
			[ \html_requires(css('eq_start_page.css')),
			  \html_requires('http://yui.yahooapis.com/combo?3.3.0/build/cssreset/reset-min.css&3.3.0/build/cssfonts/fonts-min.css&gallery-2011.02.23-19-01/build/gallery-node-accordion/assets/skins/sam/gallery-node-accordion.css'),
			  \html_requires('http://github.com/mattparker/yui3-gallery/raw/master/build/gallery-resize/assets/resize-core.css'),
			  \html_requires('http://github.com/mattparker/yui3-gallery/raw/master/build/gallery-resize/assets/skin/sam/resize-skin.css'),
 			  div(class('yui3-skin-sam'),
			      [ div(id(header), []),
				div(id(main),
				    [ h1('AMALGAME'),
				      div([id(content), class('yui3-accordion')],
					  [ \html_new_project,
					    \html_load_mapping,
					    \html_load_workflow
					  ])
				    ]),
				script(type('text/javascript'),
				       [ \yui_script
				       ])
			      ])
			]).

html_new_project -->
	html_acc_item(new, 'start new alignment project',
		      [ div(id(navigator), []),
			form(action(location_by_id(http_eq_new)),
			     [ \html_vocab_select(source),
			       \html_vocab_select(target),
			       \html_submit('Start')
			     ])
		      ]).

html_load_mapping -->
	{ findall(G, is_alignment_graph(G,_), Graphs)
	},
	html_acc_item(mapping, 'load mappings',
		      [ form(action(location_by_id(http_mapping_view)),
			     [ \html_alignment_table(Graphs),
 			       \html_submit('Start')
			     ])
		      ]).

html_load_workflow -->
	html_acc_item(workflow, 'load alignment workflow',
		      [tbd]).


html_vocab_select(Id) -->
	html(div(class(vocselect),
		 [input([id(Id+btn), class(select), type(button), value('set as '+Id)]),
		  input([type(text), autocomplete(off), class(label), disabled(true), id(Id+label), value('')]),
		  input([type(hidden), value(''), autocomplete(off), name(Id), id(Id)])
		 ])).

html_submit(Label) -->
	html(div(class(controls),
		 [ input([type(submit), autocomplete(off), class(start),
			  disabled(true), value(Label)])
		 ])).


%%	html_acc_item(+Id, +Label, +HTMLBody)
%
%	Emit html markup for a YUI3 accordion item.

html_acc_item(Id, Label, Body) -->
	html(div([class('yui3-accordion-item'), id(Id)],
		 [ div(class('yui3-accordion-item-hd'),
		       a([href('javascript:{}'), class('yui3-accordion-item-trigger')],
			   Label)),
		   div(class('yui3-accordion-item-bd'),
		       Body)
		 ])).

%%	html_alignment_table(+Graphs)
%
%	Emit HTML table with alignment graph properties.

html_alignment_table(Graphs) -->
	html(table([thead(tr(\html_alignment_head)),
		    tbody(\html_alignment_rows(Graphs))
		   ])).

html_alignment_head -->
	html([th([]),
	      th(name),
	      th(source),
	      th(target),
	      th(mappings)
	     ]).

html_alignment_rows([]) --> !.
html_alignment_rows([Graph|Gs]) -->
	{ rdf(Graph, amalgame:source, Source),
	  rdf(Graph, amalgame:target, Target),
 	  rdf(Graph, amalgame:count, MappingCount)
	},
	html(tr([td(input([type(checkbox), autocomplete(off), class(option), name(url), value(Graph)])),
		 td(\html_graph_name(Graph)),
		 td([\turtle_label(Source)]),
		 td([\turtle_label(Target)]),
		 td(class(count), MappingCount)
		])),
	html_alignment_rows(Gs).

html_graph_name(Graph) -->
	{ graph_label(Graph, Label)
	},
	html(Label).

graph_label(Graph, Label) :-
	rdf_label(Graph, Lit),
	literal_text(Lit, Label).
graph_label(Graph, Graph).


%%	yui_script
%
%	Emit YUI object.

yui_script -->
	{ http_absolute_location(js('resourcelist.js'), ResourceList, []),
	  http_absolute_location(js('columnbrowser.js'), ColumnBrowser, [])
  	},
	js_yui3([{modules:{gallery: 'gallery-2011.02.23-19-01',
 			   resourcelist:{fullpath:ResourceList},
			   columnbrowser:{fullpath:ColumnBrowser}
 			  }}
		],
		[node,event,widget,anim,datasource,
		 'gallery-node-accordion','gallery-resize','gallery-value-change',
   		 resourcelist,columnbrowser
 		],
		[ \js_navigator('#navigator', navigator),
		  \js_mappings,
 		  'Y.one("#content").plug(Y.Plugin.NodeAccordion,
					  {multiple:false,
					   fade:true,
					   anim:true,
					   effect:Y.Easing.backIn})'
  		]).

%%	js_navigator(+HTMLElement, +Id)
%
%	Emit javascript to initialize a Y.Mazzle.columnbrowser.

js_navigator(El, Id) -->
	js_navigator_format,
	js_navigator_datasource(ds),
	js_navigator_widget(Id, ds),
	js_yui3_render(Id, El),
	js_navigator_select(Id),
	html([Id,'.setTitle = function() {};\n',
	      Id,'.setFooter = function(o) {};\n',
	      'Y.on("click", valueSet, "#sourcebtn", this, "source");\n',
	      'Y.on("click", valueSet, "#targetbtn", this, "target");\n'
	     ]).

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

js_navigator_select(Id) -->
	js_function_decl(valueSet, [e, which],
		    \[
'   var selected =  ',Id,'.get("selected");\n',
'   if(selected) {
	Y.log(which+": "+selected.id);
	var uri = selected.id,
	    label = selected.label,
	    labelNode = Y.one("#"+which+"label"),
	    uriNode = Y.one("#"+which);\n',
'	    labelNode.set("value", label);
	    labelNode.addClass("filled");
	uriNode.set("value", uri);\n',
'       var nodes = Y.Node.all("#new .label.filled");
	if(nodes.size()==2) {
	    Y.one("#new .start").set("disabled", false);
	} else {
	    Y.one("#new .start").set("disabled", true);
        }
     }'
		   ]).


%%	js_mappings
%
%	Emit JavaScript to handle interaction of mapping list

js_mappings -->
	html(\[
'    Y.all("#mapping .option").on("click", function(e) {
	  e.target.toggleClass("selected");
	  var nodes = Y.Node.all("#mapping .selected");\n',
'	  if(nodes.size()>0) {
	     Y.one("#mapping .start").set("disabled", false);
          } else {
	     Y.one("#mapping .start").set("disabled", true);
	  }
     });\n'
	      ]).
