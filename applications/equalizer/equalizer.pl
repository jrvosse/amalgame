:- module(equalizer, []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_request_value)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_host)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_litindex)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(settings)).
:- use_module(components(label)).

:- use_module(library(skos/vocabularies)).

% add local web directories from which static files are served.

:- prolog_load_context(directory, Dir),
   asserta(user:file_search_path(eq, Dir)).
:- asserta(user:file_search_path(css, eq(web))).
:- asserta(user:file_search_path(js, eq(web))).

% http handlers for this applications

:- http_handler(amalgame(equalizer), http_equalizer, []).
:- http_handler(amalgame(alignconcepts), http_align_concepts, []).

%%	http_equalizer(+Request)
%
%	HTTP handler for web page for interactive vocabulary alignment.

http_equalizer(_Request) :-
	vocabularies(Vocabularies),
	html_equalizer_page(Vocabularies).

vocabularies(Vocabularies) :-
 	bagof(Vocab-ConceptSchemes,
	      bagof(CS,
		    (	  rdf(Vocab, rdf:type, amalgame:'Vocabulary'),
			  rdf(Vocab, amalgame:hasConceptScheme, CS)
		    ),
		    ConceptSchemes),
	      Vocabularies),
	\+ Vocabularies = [].
vocabularies([all-ConceptSchemes]) :-
	findall(CS, rdf(CS, rdf:type, skos:'ConceptScheme'), ConceptSchemes).

html_equalizer_page(Vocabularies) :-
  	reply_html_page(cliopatria(default),
			[ title(['Align vocabularies'])
 			],
			[  \html_requires(css('equalizer.css')),
			   \html_requires('http://github.com/mattparker/yui3-gallery/raw/master/build/gallery-resize/assets/resize-core.css'),
			   \html_requires('http://github.com/mattparker/yui3-gallery/raw/master/build/gallery-resize/assets/skin/sam/resize-skin.css'),
			   \html_requires('http://yui.yahooapis.com/3.1.2/build/yui/yui-min.js'),
 			   div(id(header),
			       []),
 			   div(id(main),
			       [ div(id(vocabularies),
				      \html_vocabulary_select(Vocabularies)),
				 div(id(browsers),
				     [ div(id(source), []),
				       div(id(equalizer),
					   [ div(id('source-concept'), []),
					     div(id('align'),
						 input([id('align-button'), class(hidden), type(button), value('Align')])),
					     div(id('target-concept'), [])
					   ]),
				       div(id(target), [])
				     ])
			       ]),
 			   script(type('text/javascript'),
				  [ \yui_script
 				  ])
			]).

html_vocabulary_select(Vocabularies) -->
	html([ div(class(hd), h3('Select vocabularies')),
	       div(class(bd),
		   [ div(class('target-select'),
			 [ div([id('source-select'), class(selected)],
			       a(href('javascript:{}'), 'source')),
			   div([id('target-select')],
			       a(href('javascript:{}'),	'target'))
			 ]),
		     \html_vocabularies(Vocabularies)
		   ])
	     ]).

html_vocabularies([all-ConceptSchemes]) -->
	!,
	html(ul(\html_concept_schemes(ConceptSchemes))).
html_vocabularies(Vs) -->
	html_vocabulary_list(Vs).

html_vocabulary_list([]) --> !.
html_vocabulary_list([Voc-ConceptSchemes|Vs]) -->
	{ rdf_display_label(Voc, Label)
	},
	html([h4(title(Voc), Label),
	      ul(\html_concept_schemes(ConceptSchemes))
	     ]),
 	html_vocabulary_list(Vs).

html_concept_schemes([]) --> !.
html_concept_schemes([URI|Cs]) -->
	{ rdf_display_label(URI, Label)
	},
	html(li(title(URI), a(href('javascript:{}'), Label))),
	html_concept_schemes(Cs).


yui_script -->
	{ http_absolute_location(js('resourcelist.js'), ResourceList, []),
	  http_absolute_location(js('columnbrowser.js'), ColumnBrowser, []),
 	  http_location_by_id(http_concepts, Concepts),
	  http_location_by_id(http_concept_info, ConceptInfo),
	  http_location_by_id(http_align_concepts, Align)
  	},
	html(\[
'YUI({
    modules: {
	gallery: "gallery-2010.05.21-18-16",\n',
'	resourcelist: {
            fullpath: "',ResourceList,'",
            requires: ["node","event","widget"]
        },\n',
'	columnbrowser: {
            fullpath: "',ColumnBrowser,'",
            requires: ["node","event","widget","resourcelist","gallery-resize","gallery-value-change"]
        }
    }\n',
'}).use("io","datasource","overlay","columnbrowser",function(Y) {\n',

'var ds = new Y.DataSource.IO({source:""})
	      .plug(Y.Plugin.DataSourceJSONSchema, {
		    schema: {
    			resultListLocator: "results",
    			resultFields: ["id", "label", "hasNext", "matches", "scheme"],
			metaFields: {"totalNumberOfResults":"totalNumberOfResults"}
    		    }\n',
'    	      })
    	      .plug({fn:Y.Plugin.DataSourceCache, cfg:{max:20}});\n',

'var browsers = {}, align = {};
function createBrowser(target, voc) {
  if(!browsers[target]) {
      browsers[target] = new Y.mazzle.ColumnBrowser({\n',
'	    datasource: ds,
 	    maxNumberItems: 100,\n',
' 	    columns: [
	        {   request: "',Concepts,'",
		    label: "concept",
		    params: {type:"topconcept", parent:voc},\n',
'		    formatter: formatItem,
		    options: [
	                {value:"inscheme", label:"concepts in scheme"},
	                {value:"topconcept", selected:true, label: "top concepts"}
	            ]
	        },\n',
'	        {   request: "',Concepts,'",
		    params: {type:"child"},
		    options: [
			 {value:"descendant", label:"descendants"},
			 {value:"child", selected:true, label:"children"}
		    ],\n',
'		    repeat: true
 	        }
	    ]
	});\n',
'       browsers[target].on("itemSelect",  function(resource) {
	    align[target] = resource;
	    if(target=="target") { Y.one("#align-button").removeClass("hidden");}
 	    Y.io("',ConceptInfo, '", {
	        data: "concept="+encodeURIComponent(resource.id),
	        on: {  success: function(tid, o) {
		          Y.one("#"+target+"-concept").set("innerHTML", o.responseText);
		       }}
	    });
	})\n',
'	browsers[target].render("#"+target);
    }\n',
'  else {
	browsers[target].get("columns")[0].params.parent = voc;
	browsers[target]._getColumnData(0);
   }\n',
'browsers[target].titleNode.set("innerHTML", "<h3>"+voc+"</h3>");
};\n',

'function formatItem(oResource) {
        var label = oResource["label"],
            uri   = oResource["id"],
            value = (label&&!Y.Lang.isObject(label)) ? label : uri;\n',
'	var HTML = "";
	if(oResource.hasNext) { HTML += "<div class=\'more\'>&gt;</div>"; }
	HTML += "<div class=\'resourcelist-item-value\' title=\'"+uri+"\'>"+value+"</div>";
	return HTML;
};\n',

'Y.mazzle.ColumnBrowser.prototype.setTitle = function() {};\n',
'Y.mazzle.ColumnBrowser.prototype.setFooter = function() {};\n',

'Y.delegate("click", function(e) {
	   var target = e.currentTarget,
	       voc = target.get("title"),
	       b = Y.one("#target-select").hasClass("selected") ? "target" : "source";
           Y.all("#vocabularies li").removeClass(b);
	   target.addClass(b);\n',
'          createBrowser(b, voc);
      }, "#vocabularies", "li");\n',

'Y.one("#source-select").on("click", function(e) {
	 this.addClass("selected");
	 Y.one("#target-select").removeClass("selected");
});\n',
'Y.one("#target-select").on("click", function(e) {
	 this.addClass("selected");
	 Y.one("#source-select").removeClass("selected");
});\n',
'Y.one("#align-button").on("click", function(e) {
 	 if(align.source&&align.target) {
	      Y.io("',Align, '", {
	        data: "source="+encodeURIComponent(align.source.id)+
		      "&target="+encodeURIComponent(align.target.id),\n',
'	        on: {  success: function(tid, o) {
		          Y.one("#source-concept").addClass("aligned");
			  Y.one("#target-concept").addClass("aligned");
		       }
		    }
	     })\n',
'	 }
});\n',

'});\n'
	      ]).




%%	http_align_concepts(+Request)
%
%	API handler to align a source and target concept

http_align_concepts(Request) :-
	http_parameters(Request,
			[ source(Source,
				   [ description('Source concept of alignment')]),
			  target(Target,
				[description('Target concept of alignment')])
			]),
	rdf_transaction(
			rdf_assert(Source, skos:exactMatch, Target, test)),
 	reply_json(json([source=Source,
			 target=Target
			])).
