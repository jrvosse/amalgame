:- module(concept_finder, []).

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

% add local web directories from which static files are served.

:- prolog_load_context(directory, Dir),
   asserta(user:file_search_path(cf, Dir)).
:- asserta(user:file_search_path(css, cf(web))).
:- asserta(user:file_search_path(js, cf(web))).

% http handlers for this applications

:- http_handler(amalgame(conceptfinder), http_concept_finder, []).
:- http_handler(amalgame(api/conceptschemes), http_concept_schemes, []).
:- http_handler(amalgame(api/concepts), http_concepts, []).
:- http_handler(amalgame(api/conceptInfo), http_concept_info, []).

%%	http_concept_finder(+Request)
%
%	HTTP handler for web page with concept finder widget.

http_concept_finder(_Request) :-
  	reply_html_page(cliopatria(default),
			[ title(['Vocabulary browser'])
 			],
			[  \html_requires(css('columnbrowser.css')),
			   \html_requires('http://github.com/mattparker/yui3-gallery/raw/master/build/gallery-resize/assets/resize-core.css'),
			   \html_requires('http://yui.yahooapis.com/3.1.2/build/yui/yui-min.js'),
 			   div(id(header),
			       []),
 			   div(id(main),
			       div(class('main-content'),
				   [ div(id(columnbrowser), [])
				   ])),
			   script(type('text/javascript'),
				  [ \yui_script
 				  ])
			]).

yui_script -->
	{ http_absolute_location(js('resourcelist.js'), ResourceList, []),
	  http_absolute_location(js('columnbrowser.js'), ColumnBrowser, []),
	  http_location_by_id(http_concept_schemes, ConceptSchemes),
	  http_location_by_id(http_concepts, Concepts),
	  http_location_by_id(http_concept_info, ConceptInfo)
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
    			resultFields: ["id", "label", "hasNext"]
    		    }\n',
'    	      })
    	      .plug({fn:Y.Plugin.DataSourceCache, cfg:{max:20}});\n',

'var cf = new Y.mazzle.ColumnBrowser({\n',
'	    datasource: ds,
	    maxNumberItems: 100,\n',
' 	    columns: [

	        {   request: "',ConceptSchemes,'",
	            formatter: formatItem
	        },\n',
'	        {   request: "',Concepts,'",
		    options: [
	                {value:"inscheme", label:"concepts in scheme"},
	                {value:"topconcept", label: "top concepts"}
	            ]
	        },\n',
'	        {   request: "',Concepts,'",
		    params: {type:"narrower"},
		    repeat: true
 	        }\n',
'	    ]
	});\n',
'function formatItem(oResource) {
        var label = oResource["label"],
            uri   = oResource["id"],
            value = (label&&!Y.Lang.isObject(label)) ? label : uri;\n',
'	var HTML = "";
	if(oResource.hasNext) { HTML += "<div class=\'more\'>&gt;</div>"; }
	HTML += "<div class=\'resourcelist-item-value\' title=\'"+uri+"\'>"+value+"</div>";
	return HTML;
}\n',
'cf.render("#columnbrowser");\n',

'myNS = YUI.namespace("resource.data");\n',
'cf.setTitle = function(resource) {
	Y.log("get info for: "+resource.id);
	Y.Get.script("',ConceptInfo,'?uri="+resource.id+"&callback=myNS.setInfo", {
	    context: Y
	});
};\n',

'myNS.setInfo = function(info) {
	var props = info.properties,
	    HTML = "<div class=\'infobox\'>"+
		   "<h3>"+info.label+"</h3>"+
		   "<div class=\'uri\'>"+info.uri+"</div>"+
		   "<table class=\'props\'>";\n',
'	for(i=0; i<props.length; i++) {
	    HTML += "<tr><td>"+props[i].plabel+"</td>"+
		    "<td>"+props[i].vlabel+"</td></tr>";
	}\n',
'	HTML += "</table></div>";
	cf.titleNode.set("innerHTML", HTML);
 };\n',

'});\n'
	      ]).


:- json_object
 	concept(id:atom, label:atom, hasNext:boolean).

%%	http_concept_schemes(+Request)
%
%       API handler to fetch concept schemes

http_concept_schemes(Request) :-
	http_parameters(Request,
			[ parent(Parent,
				 [optional(true), description('Named graph in which the concept schemes occur')]),
			  offset(Offset,
				[integer, default(0),description('Start of the results returned')]),
			  limit(Limit,
				[integer, default(20), description('maximum number of results returned')]),
			  query(Query,
				[optional(true), description('keyword query to filter the results by')])
			]),
	ConceptScheme = concept(Concept, Label, true),
	findall(ConceptScheme, concept_scheme(Parent, Query, Concept, Label), Cs),
	list_offset(Cs, Offset, OffsetResults),
	list_limit(OffsetResults, Limit, LimitResults, _),
	prolog_to_json(LimitResults, JSONResults),
	reply_json(json([offset=Offset,
			 limit=Limit,
 			 results=JSONResults])).

concept_scheme(Parent, Query, C, Label) :-
	var(Query),
	!,
	rdf(C, rdf:type, skos:'ConceptScheme', Parent),
	once(display_label(C, Label)).


%%	http_concepts(+Request)
%
%       API handler to fetch top concepts

http_concepts(Request) :-
	http_parameters(Request,
			[ parent(Parent,
				   [description('Concept or concept scheme from which we request the concepts')]),
			  type(Type,
			       [default(topconcept), description('Method to determine the concepts')]),
			  offset(Offset,
				[integer, default(0), description('Start of the results returned')]),
			  limit(Limit,
				[integer, default(20), description('maximum number of results returned')]),
			  query(Query,
				[optional(true), description('keyword query to filter the results by')])
			]),
	TopConcept = concept(Concept, Label, HasNarrower),
	findall(TopConcept, concept(Type, Parent, Query, Concept, Label, HasNarrower), Cs),
	term_sort_by_arg(Cs, 2, Sorted),
	list_offset(Sorted, Offset, OffsetResults),
	list_limit(OffsetResults, Limit, LimitResults, _),
	prolog_to_json(LimitResults, JSONResults),
	reply_json(json([parent=Parent,
			 offset=Offset,
			 limit=Limit,
 			 results=JSONResults])).

concept(Type, Parent, Query, Concept, Label, HasNarrower) :-
	var(Query),
	!,
	concept_(Type, Parent, Concept),
	has_narrower(Concept, HasNarrower),
 	once(display_label(Concept, Label)).

concept_(inscheme, ConceptScheme, Concept) :- !,
	inscheme(ConceptScheme, Concept).
concept_(topconcept, ConceptScheme, Concept) :- !,
	top_concept(ConceptScheme, Concept).
concept_(narrower, Parent, Concept) :-
	narrower_concept(Parent, Concept).
concept_(related, Parent, Concept) :-
	related_concept(Parent, Concept).

%%	inscheme(+ConceptScheme, -Concept)
%
%	True if Concept is contained in a skos:ConceptScheme by
%	skos:inScheme.

inscheme(ConceptScheme, Concept) :-
	rdf(Concept, skos:inScheme, ConceptScheme).

%%	top_concept(+ConceptScheme, -Concept)
%
%	True if Concept is a skos:hasTopConcept of ConceptScheme, or
%	inversely by skos:topConceptOf

top_concept(ConceptScheme, Concept) :-
	rdf(ConceptScheme, skos:hasTopConcept, Concept).
top_concept(ConceptScheme, Concept) :-
	rdf(Concept, skos:topConceptOf, ConceptScheme),
	\+ rdf(ConceptScheme, skos:hasTopConcept, Concept).

%%	narrower_concept(+Concept, -Narrower)
%
%	True if Narrower is related to Concept by skos:narrower or
%	inversely by skos:broader.

narrower_concept(Concept, Narrower) :-
	rdf_has(Concept, skos:narrower, Narrower).
narrower_concept(Concept, Narrower) :-
	rdf_has(Narrower, skos:broader, Concept),
	\+ rdf_has(Concept, skos:narrower, Narrower).

%%	related_concept(+Concept, -Related)
%
%	True if Related is related to Concept by skos:related.

related_concept(Concept, Related) :-
	rdf_has(Concept, skos:related, Related).
related_concept(Concept, Related) :-
	rdf_has(Related, skos:related, Concept),
	\+ rdf_has(Concept, skos:related, Related).

%%	has_narrower(+Concept, -Boolean)
%
%	Boolean is true when concept has a skos:narrower concept.

has_narrower(Concept, true) :-
 	rdf_has(Concept, skos:narrower, _),
	!.
has_narrower(Concept, true) :-
	rdf_has(_, skos:broader, Concept),
	!.
has_narrower(_, false).

:- json_object
        literal(literal:atom),
	literal(literal:_),
        type(type:atom, text:atom),
        lang(lang:atom, text:atom),
        prop(property:atom, plabel:atom, value:_, vlabel:atom).

:- rdf_meta
        skos_info_property(r).

%%	http_concept_info(+Request)
%
%       API handler to fetch info about a URI.

http_concept_info(Request) :-
	http_parameters(Request,
			[  uri(URI,
			       [uri, description('Resource to request info about')]),
			   callback(Callback,
				    [optional(true),
				     description('Callback function in which the response is wrapped')])
			]),
	display_label(URI, Label),
 	findall(prop(P,PL,V,VL), concept_info(URI, P, PL, V, VL), Properties),
	prolog_to_json(Properties, JSONProps),
	JSON = json([uri(URI), label(Label), properties(JSONProps)]),
	(   nonvar(Callback)
	->  reply_jsonp(JSON, Callback)
 	;   reply_json(JSON)
	).

concept_info(R, P, PL, V, VL) :-
	skos_info_property(P),
	(   rdf_has(R,P,V)
	;   rdf_has(V,P,R),
	    \+ rdf_has(R,P,V)
	),
	once(display_label(P, PL)),
	once(display_label(V, VL)).

skos_info_property(skos:prefLabel).
skos_info_property(skos:altLabel).
skos_info_property(skos:scopeNote).
skos_info_property(skos:notation).
skos_info_property(skos:related).

		 /*******************************
		 *	     UTILILIES          *
		 *******************************/

%%	terms_sort_by_arg(+ListOfTerms, +N, -SortedTerms)
%
%	Sorts ListOfTerms by the nth argument of each term.

term_sort_by_arg(List, Arg, Sorted) :-
	maplist(arg_key(Arg), List, Pairs),
	keysort(Pairs, Sorted0),
	pairs_values(Sorted0, Sorted).

arg_key(N, Term, Key-Term) :-
	arg(N, Term, Key).

%%	list_offset(+List, +N, -SmallerList)
%
%	SmallerList starts at the nth element of List.

list_offset(L, N, []) :-
	length(L, Length),
	Length < N,
	!.
list_offset(L, N, L1) :-
	list_offset_(L, N, L1).

list_offset_(L, 0, L) :- !.
list_offset_([_|T], N, Rest) :-
	N1 is N-1,
	list_offset_(T, N1, Rest).

%%	list_limit(+List, +N, -SmallerList, -Rest)
%
%	SmallerList ends at the nth element of List.

list_limit(L, N, L, []) :-
	length(L, Length),
	Length < N,
	!.
list_limit(L, N, L1, Rest) :-
	list_limit_(L, N, L1, Rest).

list_limit_(Rest, 0, [], Rest) :- !.
list_limit_([H|T], N, [H|T1], Rest) :-
	N1 is N-1,
	list_limit_(T, N1, T1, Rest).

%%	display_label(+Resource, -Txt)
%
%	Txt is a label of Resource suited for display.

display_label(literal(Lit), Label) :-
	!,
	literal_text(literal(Lit), Label).
display_label(R, Label) :-
	rdf_label(R, Lit),
	!,
	literal_text(Lit, Label).
display_label(R, Label) :-
	rdfs_label(R, Label).


%%	reply_jsonp(+JSON, +Callback)
%
%	Output an html script node, where JSON is embedded in a
%	javascript funtion.

reply_jsonp(JSON, Callback) :-
	with_output_to(string(JSONString),
		       json_write(current_output, JSON, [])),
	format('Content-type: text/javascript~n~n'),
	phrase(html([Callback,'(',JSONString,')']), HTML),
	print_html(HTML).
