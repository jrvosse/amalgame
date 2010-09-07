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
:- use_module(components(label)).

% add local web directories from which static files are served.

:- prolog_load_context(directory, Dir),
   asserta(user:file_search_path(cf, Dir)).
:- asserta(user:file_search_path(css, cf(web))).
:- asserta(user:file_search_path(js, cf(web))).

% http handlers for this applications

:- http_handler(amalgame(conceptfinder), http_concept_finder, []).
:- http_handler(amalgame(api/conceptschemes), http_concept_schemes, []).
:- http_handler(amalgame(api/concepts), http_concepts, []).
:- http_handler(amalgame(private/conceptinfo), http_concept_info, []).

%%	http_concept_finder(+Request)
%
%	HTTP handler for web page with concept finder widget.

http_concept_finder(_Request) :-
  	reply_html_page(cliopatria(default),
			[ title(['Vocabulary browser'])
 			],
			[  \html_requires(css('columnbrowser.css')),
			   \html_requires('http://github.com/mattparker/yui3-gallery/raw/master/build/gallery-resize/assets/resize-core.css'),
			   \html_requires('http://github.com/mattparker/yui3-gallery/raw/master/build/gallery-resize/assets/skin/sam/resize-skin.css'),
			   \html_requires('http://yui.yahooapis.com/3.1.2/build/yui/yui-min.js'),
 			   div(id(header),
			       []),
 			   div(id(main),
			       div(class('main-content'),
				   [ div([class('yui-skin-sam'), id(columnbrowser)], [])
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
    			resultFields: ["id", "label", "hasNext", "matches", "scheme"],
			metaFields: {"totalNumberOfResults":"totalNumberOfResults"}
    		    }\n',
'    	      })
    	      .plug({fn:Y.Plugin.DataSourceCache, cfg:{max:20}});\n',

'var cf = new Y.mazzle.ColumnBrowser({\n',
'	    datasource: ds,
	    maxNumberItems: 100,\n',
' 	    columns: [
	        {   request: "',ConceptSchemes,'",
		    label: "conceptscheme",
	            formatter: formatItem
	        },\n',
'	        {   request: "',Concepts,'",
		    label: "concept",
		    params: {type:"topconcept"},\n',
'		    options: [
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
};\n',
'cf.render("#columnbrowser");\n',

'cf.setTitle = function(resource) {
 	Y.io("',ConceptInfo, '", {
	    data: "concept="+encodeURIComponent(resource.id),
	    on: {  success: function(tid, o) {
		      cf.titleNode.set("innerHTML", o.responseText);
	           }
		}
	});\n',
'};\n',

'});\n'
	      ]).


%%	http_concept_schemes(+Request)
%
%       API handler to fetch concept schemes

http_concept_schemes(Request) :-
	http_parameters(Request,
			[
			  offset(Offset,
				[integer, default(0),
				 description('Start of the results returned')]),
			  limit(Limit,
				[integer, default(20),
				 description('maximum number of results returned')]),
			  query(Query,
				[optional(true),
				 description('keyword query to filter the results by')])
			]),
	ConceptScheme = concept(Concept, Label, true),
	findall(ConceptScheme, concept_scheme(Query, Concept, Label), Cs),
	length(Cs, Total),
	list_offset(Cs, Offset, OffsetResults),
	list_limit(OffsetResults, Limit, LimitResults, _),
	prolog_to_json(LimitResults, JSONResults),
	reply_json(json([offset=Offset,
			 limit=Limit,
			 totalNumberOfResults=Total,
 			 results=JSONResults])).

:- json_object
 	concept(id:atom, label:atom, hasNext:boolean).

concept_scheme(Query, C, Label) :-
	var(Query),
	!,
	rdf(C, rdf:type, skos:'ConceptScheme'),
	display_label(C, Label).
concept_scheme(Query, C, Label) :-
	rdf(C, rdf:type, skos:'ConceptScheme'),
	once(rdf(C, rdfs:label, literal(prefix(Query), Lit))),
	text_of_literal(Lit, Label).


%%	http_concepts(+Request)
%
%       API handler to fetch top concepts

http_concepts(Request) :-
	http_parameters(Request,
			[ parent(Parent,
				   [optional(true),
				    description('Concept or concept scheme from which we request the concepts')]),
			  type(Type,
			       [oneof(topconcept,inscheme,child,descendant,related),
				default(inscheme),
				description('Method to determine the concepts')]),
			  offset(Offset,
				[integer, default(0),
				 description('Start of the results returned')]),
			  limit(Limit,
				[integer, default(20),
				 description('maximum number of results returned')]),
			  query(Query,
				[optional(true),
				 description('keyword query to filter the results by')])
			]),
	C = concept(Concept, Label, HasNarrower),
	findall(C, concept(Type, Parent, Query, Concept, Label, HasNarrower), Cs0),
	sort(Cs0, Cs),
	term_sort_by_arg(Cs, 2, Sorted),
	length(Sorted, Total),
	list_offset(Sorted, Offset, OffsetResults),
	list_limit(OffsetResults, Limit, LimitResults, _),
	prolog_to_json(LimitResults, JSONResults),
	reply_json(json([parent=Parent,
			 offset=Offset,
			 limit=Limit,
			 totalNumberOfResults=Total,
 			 results=JSONResults])).

concept(Type, Parent, Query, Concept, Label, HasNarrower) :-
	var(Query),
	!,
	concept_(Type, Parent, Concept),
	has_narrower(Concept, HasNarrower),
 	display_label(Concept, Label).
concept(Type, Parent, Query, Concept, Label, HasNarrower) :-
	rdf_has(Concept, rdfs:label, literal(prefix(Query), Lit)),
	(   Type = descendant
	->  once(same_scheme(Parent, Concept))
	;   true
	),
 	once(concept_(Type, Parent, Concept)),
	text_of_literal(Lit, Label),
	has_narrower(Concept, HasNarrower).

concept_(inscheme, ConceptScheme, Concept) :- !,
	inscheme(ConceptScheme, Concept).
concept_(topconcept, ConceptScheme, Concept) :- !,
	top_concept(ConceptScheme, Concept).
concept_(child, Parent, Concept) :-
	narrower_concept(Parent, Concept).
concept_(descendant, Parent, Concept) :-
	descendant(Parent, Concept).
concept_(related, Parent, Concept) :-
	related_concept(Parent, Concept).

same_scheme(C1, C2) :-
	rdf(C1, skos:inScheme, Scheme),
	rdf(C2, skos:inScheme, Scheme).

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

%%	descendant(?Concept, ?Descendant)
%
%	Descendant is a child of Concept or recursively of its children

descendant(Concept, Descendant) :-
	var(Descendant),
	!,
	narrower_concept(Concept, Narrower),
	(   Descendant = Narrower
	;   descendant(Narrower, Descendant)
	).
descendant(Concept, Descendant) :-
%	var(Concept),
%	!,
	parent_of(Concept, Descendant).
/*
descendant(Concept, Descendant) :-
	tree_index(Concept, CStart, CEnd),
	tree_index(Descendant, DStart, DEnd),
	DStart > CStart,
	DEnd =< CEnd.
*/
parent_of(Concept, Concept). % really?
parent_of(Concept, Descendant) :-
	narrower_concept(Broader, Descendant),
	parent_of(Concept, Broader).

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


%%	http_concept_info(+Request)
%
%       API handler to fetch info about a URI.
%
%       @TBD support for language tags

http_concept_info(Request) :-
	http_parameters(Request,
			[  concept(C,
			       [description('Concept to request info about')])
 			]),
	display_label(C, Label),
	skos_description(C, Desc),
	skos_alt_labels(C, AltLabels0),
	delete(AltLabels0, Label, AltLabels),
	skos_related_concepts(C, Related),
	format('Content-type: text/html~n~n'),
	phrase(html(\html_info_snippet(C, Label, Desc, AltLabels, Related)), HTML),
	print_html(HTML).

skos_description(C, Desc) :-
	(   rdf_has(C, skos:scopeNote, Lit)
	->  literal_text(Lit, Desc)
	;   Desc = ''
	).
skos_alt_labels(C, AltLabels) :-
 	findall(AL, ( rdf_has(C, skos:altLabel, Lit),
		      literal_text(Lit, AL)
		    ),
		AltLabels0),
	sort(AltLabels0, AltLabels).
skos_related_concepts(C, Related) :-
	Concept = concept(R, Label),
 	findall(Concept, ( skos_related(C, R),
			   display_label(R, Label)
		    ),
		Related).

skos_related(C, R) :-
	rdf_has(C, skos:related, R).
skos_related(C, R) :-
	rdf_has(R, skos:related, C),
	\+ rdf_has(C, skos:related, R).

html_info_snippet(URI, Label, Desc, AltLabels, Related) -->
	html(div(class(infobox),
		 [ h3([Label,
		       \html_label_list(AltLabels)
		      ]),
		   div(class(uri), URI),
		   div(class(desc), Desc),
		   \html_related_list(Related)
		 ])).

html_label_list([]) --> !.
html_label_list(Ls) -->
	html(span(class(altlabels),
		  [ ' (',
		    \html_label_list_(Ls),
		    ')'
		  ])).

html_label_list_([L]) --> !,
	html(span(class(label), L)).
html_label_list_([L|Ls]) -->
	html(span(class(label), [L,', '])),
	html_label_list_(Ls).

html_related_list([]) --> !.
html_related_list(Cs) -->
	html(div(class(related),
		 [ 'related: ',
		   \html_concept_list(Cs)
		 ])).

html_concept_list([concept(URI, Label)]) --> !,
	html(span([class(concept), title(URI)], Label)).
html_concept_list([concept(URI, Label)|Cs]) -->
	html(span([class(concept), title(URI)], [Label, ', '])),
	html_concept_list(Cs).


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
	once(rdfs_label(R, Label)).


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
