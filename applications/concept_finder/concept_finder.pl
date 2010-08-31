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
:- http_handler(amalgame(conceptschemes), http_concept_schemes, []).
:- http_handler(amalgame(concepts), http_concepts, []).
:- http_handler(amalgame(narrowerconcepts), http_narrower_concepts, []).

%%	http_concept_finder(+Request)
%
%	HTTP handler for web page with concept finder widget.

http_concept_finder(_Request) :-
  	reply_html_page(cliopatria(default),
			[ title(['Vocabulary browser'])
 			],
			[  \html_requires(css('columnbrowser.css')),
			   \html_requires('http://github.com/mattparker/yui3-gallery/raw/master/build/gallery-resize/assets/resize-core.css'),
			   \html_requires('http://yui.yahooapis.com/3.1.1/build/yui/yui-min.js'),
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
	  http_location_by_id(http_narrower_concepts, NarrowerConcepts)
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
'}).use("io","datasource","columnbrowser",function(Y) {\n',

'var ds = new Y.DataSource.IO({source:""})
	      .plug(Y.Plugin.DataSourceJSONSchema, {
		    schema: {
    			resultListLocator: "results",
    			resultFields: ["id", "label", "hasNarrower"]
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
	            ]\n',
'	        },\n',
'	        {   request: "',NarrowerConcepts,'"
	        }\n',
'	    ]
	});\n',
'function formatItem(oResource) {
        var label = oResource["label"],
            uri   = oResource["concept"],
            value = (label&&!Y.Lang.isObject(label)) ? label : uri;\n',
'	var HTML = "";
	if(oResource.hasNarrower) { HTML += "<div class=\'more\'>&gt;</div>"; }
	HTML += "<div class=\'resourcelist-item-value\' title=\'"+uri+"\'>"+value+"</div>";
	return HTML;
}\n',
'cf.render("#columnbrowser");\n',
'});\n'
	      ]).


:- json_object
        concept(id:atom, label:atom),
	concept(id:atom, label:atom, hasNarrower:boolean).

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
	ConceptScheme = concept(Concept, Label),
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
				   [description('Concept scheme from which we request the concepts')]),
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
	findall(TopConcept, scheme_concept(Type, Parent, Query, Concept, Label, HasNarrower), Cs),
	term_sort_by_arg(Cs, 2, Sorted),
	list_offset(Sorted, Offset, OffsetResults),
	list_limit(OffsetResults, Limit, LimitResults, _),
	prolog_to_json(LimitResults, JSONResults),
	reply_json(json([parent=Parent,
			 offset=Offset,
			 limit=Limit,
 			 results=JSONResults])).

scheme_concept(Type, ConceptScheme, Query, Concept, Label, HasNarrower) :-
	var(Query),
	!,
	scheme_concept_(Type, ConceptScheme, Concept),
	has_narrower(Concept, HasNarrower),
 	once(display_label(Concept, Label)).

scheme_concept_(inscheme, ConceptScheme, Concept) :- !,
	inscheme(ConceptScheme, Concept).
scheme_concept_(topconcept, ConceptScheme, Concept) :- !,
	topconcept(ConceptScheme, Concept).

inscheme(ConceptScheme, Concept) :-
	rdf(Concept, skos:inScheme, ConceptScheme).

topconcept(ConceptScheme, Concept) :-
	rdf(ConceptScheme, skos:hasTopConcept, Concept).
topconcept(ConceptScheme, Concept) :-
	rdf(Concept, skos:topConceptOf, ConceptScheme),
	\+ rdf(ConceptScheme, skos:hasTopConcept, Concept).

%%	http_top_concepts(+Request)
%
%       API handler to fetch top concepts

http_narrower_concepts(Request) :-
	http_parameters(Request,
			[ parent(Parent,
				   [description('Concepts form which we request narrower concepts')]),
 			  offset(Offset,
				[integer, default(0), description('Start of the results returned')]),
			  limit(Limit,
				[integer, default(20), description('maximum number of results returned')]),
			  query(Query,
				[optional(true), description('keyword query to filter the results by')])
			]),
	TopConcept = concept(Concept, Label, HasNarrower),
	findall(TopConcept, narrower_concept(Parent, Query, Concept, Label, HasNarrower), Cs),
	term_sort_by_arg(Cs, 2, Sorted),
	list_offset(Sorted, Offset, OffsetResults),
	list_limit(OffsetResults, Limit, LimitResults, _),
	prolog_to_json(LimitResults, JSONResults),
	reply_json(json([parent=Parent,
			 offset=Offset,
			 limit=Limit,
 			 results=JSONResults])).

narrower_concept(Parent, Query, Concept, Label, HasNarrower) :-
	var(Query),
	!,
	narrower_concept(Parent, Concept),
	has_narrower(Concept, HasNarrower),
 	once(display_label(Concept, Label)).

%%	narrower_concept(+Concept, -Narrower)
%
%	Narrower is related to Concept by skos:narrower or inversely by
%	skos:broader.

narrower_concept(Concept, Narrower) :-
	rdf_has(Concept, skos:narrower, Narrower).
narrower_concept(Concept, Narrower) :-
	rdf_has(Narrower, skos:broader, Concept),
	\+ rdf_has(Concept, skos:narrower, Narrower).

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

display_label(R, Label) :-
	rdf_label(R, Lit),
	!,
	literal_text(Lit, Label).
display_label(R, Label) :-
	rdfs_label(R, Label).
