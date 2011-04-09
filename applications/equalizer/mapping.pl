:- module(mapping,
	  [ mapping_relation/2
	  ]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_host)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(settings)).
:- use_module(user(user_db)).
:- use_module(components(label)).

:- use_module(library(skos/vocabularies)).
:- use_module(library(amalgame/alignment_graph)).
:- use_module(library(amalgame/edoal)).
:- use_module(library(amalgame/map)).
:- use_module(library(ag_util)).

:- setting(rows_per_page, integer, 100,
	   'Maximum number of mappings shown.').

% http handlers for this applications

:- http_handler(amalgame(data/mapping), http_data_mapping, []).
:- http_handler(amalgame(data/mappingevaluate), http_data_mapping_evaluate, []).
:- http_handler(amalgame(private/resourcecontext), http_resource_context, []).

%%	mapping_relation(+Id, +URI)
%
%	Available mapping relations.

mapping_relation(exact, 'http://www.w3.org/2004/02/skos/core#exactMatch').
mapping_relation(close, 'http://www.w3.org/2004/02/skos/core#closeMatch').
mapping_relation(narrower, 'http://www.w3.org/2004/02/skos/core#narrowMatch').
mapping_relation(broader, 'http://www.w3.org/2004/02/skos/core#broadMatch').
mapping_relation(related, 'http://www.w3.org/2004/02/skos/core#related').
mapping_relation(unrelated, 'http://purl.org/vocabularies/amalgame#unrelated').
mapping_relation(unsure, 'http://purl.org/vocabularies/amalgame#unsure').

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
	node_type(URL, Type),
	e(URL, Mapping0),
 	maplist(mapping_label, Mapping0, Mapping1),
	sort_key(SortBy, SortKey),
	sort_by_arg(Mapping1, SortKey, MSorted),
 	list_offset(MSorted, Offset, MOffset),
	list_limit(MOffset, Limit, MLimit, _),
	mapping_data(MLimit, Mapping),
	mapping_statistics(Mapping0, Statistics),
	reply_json(json([url=URL,
 			 limit=Limit,
			 offset=Offset,
			 type=Type,
			 mapping=Mapping,
			 statistics=Statistics])).

node_type(URL, Type) :-
	rdf(URL, rdf:type, Class),
 	opm_type(Class, Type).

opm_type(C, mapping) :-
	rdf_equal(C, amalgame:'Mapping'),
	!.
opm_type(C, process) :-
	rdfs_subclass_of(C, opmv:'Process'),
	!.
opm_type(_, vocab).


sort_key(source, 2).
sort_key(target, 4).

mapping_label(align(S, T, Prov), align(S,SL,T,TL,Prov)) :-
	resource_label_text(S, SL),
	resource_label_text(T, TL).

mapping_data([], []).
mapping_data([Align|As], [json(Data)|Os]) :-
	Align = align(Source, SLabel, Target, TLabel, _Prov),
	Data0 = [source=json([uri=Source, label=SLabel]),
		 target=json([uri=Target, label=TLabel])
		],
 	(   has_map([Source, Target], _, Properties, test), %@TBD check in right graph
	    memberchk(relation(R), Properties)
	->  relation_label(R, RLabel),
	    Data = [relation=json([uri=R, label=RLabel])|Data0]
	;   Data = Data0
	),
 	mapping_data(As, Os).

relation_label(R, Label) :-
	mapping_relation(Label, R), !.
relation_label(R, R).

%%	mapping_statistics(+Mapping, -Stats)
%
%	Stats of mapping

mapping_statistics(Mappings, Stats) :-
	Stats = json([mappingcount=Total,
		      sourcecount=SN,
		      targetcount=TN
		     ]),
	maplist(align_source, Mappings, Ss0),
	maplist(align_target, Mappings, Ts0),
	sort(Ss0, Ss),
	sort(Ts0, Ts),
	length(Mappings, Total),
	length(Ss, SN),
	length(Ts, TN).

align_source(align(S,_,_), S).
align_target(align(_,T,_), T).



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
