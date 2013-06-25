:- module(eq_mapping,
	  [
	  ]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_json)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(settings)).
:- use_module(user(user_db)).
:- use_module(components(label)).
:- use_module(components(graphviz)).

:- use_module(library(amalgame/caching)).
:- use_module(library(amalgame/ag_evaluation)).
:- use_module(library(amalgame/edoal)).
:- use_module(library(amalgame/map)).
:- use_module(library(amalgame/expand_graph)).
:- use_module(library(amalgame/util)).

:- setting(rows_per_page, integer, 100,
	   'Maximum number of mappings shown.').

% http handlers for this applications

:- http_handler(amalgame(data/mapping), http_data_mapping, []).
:- http_handler(amalgame(data/evaluate), http_data_evaluate, []).
:- http_handler(amalgame(private/correspondence), http_correspondence, []).

%%	http_data_mapping(+Request)
%
%	Emit JSON object with mappings for a URL.

http_data_mapping(Request) :-
	setting(rows_per_page, RowsPerPage),
	http_parameters(Request,
			[ url(URL,
			      [description('URL of mapping or evaluation graph')]),
			  alignment(Strategy, [description('URL of strategy')]),
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

	expand_node(Strategy, URL, Mapping0),
	length(Mapping0, Count),
	augment_with_evaluation_relations(Strategy, URL, Mapping0, Augmented),
	maplist(mapping_label, Augmented, Labeled),
	sort_key(SortBy, SortKey),
	sort_by_arg(Labeled, SortKey, MSorted),
	list_offset(MSorted, Offset, MOffset),
	list_limit(MOffset, Limit, MLimit, _),
	mapping_data(MLimit, Mapping),

	reply_json(json([url=URL,
			 limit=Limit,
			 offset=Offset,
			 mapping=Mapping,
			 total=Count])).

sort_key(source, 2).
sort_key(target, 4).

mapping_label(align(S, T, Prov), align(S,SLabel, T,TLabel, Relation)) :-
	rdf_display_label(S, SL),
	rdf_display_label(T, TL),

	(   rdf_has(S, skos:notation, literal(Sn))
	->  format(atom(SLabel), '~w (~w)', [SL, Sn])
	;   format(atom(SLabel), '~w', [SL])
	),

	(   rdf_has(T, skos:notation, literal(Tn))
	->  format(atom(TLabel), '~w (~w)', [TL, Tn])
	;   format(atom(TLabel), '~w', [TL])
	),
	(   option(relation(Rel), Prov)
	->  relation_label(Rel, RLabel),
	    Relation = json([uri=Rel, label=RLabel])
	;   Relation = nil
	).

mapping_data([], []).
mapping_data([Align|As], [json(Data)|Os]) :-
	Align = align(Source, SLabel, Target, TLabel, Relation),
	Data = [source=json([uri=Source, label=SLabel]),
		target=json([uri=Target, label=TLabel]),
		relation = Relation
	       ],
	mapping_data(As, Os).

relation_label(R, Label) :-
	mapping_relation(Label, R), !.
relation_label(R, R).


%%	http_data_evaluate(+Request)
%
%	Accept/reject a mapping.

http_data_evaluate(Request) :-
	logged_on(User0, anonymous),
	user_property(User0, url(User)),
	http_parameters(Request,
			[  source(Source,
				  [description('Source of mapping')]),
			   target(Target,
				  [descript('Target of mapping')]),
			   relation(Relation,
				    [description('Relation between source and target')]),
			   mapping(Mapping,
				   [description('URI of mapping being evaluated')]),
			   alignment(Alignment,
				     [description('Alignment strategy graph')]),
			   comment(Comment,
				   [default(''),
				    description('Explanation of action')])
			]),

	evaluation_graph(Alignment, Mapping, Graph),
	flush_stats_cache(Graph, Alignment),
	(   has_correspondence(align(Source, Target, EvalProv), Graph)
	->  remove_correspondence(align(Source, Target, EvalProv), Graph)
	;   EvalProv = [] % Fixme, we want to use the prov from Mapping here!
	),
	now_xsd(Now),
	Options = [
		   graph(Graph),
		   prov([[method(manual_evaluation),
			 user(User),
			 date(Now),
			 comment(Comment),
			 relation(Relation)]|EvalProv])
		  ],
	debug(ag_expand, 'assert cell options: ~w', Options),
	mapping_relation(RLabel, Relation),
	assert_cell(Source, Target, Options),

	rdf_display_label(Source, SLabel),
	rdf_display_label(Target, TLabel),
	reply_json(json([source=json([uri=Source, label=SLabel]),
			 target=json([uri=Target, label=TLabel]),
			 relation=json([uri=Relation, label=RLabel])
			])).


%%	http_correspondence(+Request)
%
%	Returns	HTML with the Context in which the resource occurs

http_correspondence(Request) :-
	http_parameters(Request,
			[ source(Source,
				 [description('URI of the source concept')]),
			  target(Target,
				 [description('URI of the target concept')]),
			  mapping(Mapping,
				  [description('URI of the mapping')]),
			  alignment(Strategy, [description('URL of strategy')]),
			  allsource(AllSource,
				 [boolean, default(false),
				  description('Include all sources')]),
			  alltarget(AllTarget,
				 [boolean, default(false),
				  description('Include all target')])
			]),
	findall(R-L, mapping_relation(L, R), Relations),
	expand_node(Strategy, Mapping, Ms),
	(   AllSource
	->  A = align(Source,_,_)
	;   AllTarget
	->  A = align(_,Target,_)
	;   A = align(Source,Target,_)
	),
	findall(A, member(A, Ms), Cs),
	html_current_option(content_type(Type)),
	phrase(html_correspondences(Cs, Relations), HTML),
	format('Content-type: ~w~n~n', [Type]),
	print_html(HTML).

html_correspondences([], _) --> !.
html_correspondences([align(Source,Target,Evidence)|Cs], Relations) -->
	html_correspondence(Source, Target, Evidence, Relations),
	html_correspondences(Cs, Relations).

html_correspondence(Source, Target, Evidence, Relations) -->
	{ %option(relation(Relation), Prov, '')
	  Relation = ''
	},
	html([div(class('yui3-g'),
		  [ div(class('yui3-u-1-2'),
			\html_resource_context(Source, Evidence)),
		    div(class('yui3-u-1-2'),
			\html_resource_context(Target, Evidence))
		  ]),
	      div(class(evidences),
		  \html_evidences(Evidence, Source, Target)
		  ),
	      div(class(relations),
		  [ input([type(hidden), name(source), value(Source)]),
		    input([type(hidden), name(target), value(Target)]),
		    div(\html_relations(Relations, Relation)),
		    div(class(comment), ['because: ',
			 input([type(text), name(comment)], [])
			])
		  ])
	     ]).

html_evidences([],_,_) --> !.
html_evidences([E|Es],Source,Target) -->
	{ option(method(Method), E, ''),
	  option(graph(Graph), E, [])
	},
	html(div(class(evidence),
		 [ div(class(method), ['match: ', Method]),
		   div(class('graph yui3-g'),
		       [ div(class('source yui3-u-1-2'),
			     \html_evidence_graph(Graph, Source, 'LR')),
			 div(class('target yui3-u-1-2'),
			     \html_evidence_graph(Graph, Target, 'RL'))
		       ])
		 ])),
	html_evidences(Es,Source,Target).

html_evidence_graph([],_,_) --> !.
html_evidence_graph(Graph,Node,Layout) -->
	graphviz_graph(evidence_graph(Graph,Node),
		       [shape_hook(evidence_shape),
			graph_attributes([rankdir(Layout)])]).

evidence_graph(Graph, Node, NodeTriples) :-
	T = rdf(Node, _, _), % FIX me, this should include more triples
	findall(T, member(T, Graph), NodeTriples).


html_resource_context('',_) --> !.
html_resource_context(URI, Prov) -->
	{ rdf_display_label(URI, Label),
	  resource_alternative_labels(URI, Label, Prov, Alt),
	  resource_tree(URI, Tree),
	  related_resources(URI, Related),
	  image_examples(URI, Examples)
	},
	html(div(class('resource-info'),
		 [div(class(label), a([alt(URI), href(URI)], Label)),
		  div(class(alt), \html_alt_labels(Alt)),
		  \html_definition(URI),
		  \html_scope(URI),
		  \html_resource_tree(Tree),
		  \html_related_list(Related),
		  \html_image_examples(Examples)
		 ])).

html_relations([], _) --> !.
html_relations([Rel-Label|Rs], Active) -->
	{ (   Rel == Active
	  ->  Checked = checked
	  ;   Checked = ''
	  )
	},
	html(span(class(relation),
		 [input([type(radio), name(relation), value(Rel), Checked]),
		  ' ',
		  label(Label)
		 ])),
	html_relations(Rs, Active).

html_image_examples([]) --> !.
html_image_examples([E|Tail]) -->
	{
	 http_link_to_id(http_thumbnail, [uri(E)], Src)
	},
	html(div([class(example)],
		 [img([src(Src),height(150)])]
		)),
	html_image_examples(Tail).

resource_alternative_labels(R, Label, _Prov, Alt) :-
	findall(L, (rdf_label(R, L)), Ls),
	delete(Ls, Label, Alt0),
	sort(Alt0, Alt).

/*
	(   matching_label(R, Prov, MatchingLabel), selectchk(MatchingLabel, Alt1, Rest)
	->  Alt = [match(MatchingLabel)|Rest]
	;   Alt = Ls
	).


matching_label(S, Prov, MatchingLabel) :-
	option(graph(Graph), Prov),
	member(rdf(S,_P,O), Graph),
	literal_text(O, MatchingLabel).
*/


%%	related_resources(+Resource, -Related)
%
%	Related resources are linked by skos:related to Resource.

related_resources(S, Rs) :-
	findall(R, skos_related(S, R), Rs0),
	sort(Rs0, Rs).

image_examples(R, Es) :-
	% hack: assume non-literal examples to be image urls ...
	findall(E, ( rdf(R, skos:example, E),
		     \+ E =.. [literal|_]
		   ),
		List),
	sort(List, Es).

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



html_alt_labels([]) --> !.
html_alt_labels(Alt) -->
        html_label_list(Alt).

html_label_list([L]) -->
	html_label(L).
html_label_list([L|Ls]) -->
	html_label(L),
	html([', ']),
	html_label_list(Ls).

%html_label(match(L)) -->
%	html(span([class(match), style('font-weight: bold')], L)).

html_label(L) -->
	turtle_label(L).

html_definition(URI) -->
	{ rdf_has(URI, skos:definition, Lit),
	  literal_text(Lit, Txt)
	},
	!,
	html_item(definition, Txt).
html_definition(_) --> !.

html_scope(URI) -->
	{ rdf_has(URI, skos:scopeNote, Lit),
	  literal_text(Lit, Txt)
	},
	!,
	html_item(scope, Txt).
html_scope(_) --> !.


html_related_list([]) --> !.
html_related_list(Rs) -->
	html_item(related,
		  \html_resource_list(Rs, 3)).

html_item(Type, Body) -->
	html(div(class(Type),
		 [ div(class(hd), Type),
		   div(class(bd), Body)
		 ])).


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




%%	evidence_shape(+Resource, -Shape)
%
%	Defines graph node shape for different types of evidence
%	resources.

evidence_shape(literal(_),
	       [shape(box),
		style(filled),
		fontsize(10)]) :-
	!.
evidence_shape(_,
	       [fontsize(10)]).
