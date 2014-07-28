:- module(ag_mapping,
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
:- use_module(library(amalgame/ag_provenance)).
:- use_module(library(amalgame/ag_stats)).
:- use_module(library(amalgame/edoal)).
:- use_module(library(amalgame/map)).
:- use_module(library(amalgame/expand_graph)).
:- use_module(library(amalgame/util)).

:- setting(amalgame:rows_per_page, integer, 100,
	   'Maximum number of mappings shown.').

% http handlers for this applications

:- http_handler(amalgame(data/mapping), http_data_mapping, []).
:- http_handler(amalgame(data/evaluate), http_data_evaluate, []).
:- http_handler(amalgame(api/correspondence), http_correspondence, []).

%%	http_data_mapping(+Request)
%
%	Emit JSON object with mappings for a URL.

http_data_mapping(Request) :-
	setting(amalgame:rows_per_page, RowsPerPage),
	http_parameters(Request,
			[ url(URL,
			      [description('URL of mapping or evaluation graph')]),
			  strategy(Strategy, [description('URL of strategy')]),
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
	augment_relations(Strategy, URL, Mapping0, Augmented, []),
	maplist(mapping_label, Augmented, Labeled),
	sort_key(SortBy, SortKey),
	sort_by_arg(Labeled, SortKey, MSorted),
	list_offset(MSorted, Offset, MOffset),
	list_limit(MOffset, Limit, MLimit, _),
	mapping_data(MLimit, Mapping),
	node_stats(Strategy, URL, Stats),
	reply_json(jsondict{url:URL,
			    limit:Limit,
			    offset:Offset,
			    stats:Stats,
			    total:Count,
			    mapping:Mapping
			   }).

sort_key(source, 2).
sort_key(target, 4).

mapping_label(align(S, T, Prov), align(S,SLabel, T,TLabel, Relation)) :-
	notation_ish(S, SLabel),
	notation_ish(T, TLabel),
	append(Prov, FlatProv),
	(   option(relation(Rel), FlatProv)
	->  relation_label(Rel, RLabel),
	    Relation = json([uri=Rel, label=RLabel])
	;   Relation = null
	).

%%	notation_ish(Concept, NotationIsh) is det.
%
%	Unify NotationIsh with a label extend by (notation).
%	For notation, use the skos:notation or dc/dcterms:identifier
notation_ish(Concept, NotationIsh) :-
	rdf_display_label(Concept, Label),
	(   (rdf(Concept, skos:notation, N)
	    ;	rdf_has(Concept, skos:notation, N)
	    ;	rdf_has(Concept, dc:identifier, N)
	    )
	->  literal_text(N, LT),
	    format(atom(NotationIsh), '~w (~w)', [Label, LT])
	;   NotationIsh = Label
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
	http_parameters(Request,
			[  values(Values,
				  [description('JSON object with the new source/target pair values')]),
			   originals(Originals,
				     [description('JSON object with original source/target pair')]),
			   relation(Relation,
				    [description('Relation between source and target')]),
			   mapping(Mapping,
				   [description('URI of mapping being evaluated')]),
			   strategy(Strategy,
				    [description('Alignment strategy graph')]),
			   comment(Comment,
				   [default(''),
				    description('Explanation of action')]),
			   mode(Mode,
				[default(one), oneof([all,one]),
				description('Apply to one or all correspondences of this mapping')])
			]),
	evaluation_graph(Strategy, Mapping, Graph),
	process_entity(EvalProcess,  Graph),
	flush_refs_cache(Strategy),           % to recompute all reference stats
	flush_stats_cache(Graph, Strategy),   % to recompute G's basic stats
	flush_expand_cache(EvalProcess, Strategy),  % evaluation graph cache is now outdated
	user_property(User0, url(User)),

	my_atom_json_dict(Values,    V, []),
	my_atom_json_dict(Originals, O, []),

	Options = [
	    user(User),
	    evaluation_graph(Graph),
	    strategy(Strategy),
	    mapping(Mapping),
	    comment(Comment),
	    relation(Relation),
	    mode(Mode)
	],


	(   V=O
	->  original_concepts_assessment(V, Options)
	;   WithdrawOptions = [
		user(User),
		evaluation_graph(Graph),
		strategy(Strategy),
		mapping(Mapping),
		comment(WithDrawComment),
		relation(Unrelated)
	    ],
	    format(atom(WithDrawComment), 'Overruled by ~p ~p ~p', [V.source, Relation, V.target]),
	    rdf_equal(evaluator:unrelated, Unrelated),
	    new_concepts_assessment(V, O, Options, WithdrawOptions)
	).

original_concepts_assessment(V, Options) :-
	(   option(mode(one), Options)
	->  assert_relation(V.source, V.target, Options)
	;   option(mapping(Mapping), Options),
	    assert_relations(Mapping, Options)
	),

	option(relation(Relation), Options),
	mapping_relation(RLabel, Relation),
	rdf_display_label(V.source, SLabel),
	rdf_display_label(V.target, TLabel),
	reply_json(json([source=json([uri=V.source, label=SLabel]),
			 target=json([uri=V.target, label=TLabel]),
			 relation=json([uri=Relation, label=RLabel])
			])).


new_concepts_assessment(V, O, Options, WithdrawOptions) :-
	assert_relation(O.source, O.target, WithdrawOptions),
	assert_relation(V.source, V.target, Options),
	option(relation(Relation), WithdrawOptions),
	mapping_relation(RLabel, Relation),
	rdf_display_label(O.source, SLabel),
	rdf_display_label(O.target, TLabel),
	reply_json(json([source=json([uri=V.source, label=SLabel]),
			 target=json([uri=V.target, label=TLabel]),
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
			  strategy(Strategy, [description('URL of strategy')]),
			  allsource(AllSource,
				 [boolean, default(false),
				  description('Include all sources')]),
			  alltarget(AllTarget,
				 [boolean, default(false),
				  description('Include all target')])
			]),
	find_correspondences(Mapping, Strategy, Source, Target, AllSource, AllTarget, Cs),
	html_current_option(content_type(Type)),
	findall(R-L, mapping_relation(L, R), Relations),
	phrase(html_correspondences(Cs, Relations), HTML),
	format('Content-type: ~w~n~n', [Type]),
	print_html(HTML).

find_correspondences(Mapping, Strategy, Source, Target, true, true, Cs):-
% Case we need all correspondences involving Source or Target
	As = align(Source, _, _),
	At = align(_, Target, _),
	(   rdf(_, amalgame:evidenceGraph, _, Mapping)
	->  findall(As, has_correspondence(As, Mapping), Ss),
	    findall(At, has_correspondence(At, Mapping), Ts)
	;   expand_node(Strategy, Mapping, Ms),
	    findall(As, member(As, Ms), Ss),
	    findall(At, member(At, Ms), Ts)
	),
	append(Ss, Ts, Cs0),
	sort(Cs0, Cs).

find_correspondences(Mapping, Strategy, Source, Target, AllSource, AllTarget, Cs):-
% Other 3 cases:
	(   AllSource, \+ AllTarget
	->  A = align(Source,_,_)
	;   AllTarget, \+ AllSource
	->  A = align(_,Target,_)
	;   \+ AllSource, \+ AllTarget
	->  A = align(Source,Target,_)
	),
	!
	,
	(   rdf(_, amalgame:evidenceGraph, _, Mapping)
	->  findall(A, has_correspondence(A, Mapping), Cs)
	;   expand_node(Strategy, Mapping, Ms),
	    findall(A, member(A, Ms), Cs)
	).

assert_relations(Mapping, Options) :-
	option(strategy(Strategy), Options),
	expand_node(Strategy, Mapping, Mappings),
	forall(member(align(Source, Target, Prov), Mappings),
	       assert_relation(Source, Target,
			       [prov(Prov)|Options])
	      ).

assert_relation(Source, Target, Options) :-
	option(relation(Relation), Options),
	option(evaluation_graph(Graph), Options, eval),
	option(comment(Comment), Options, ''),
	option(user(User), Options, ''),
	option(prov(Prov), Options, []),

	(   has_correspondence(align(Source, Target, OldProv), Graph)
	->  remove_correspondence(align(Source, Target, OldProv), Graph)
	;   true
	),
	now_xsd(Now),
	NewProv = [ method(manual_evaluation),
		    user(User),
		    date(Now),
		    comment(Comment),
		    relation(Relation)
		  ],
	AssertOptions = [
			 evidence_graphs(enabled),
			 graph(Graph),
			 prov([NewProv|Prov])
			],
	append(AssertOptions, Options, NewOptions),
	debug(ag_expand, 'assert cell options: ~w', NewOptions),
	assert_cell(Source, Target, NewOptions).


html_correspondences([], _) --> !.
html_correspondences([align(Source,Target,Evidence)|Cs], Relations) -->
	html_correspondence(Source, Target, Evidence, Relations),
	html_correspondences(Cs, Relations).

html_correspondence(Source, Target, Evidence, Relations) -->
	{ Relation = '',
	  length(Evidence, EvLength)
	},
	html([div(class('yui3-g'),
		  [ div(class('yui3-u-1-2'),
			\html_resource_context(Source, Evidence)),
		    div(class('yui3-u-1-2'),
			\html_resource_context(Target, Evidence))
		  ]),
	      div(class([manualfixes, 'yui3-g']),
		  [ div([class([sourcediv, 'yui3-u-1-5'])],
			[div([class(sourceuri)], Source),
			 input([type(hidden), class(original), value(Source)]),
			 input([type(text), class([skos_ac_field]), name(source)])
			]),
		    div([class([relations, 'yui3-u-3-5'])],
			\html_relations(Relations, Relation)),
		    div([class([targetdiv, 'yui3-u-1-5'])],
			[div([class(targeturi)], Target),
			 input([type(hidden), class(original), value(Target)]),
			 input([type(text), class([skos_ac_field]), name(target)])
			]),
		    div(class([comment, 'yui3-u-1']),
			['because: ', input([type(text), name(comment)], [])
			])
		  ]),
	      div(class(evcount),
		  [ '~w individual motivations: '-(EvLength)]),
	      div(class(evidences),
		  \html_evidences(Evidence, Source, Target))
	     ]).

html_evidences([],_,_) --> !.
html_evidences([E|Es],Source,Target) -->
	{ option(method(Method), E, ''),
	  option(graph(Graph), E, []),
	  (   option(match(Match), E)
	  ->  format(atom(MatchAtom), ' (~2f)' , [Match]),
	      Mt = span([class(match)], MatchAtom)
	  ;   Mt = ''
	  ),
	  (   option(date(Date), E)
	  ->  At = span([class(date)], [' at: ', Date])
	  ;   At = ''
	  ),
	  (   option(source(MSource), E)
	  ->  format(atom(SrcAtom), ' (on: ~p' , [MSource]),
	      Src =  span([class(source)], SrcAtom)
	  ;   Src = ''
	  ),
	  (   option(target(MTarget), E)
	  ->  format(atom(TrgAtom), '/~p)' , [MTarget]),
	      Trg =  span([class(target)], TrgAtom)
	  ;   Trg = ''
	  ),
	  (   option(source_stem(SourceStem), E)
	  ->  format(atom(SStemAtom), ' (source stem: ~w)' , [SourceStem]),
	      Ss = span([class(source_stem)], SStemAtom)
	  ;   Ss = ''
	  ),
	  (   option(target_stem(TargetStem), E)
	  ->  format(atom(TStemAtom), ' (target stem: ~w)' , [TargetStem]),
	      Ts = span([class(target_stem)], TStemAtom)
	  ;   Ts = ''
	  ),
	  (   option(score(Score), E)
	  ->  format(atom(ScsAtom), ' (score: ~w)', [Score]),
	      Scs = span([class(score)], ScsAtom)
	  ;   Scs = ''
	  ),
	  (   option(user(User), E)
	  ->  By = span([class(who)], [' by: ', \rdf_link(User)])
	  ;   By = ''
	  ),
	  (   option(comment(Comment), E)
	  ->  Cm = span([class(comment)], [' with comment: ', Comment])
	  ;   Cm = ''
	  )
	},
	html(div(class(evidence),
		 [ div(class(method), ['match: ', Method, By, At, Mt, Src, Trg, Ss, Ts, Scs, Cm]),
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
		       [shape_hook(ag_mapping:evidence_shape),
			label_hook(ag_mapping:evidence_label),
			graph_attributes([rankdir(Layout)])]).


evidence_graph(Graph, Node, NodeTriples) :-
	findall(rdf(S,P,O),
		(   member(rdf(S,P,O), Graph),
		    (	S == Node ; O == Node)
		),
		NodeTriples).


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
	{ rdf_lang(URI, skos:definition, Txt)
	},
	!,
	html_item(definition, Txt).
html_definition(_) --> !.

html_scope(URI) -->
	{ rdf_lang(URI, skos:scopeNote, Txt)
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

%%	evidence_label(+Resource, +Lang, +MaxLen, -Label) is det.
%
%	Defines graph node label for different types of evidence
%	resources.
%
evidence_label(Resource, Lang, MaxLen, Label) :-
       rdf_display_label(Resource, Lang, Text),
       truncate_atom(Text, MaxLen, Label0),
       (   rdf_global_id(NS:_Local, Resource)
       ->  atomic_list_concat([NS, ':', Label0], Label)
       ;   Label = Label0
       ).
