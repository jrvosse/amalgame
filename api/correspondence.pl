:- module(ag_api_correspondence,
	  [
	  ]).

:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(components(label)).
:- use_module(components(graphviz)).

:- use_module(library(skos/util)).

:- use_module(library(amalgame/map)).
:- use_module(library(amalgame/expand_graph)).
:- use_module(library(amalgame/util)).

% http handlers

:- http_handler(amalgame(api/correspondence), http_correspondence, []).

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
			  mode(Mode, [
                                   oneof(empty, 'fill-in'),
                                   default(empty),
                                   description('Fill-in the form or leave it empty')]),
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
	phrase(html_correspondences(Cs, [relations(Relations), mode(Mode)]), HTML),
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

html_correspondences([], _) --> !.
html_correspondences([A|As], Options) -->
        html_correspondence(A, Options),
        html_correspondences(As, Options).

html_correspondence(align(Source, Target, Evidence), Options) -->
        { length(Evidence, EvLength),
	  option(relations(Relations), Options),
	  (   option(mode('fill-in'), Options)
	  ->  member(Method, Evidence),
	      member(relation(Relation), Method),
	      member(comment(Comment), Method)
	  ;   Comment = '', Relation = undefined
	  )
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
                        ['because: ', input([type(text), name(comment), value(Comment)])
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
		       [shape_hook(ag_api_correspondence:evidence_shape),
			label_hook(ag_api_correspondence:evidence_label),
			graph_attributes([rankdir(Layout)])]).


evidence_graph(Graph, Node, NodeTriples) :-
	findall(rdf(S,P,O),
		(   member(rdf(S,P,O), Graph),
		    (	S == Node ; O == Node)
		),
		NodeTriples).


html_resource_context('',_) --> !.
html_resource_context(URI, _Prov) -->
	{ rdf_display_label(URI, Label),
	  skos_all_labels(URI, Alt0),
	  select(Label, Alt0, Alt),
	  resource_tree(URI, Tree),
	  skos_related_concepts(URI, Related),
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



image_examples(R, Es) :-
	% hack: assume non-literal examples to be image urls ...
	findall(E, ( rdf(R, skos:example, E),
		     \+ E =.. [literal|_]
		   ),
		List),
	sort(List, Es).

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

html_label(L) -->
	html(L).

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
