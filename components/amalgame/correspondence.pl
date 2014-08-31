:- module(ag_component_correspondence,
	  [ html_correspondence_overlay//1,  % correspondence detail view, static part
	    html_correspondences//2	     % idem, dynamic part
	  ]).
:- use_module(library(option)).
:- use_module(library(lists)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(components(label)).
:- use_module(components(graphviz)).

:- use_module(library(skos/util)).
:- use_module(library(amalgame/util)).
:- use_module(library(amalgame/rdf_util)).

html_correspondence_options(_Options) -->
	html([ 'include all correspondences with the same: ',
	       input([type(checkbox), id(allsources), autocomplete(off)]),
	       label(' source'),
	       input([type(checkbox), id(alltargets), autocomplete(off)]),
	       label(' target')
	     ]).

html_correspondence_buttons(Options) -->
	{ is_list(Options),
	  option(editmode(none), Options, none),!
	},
	html_correspondence_buttons(nav).


html_correspondence_buttons(Options) -->
	{ is_list(Options),
	  option(editmode(edit), Options, none),!
	},
	html_correspondence_buttons(nav),
	html_correspondence_buttons(edit).

html_correspondence_buttons(nav) -->
	html([ button([type(button), class(cancel), value(cancel), title(cancel)],   'x'),
	       button([type(button), class(prev),   value(prev),   title(previous)], '<'),
	       button([type(button), class(next),   value(next),   title(next)],     '>')
	     ]).

html_correspondence_buttons(edit) -->
	% delete button only valid if nothing has changed,
	% submit/setall only if something has:
	html([ button([type(button), class([edit, notchanged, delete]), value(delete)], 'delete'),
	       button([type(button), class([edit, changed,    submit]), value(submit)], 'submit'),
	       button([type(button), class([edit, changed,    setall])], 'apply to all')
	     ]).

html_correspondence_overlay(Options) -->
	html(form([div([class('yui3-widget-bd')],
		       [ div(class(options), \html_correspondence_options(Options)),
			 div(class(buttons), \html_correspondence_buttons(Options)),
			 div([class(concepts), id(concepts)], [])
		       ]),
		   div(class('yui3-widget-ft'),
		       [ div([class(controls)],
			     [ div(class(buttons), \html_correspondence_buttons(Options))
			     ])
		       ])
		  ])).

%%	html_correspondences(+Correspondences, +Options)// is det.
%
%	Correspondences is a List align/3 terms, Options can be
%
html_correspondences([], _) --> !.
html_correspondences([A|As], Options) -->
        html_correspondence(A, Options),
        html_correspondences(As, Options).

html_correspondence(align(Source, Target, Evidence), Options) -->
        { length(Evidence, EvLength),
	  option(relations(Relations), Options, []),
	  (   option(mode('fill-in'), Options)
	  ->  member(Method, Evidence),
	      option(relation(Relation), Method, undefined),
	      option(comment(Comment),   Method, '')
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
	  (   option(graph(Graph), E), atom(Graph)
	  ->  Gr = span([class(graph)], [' from: ', \rdf_link(Graph)])
	  ;   Gr = ''
	  ),
	  (   option(relation(Rel), E), atom(Rel)
	  ->  Re = span([class(relation)], [' relation: ', \rdf_link(Rel)])
	  ;   Re = ''
	  ),
	  (   option(comment(Comment), E)
	  ->  Cm = span([class(comment)], [' with comment: ', Comment])
	  ;   Cm = ''
	  )
	},
	html(div(class(evidence),
		 [ div(class(method),
		       ['match: ',
			Method, By, Gr, Re, At, Mt, Src, Trg, Ss, Ts, Scs, Cm]),
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
		       [shape_hook(ag_component_correspondence:evidence_shape),
			label_hook(ag_component_correspondence:evidence_label),
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
	  findall(R, skos_related_concept(URI, R), Related),
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
%	Tree contains the ancestors and children from Resource.

resource_tree(R, Tree) :-
	Node = node(R, [hit], Children),
	ancestor_tree(Node, Tree, []),
        children(R, Children).

ancestor_tree(Node, Tree, Options) :-
        Node = node(URI,_,_),
	skos_parent_child(Parent, URI),
        URI \== Parent,
        (   select_option(sibblings(true), Options, Options1)
        ->  ancestor_tree(node(Parent, [], [Node|Siblings]), Tree, Options1),
            children(Parent, Children),
            select(node(URI,_,_), Children, Siblings)
        ;   ancestor_tree(node(Parent, [], [Node]), Tree, Options)
        ).
ancestor_tree(Tree, Tree, _).

children(Concept, List) :-
	findall(D, skos_descendant_of(Concept, D), Dlist),
	maplist(has_child, Dlist, List).

has_child(C, node(C, [], HasChild)) :-
	  (   skos_parent_child(C, _)
	  ->  HasChild = true
	  ;   HasChild = false
	  ).

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
