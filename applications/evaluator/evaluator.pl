:- module(evaluator, []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_request_value)).
:- use_module(library(http/http_session)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_host)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/yui_resources)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_litindex)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(count)).
:- use_module(library(settings)).

:- use_module(components(label)).
:- use_module(user(user_db)).

:- use_module(library(skos/vocabularies)).
:- use_module(library(amalgame/alignment)).
:- use_module(library(amalgame/map)).
:- use_module(library(amalgame/edoal)).
:- use_module(library(amalgame/opm)).

:- use_module(library(json_graph)).

:- setting(evaluator:maxMappings, nonneg, 1000,
	   'Max number of mappings per mapping file to put in the \
	   todo list of the mapping evaluator').
:- setting(evaluator:authorization, oneof([required, optional]), required,
	   'Login required to evaluate or not').

% add local web directories from which static files are served.

:- prolog_load_context(directory, Dir),
   asserta(user:file_search_path(ev, Dir)).
:- asserta(user:file_search_path(css, ev(web))).
:- asserta(user:file_search_path(js, ev(web))).

% http handlers for this applications

:- http_handler(amalgame(evaluator),	       http_evaluator, []).
:- http_handler(amalgame(api/evaluator/reset), http_evaluator_reset, []).
:- http_handler(amalgame(api/evaluator/get),   json_get_mapping, []).
:- http_handler(amalgame(api/evaluator/judge), json_judge_mapping, []).
:- http_handler(amalgame(api/evaluator/save),  json_save_results, []).
:- http_handler(amalgame(api/evaluator/concept),  json_concept, []).


% This triple is deprecated and doubles the glosses in the interface
:- rdf_retractall('http://www.w3.org/2006/03/wn/wn20/schema/gloss',
		  rdfs:subPropertyOf,
		  skos:scopeNote).

:- html_resource(evaluator,
                 [ virtual(true),
		   requires([
			     js('evaluator.js'),
			     css('evaluator.css')
			    ])
		 ]).

:- html_resource(js('evaluator.js'),
                 [ requires([
                             yui('treeview/treeview.js'),
                             yui('treeview/assets/skins/sam/treeview.css'),
                             yui('datasource/datasource.js'),
			     yui('button/button.js'),
                             js('parameters.js')
                            ])
                 ]).


attribute_decl(judgement,        [default(none)]).
attribute_decl(subject,          [default(none)]).
attribute_decl(predicate,        [default(none)]).
attribute_decl(object,           [default(none)]).
attribute_decl(method,           [oneof([head,next]),default(head)]).

:- dynamic
	is_locked/3.

%%	http_evaluator(+Request)
%
%	HTTP handler for web page with the alignment evaluator

http_evaluator(Request) :-
	http_parameters(Request, [
				  graph(Graph, []),
				  target(Target, [default(evaluation_results)])
				 ]),
	(   setting(evaluator:authorization, required)
	->  authorized(write(default, create(evaluation)))
	;   true
	),
	logged_on(User, 'anonymous'),
	(   is_locked(Target, OtherUser, TimeStamp), User \= OtherUser, rdf_graph(Target)
	->  http_link_to_id(http_list_alignment, [graph(Graph)], TryAgainLink),
	    reply_html_page(cliopatria(default),
			    title('Amalgame: target locked'),
			    [h4('Error target locked by other user'),
			     p([],['Target graph: ', Target, ' is locked by ', OtherUser, ' since ', TimeStamp]),
			     p([],['Please ', a([href(TryAgainLink)], 'select'),' another target graph name'])
			    ])
	;   http_link_to_id(http_evaluator_reset, [target(Target)], ResetLink),
	    reply_html_page(cliopatria(default),
			    [ title(['Amalgame alignment evaluator']),
			      script([type('text/javascript'),
				      src('http://yui.yahooapis.com/2.7.0/build/yahoo/yahoo.js')
				     ],[]),
			      script([type('text/javascript'),
				      src('http://yui.yahooapis.com/2.7.0/build/json/json.js')
				     ], []),
			      link([rel(stylesheet),
				    type('text/css'),
				    href('http://yui.yahooapis.com/2.7.0/build/reset-fonts-grids/reset-fonts-grids.css')
				   ],[]),
			      \html_requires(evaluator)
			    ],
			    [
			     div(id(main),
				 div(class('main-content'),
				     [ div(id(evaluator), [])
				     ])),
			     script(type('text/javascript'),
				    [ \yui_script(Graph, Target)]),
			     div(a([href(ResetLink)],'reset all(!)'))
			    ]
			   )
	).

http_evaluator_reset(Request) :-
	http_parameters(Request, [
				  target(Target, [default(evaluation_results)])
				 ]),
	http_session_retractall(mappings_to_do(_,_)),
	http_session_retractall(judgement(_,_,_,_)),
	logged_on(User, 'anonymous'),
	(   unlock_graph(Target, User, _TimeStamp)
	->  format(atom(LockMessage), 'Target graph ~p unlocked for user ~w', [Target, User])
	;   format(atom(LockMessage), 'Warning: cannot unlock ~p for user ~w', [Target, User])
	),
	reply_html_page(cliopatria(default),
			title(['Evaluator reset']),
			[p('The Amalgame alignment evaluator has been reset'),
			 p(LockMessage)
			]).

yui_script(Graph, Target) -->
	{
	 setting(http:prefix, Prefix)
	},
	html(['function serverPrefix() { return "', Prefix, '/amalgame";}\n',
	      'var evaluator = new YAHOO.mazzle.MapCheck(',
	      '\'evaluator\', ',
	      '{graph:\'', Graph, '\',',
	      ' targetgraph:\'', Target, '\'});'
	     ]
	    ).

json_judge_mapping(Request) :-
	http_parameters(Request,
                        [
                         judgement(Judgement0),
                         subject(Subject),
                         predicate(Predicate),
                         object(Object),
			 target(Target, [default(evaluation_results)]),
			 comment(Comment, [default('')])
                        ],
                        [attribute_declarations(attribute_decl)]),
	logged_on(User, anonymous),
	term_to_atom(Judgement1, Judgement0),
	rdf_global_term(Judgement1, Judgement),
        debug(evaluator, 'Judgement: ~w', [Judgement]),
	assert_cell(Subject, Object,
		    [graph(Target),
		     relation(Judgement),
		     prov([relation(Predicate),
			   evaluator(User),
			   comment(Comment)
			  ])
		    ]),
        http_session_assert(judgement(Subject, Predicate, Object, Judgement)),
        reply_json(json([message='judgement processed'])).



json_get_mapping(Request) :-
	http_parameters(Request, [graph(Graph, []), method(Method, []), target(Target, [])]),
	(   Method = head
	->  get_first_mappings(Graph, Target, Mappings, Nr)
        ;   get_next_mappings(Graph, Target, Mappings, Nr)
        ),
	(   Mappings = error
	->  reply_json(json([nr_to_go=0, mappings='Error, no mappings found']))
	;   Mappings = done-done
	->  http_link_to_id(http_list_alignment, [graph(Target)], Redirect),
	    reply_json(json([nr_to_go=0, redirect=Redirect]))
	;   compose_json_answer(Nr, Mappings, Json),
	    reply_json(Json)
	).

get_first_mappings(Graph, Target, Mappings, Nr) :-
	ensure_todo_list(Graph, Target),
	http_session_data(mappings_to_do(Target,Todo)),
	Todo = [_Subject-Mappings|_],
	length(Todo,Nr),!.

get_first_mappings(_, _, error, 0).

get_next_mappings(Graph, Target, Mappings, Nr) :-
	ensure_todo_list(Graph, Target),
	http_session_data(mappings_to_do(Target,[_|ToDo])),!,
	http_session_retractall(mappings_to_do(Target,_)),
	http_session_assert(mappings_to_do(Target,ToDo)),
	length(ToDo,Nr),
        (   Nr = 0
        ->  Mappings = done-done
        ;   [_Subject-Mappings|_] = ToDo
        ).

ensure_todo_list(_Graph, Target) :-
	http_session_data(mappings_to_do(Target,Todo)),
	ground(Todo),!.
ensure_todo_list(Graph, Target) :-
	% We have no do to list...
	% Assume we start from scratch
	logged_on(User, anonymous),
	lock_graph(Target, User, _TimeStamp),!,
	(   rdf_graph(Target) -> rdf_unload(Target); true),
	rdf_assert(Target, rdf:type, amalgame:'EvaluatedAlignment', Target),
	align_clear_stats(graph(Target)),

	atom_concat('Amalgame evaluation process/',Target, Label),
	rdf_bnode(BN_Process),
	rdf_assert(BN_Process, rdfs:label, literal(Label), Target),

	opm_was_generated_by(BN_Process, Target, Target, [was_derived_from([Graph])]),

	rdf_equal(skos:closeMatch, CM),
	setting(evaluator:maxMappings, N),
	answer_set(rdf(Subject,Predicate,Object),
		    (
		    has_map([Subject, Object], _, Options, Graph),
		    option(relation(Predicate), Options, CM)
		    ),
		    N,
		    List
		   ),
	rdf_group_by(1, List, Todo),
	http_session_assert(mappings_to_do(Target, Todo)).


compose_json_answer(Nr, Mappings, Json) :-
	make_display_graph(Mappings, Display),
	graph_to_json(triples,Mappings,Jmappings),
	graph_to_json(spo, Display, Jdisplay),
	Json = json([nr_to_go=Nr,
		     mappings=Jmappings,
		     display=Jdisplay
		    ]).



make_display_graph([], []).
make_display_graph([H|Tail], Out) :-
        make_display_graph(Tail, TailGraph),
        H = rdf(S,P,O),
	iface_key_resource_graph([S,P,O],
				 [(skos:altLabel)-altlabel,
				  (skos:prefLabel)-preflabel,
				  (skos:definition)-deflabel,
                                  (skos:notation)-prelabel,
                                  (skos:scopeNote)-scopelabel,
				  registered_ns-ns,
                                  iconclassCluster-scopelabel,
                                  example_thumbs-examples
                                 ],
                                 Display,
				 []),
        append(TailGraph, Display, All),
        sort(All,Out). % just sort to remove duplicates ...

my_make_directory(Suggestion, Base, Name, Number) :-
        exists_directory(Suggestion),!,
        atom_concat(Base, Number, Suggestion1),
        Number1 is Number + 1,
        my_make_directory(Suggestion1, Base, Name, Number1).

my_make_directory(Name, _, Name, _) :-
        make_directory(Name).

my_make_directory(Name) :-
        logged_on(UserName, anonymous),
        my_make_directory(UserName, UserName, Name, 1).

open_file(Dir, Name, Stream) :-
        absolute_file_name(Name, [relative_to(Dir)], Abs),
        open(Abs, write, Stream).

open_files(Files, Dir) :-
      Files = [exact-ApprovedStream,
               close-CloseStream,
               unrelated-RejectedStream,
               narrower-NarrowerStream,
               related-RelatedStream,
               broader-BroaderStream,
               unsure-UnsureStream
              ],
      my_make_directory(Dir),
      open_file(Dir, 'approved.ttl', ApprovedStream),
      open_file(Dir, 'close.ttl', CloseStream),
      open_file(Dir, 'rejected.ttl', RejectedStream),
      open_file(Dir, 'narrower.ttl', NarrowerStream),
      open_file(Dir, 'related.ttl',  RelatedStream),
      open_file(Dir, 'broader.ttl',  BroaderStream),
      open_file(Dir, 'unsure.ttl',   UnsureStream).

close_files([]).
close_files([_-H|T]) :-
        close(H),
        close_files(T).

save_results(_Files, []).
save_results(Files, [Head|Tail]) :-
        Head = judgement(S,P,O,DB),
        memberchk(DB-File, Files),
        format(File, '<~w> <~w> <~w> .~n', [S,P,O]),
        save_results(Files,Tail).

save_results(_Files, []).
save_results(Files, [Head|Tail]) :-
        Head = judgement(S,P,O,DB),
        memberchk(DB-File, Files),
        format(File, '<~w> <~w> <~w> .~n', [S,P,O]),
        save_results(Files,Tail).

json_save_results(_Request) :-
        debug(map, 'Save request for mappings judgements',[]),
        findall(judgement(S,P,O,DB),
                 http_session_data(judgement(S,P,O,DB)),
                 Results),
        open_files(Files, Dir),
        save_results(Files, Results),
        close_files(Files),
        reply_json(json([nr_to_go=0, dir=Dir])),
        true.


json_concept(Request) :-
	http_parameters(Request,
			[ r(URI,
				 [
				  description('Concept or concept scheme from which we request the tree')])]),
	Node = node(URI, [hit], Children),
	rdf_equal(skos:broader, Rel),
	ancestor_tree(Node, Rel, Tree, []),
        children(URI, Rel, Children, []),
	tree_to_json(Tree, [], JSONTree),
        JSON = json([ method=tree,
                      resource=URI,
                      result=JSONTree
                    ]),
        reply_json(JSON).


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
        Goal = (   rdf_has(Child, Rel, R),
                   has_child(Child, Rel, HasChild),
                   skos_label(Child,Label,[preflang(en)])
                ),
        findall(Label-node(Child, [], HasChild), Goal, Children0),
        key_rank(Children0, normal, Children).

has_child(R, Rel, true) :-
        rdf_has(_, Rel, R),
        !.
has_child(_, _, false).

%%  key_rank(+List:key-value, +Type, -RankedList:value)
%
%   Values from pair are sorted by keyed.
%
%   * Type = forward or reverse

key_rank(Pairs, reverse, Values) :- !,
    keysort(Pairs, Pairs1),
        pairs_values(Pairs1, Values0),
        reverse(Values0, Values).
key_rank(Pairs, _, Values) :-
    keysort(Pairs, Pairs1),
        pairs_values(Pairs1, Values).


%%  tree_to_json(+Tree:node(uri,nodeList), +DisplayProperties, -JSON)
%
%   Tree to JSON term.

tree_to_json(node(R,Attr,Children), Ps, json(Data)) :-
        attr_params(Attr, Params),
        append(Params, Ps, Data0),
        (   is_list(Children)
        ->  Data1 = [children=Nodes|Data0],
            nodes_to_json(Children, Ps, Nodes)
        ;   bool_to_json(Children,HasChildren)
        ->  Data1 = [hasChildren=HasChildren|Data0]
        ;   Data1 = Data0
        ),
	skos_label(R,L,[preflang(en)]),
        Data = [uri=R, label=L|Data1].

nodes_to_json([], _, []) :- !.
nodes_to_json([Node|Nodes], Ps, [JNode|JSON]) :- !,
        tree_to_json(Node, Ps, JNode),
        nodes_to_json(Nodes, Ps, JSON).
nodes_to_json(Bool, _, JSON) :-
        bool_to_json(Bool, JSON).

bool_to_json(false, @false).
bool_to_json(true, @true).

attr_params([], []).
attr_params([H|T], [P|Ps]) :-
        attr_param(H, P), !,
        attr_params(T, Ps).
attr_params([_|T], Ps) :-
        attr_params(T, Ps).

attr_param(Term, Key=Value) :-
        Term =.. [Key,Value],
        !.
attr_param(hit, hit=Bool) :-
        bool_to_json(true, Bool).


lock_graph(Graph, User, Timestamp) :-
	(   is_locked(Graph, OtherUser, OtherTimeStamp)
	->  (   rdf_graph(Graph)
	    ->	debug(evaluator, 'Error ~w already locked by ~w since', [Graph, OtherUser, OtherTimeStamp]),
		fail
	    ;	unlock_graph(Graph, _, _)
	    )
	;   true
	),
	get_time(T), format_time(atom(Timestamp), '%a, %d %b %Y %H:%M:%S %z', T),
	asserta(is_locked(Graph, User, Timestamp)).

unlock_graph(Graph, User, Timestamp) :-
	(   is_locked(Graph, User, Timestamp)
	->  retractall(is_locked(Graph, User, Timestamp))
	;   debug(evaluator, 'Error ~w not locked by ~w', [Graph, User]),
	    fail
	).


