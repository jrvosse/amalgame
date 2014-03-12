:- module(ag_util_components,
	  [
	      html_eq_header//1,
	      html_showlist//1,

	      mint_node_uri/3,

	      rdf_lang/3,
	      rdf_lang/4,

	      assert_user_provenance/2,
	      amalgame_alignment/2,

	      js_mappings/2,
	      js_focus_node/3,
	      js_alignment_nodes/2,

	      now_xsd/1,
	      xsd_timestamp/2,
	      has_write_permission/0,

	      save_perc/3,
	      list_offset/3,
	      list_limit/4,
	      sort_by_arg/3,
	      remove_resource/2 % +Resource, +Graph
	  ]).


:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).
:- use_module(user(user_db)).
:- use_module(user(preferences)).
:- use_module(cliopatria(components/label)).
:- use_module(library(amalgame/caching)).
:- use_module(library(amalgame/map)).
:- use_module(library(amalgame/ag_evaluation)).

:- rdf_meta
	rdf_lang(r,r,-),
	rdf_lang(r,r,+,-).

%%	mint_node_uri(+Strategy, +Type, -URI) is det.
%
%	URI is a new URI in the publish_ns namespace of Strategy, with a
%	Local part that is equal to gensym(Type, Local),
%	such that URI is not already a RDF subject or RDF named graph.
mint_node_uri(Strategy, Type, URI) :-
	ground(Type),
	ground(Strategy),
	rdf_has(Strategy, amalgame:publish_ns, NS),
	atomic_concat(NS, Type, Base),
	reset_gensym(Base),
	repeat,
	gensym(Base, URI),
	\+ rdf_subject(URI),
	\+ rdf_graph(URI),
	!.

:- multifile
	eq:menu_item/2.

eq:menu_item(900=Handler, Label) :-
	(   (logged_on(User, X), X \== User)
	->  fail
	;   Handler = cliopatria_openid:login_page,
	    Label = 'login'
	).

has_write_permission :-
	logged_on(User, anonymous),
	catch(check_permission(User, write(default,_)), _, fail).

%%	html_eq_header(+Active, +Alignment)
%
%	Emit page header with menu bar

html_eq_header(Options) -->
	{
	  findall(Rank-(Path-Label), eq:menu_item(Rank=Path, Label), Items0),
	  keysort(Items0, ItemsSorted),
	  pairs_values(ItemsSorted, Items)
	},
	html(div(id(header),
		 [ div(class(title),
		       a(href(location_by_id(http_eq)), 'Amalgame')),
		   ul(\html_eq_menu(Items, Options))
		 ])).

html_eq_menu([], _) --> !.
html_eq_menu([Handler-Label|Is], Options) -->
	html_menu_item(Handler, Label, Options),
	html_eq_menu(Is, Options).

html_menu_item(Handler, Label, Options) -->
	{ option(active(Handler), Options)
	},
	!,
	html(li(class(selected), span(Label))).
html_menu_item(Handler, Label, Options) -->
	{ option(strategy(Strategy), Options),
	  option(focus(Focus), Options, Strategy),
	  http_link_to_id(http_eq_build,
			  [alignment(Strategy)], ReturnToAfterLogin),
	  http_link_to_id(Handler, [
				    'openid.return_to'(ReturnToAfterLogin),
				    focus(Focus),
				    alignment(Strategy)
				   ],
			  Link)
	},
	html(li(a(href(Link), Label))).

%%	assert_user_provenance(+Resource, -NamedGraph)
%
%	Assert provenance about create process.

assert_user_provenance(R, Graph) :-
	logged_on(User),
	user_property(User, url(Agent)),
	(   user_property(User, realname(Realname))
	->  rdf_assert(Agent, rdfs:label, literal(Realname), Graph)
	),
	now_xsd(Time),
	rdf_assert(R, dcterms:creator, Agent, Graph),
	rdf_assert(R, dcterms:date, literal(type(xsd:dateTime, Time)), Graph).


%%	amalgame_alignment(?Alignment, ?Schemes)
%
%	Alignment is an amalgame alignment and schemes are the
%       conceptSchemes that it includes.

amalgame_alignment(Alignment, Schemes) :-
	rdfs_individual_of(Alignment, amalgame:'AlignmentStrategy'),
	findall(S,  rdf(Alignment, amalgame:includes, S), Schemes),
	Schemes \== [].


js_mappings(Strategy, Results) :-
	findall(M-L,
		mapping(Strategy, M, L),
		Mappings),
	findall(json([uri=M, label=L, stats=json(Stats)]),
		(   member(M-L, Mappings),
		    stats_cache(M-Strategy, stats(MN, SN, TN, SPerc, TPerc)),
		    Stats = [
			     numberOfMappings(MN),
			     numberOfSourceConcepts(SN),
			     numberOfTargetConcepts(TN),
			     pSources(SPerc),
			     pTargets(TPerc)
			    ]
		),
		Results).

mapping(Strategy, URI, Label) :-
	rdf(URI, rdf:type, amalgame:'Mapping',Strategy),
	rdf_display_label(URI, Label).


%%	js_focus_node(+Alignment, +URI, -NodeProps)
%
%	NodeProps contains the currently accessible properties for URI

js_focus_node(Alignment, URI, NodeProps) :-
	findall(Type=Value, node_prop(Alignment, URI, Type, Value), NodeProps).

%%	js_alignment_nodes(+Alignment, -Nodes)
%
%	Nodes contains all nodes in alignment with their Strategy type
%	(process, vocab, strategy or mapping).

js_alignment_nodes(Strategy, Nodes) :-
	findall(S, graph_resource(Strategy, S), Nodes0),
	sort(Nodes0, Nodes1),
	maplist(node_data(Strategy), Nodes1, Nodes).


graph_resource(Graph, R) :-
	rdf(R,rdf:type,_,Graph),
	\+ is_empty_eval_graph(R).
graph_resource(Graph, R) :-
	rdf(_,amalgame:source,R,Graph).
graph_resource(Graph, R) :-
	rdf(_,amalgame:target,R,Graph).
graph_resource(Graph, R) :-
	rdf(Graph, amalgame:includes, R).

node_data(Strategy, R, R=json(Props)) :-
	findall(Type=Value, node_prop(Strategy, R, Type, Value), Props).

node_prop(_, R, uri, R).
node_prop(S, R, label, Label) :-
	(   rdf(R, rdfs:label, Lit, S) % use label defined in strategy by user!
	->  true
	;   rdf_display_label(R, Lit)
	),
	literal_text(Lit, Label).
node_prop(_S, R, type, Type) :-
	(   rdfs_individual_of(R, amalgame:'AlignmentStrategy')
	->  Type = strategy
	;   rdfs_individual_of(R, amalgame:'Mapping')
	->  Type = mapping
	;   rdfs_individual_of(R, amalgame:'Process')
	->  Type = process
	;   Type = vocab
	).
node_prop(S, R, secondary_inputs, Inputs) :-
	findall(I,
		(   rdf_has(R, amalgame:secondary_input, I, RP),
		    rdf(R, RP, I, S)

		), Inputs).
node_prop(S, R, local, Local) :-
	rdf(S, amalgame:publish_ns, NS, S),
	sub_atom(R, 0, L, _, NS),
	sub_atom(R, L, _, 0, Local).

node_prop(S, R, status, Status) :-
	rdf(R, amalgame:status, Status, S).
node_prop(S, R, default_relation, Relation) :-
	rdf(R, amalgame:default_relation, Relation, S).
node_prop(S, R, comment, Comment) :-
	rdf(R, rdfs:comment, literal(Lit), S),
	literal_text(Lit, Comment).
node_prop(_, R, link, Link) :-
	resource_link(R, Link).
node_prop(S, R, abbrev, Nick) :-
	rdfs_individual_of(R, amalgame:'Mapping'),
	nickname(S,R,Nick).
node_prop(S, R, namespace, NS) :-
	rdf(R, amalgame:publish_ns, NS, S).


%%	http:convert_parameter(+Type, +In, -URI) is semidet.
%
%	HTTP parameter conversion for the following types:
%
%	    * uri
%	    This  conversion  accepts NS:Local and absolute URIs.

http:convert_parameter(uri, In, URI) :-
	(   sub_atom(In, B, _, A, :),
	    sub_atom(In, _, A, 0, Local),
	    xml_name(Local)
	->  ( (sub_atom(In, 0, B, _, NS), rdf_db:ns(NS,_))
	    ->  rdf_global_id(NS:Local, URI)
	    ;   URI=Local
	    )
	;   is_absolute_url(In)
	->  URI = In
	).


%%	now_xsd(-Text:atom)
%
%	Text is the current time in xsd:dateTime format.

now_xsd(Text) :-
	get_time(TimeStamp),
	xsd_timestamp(TimeStamp, Text).

%%	xsd_timestamp(+Time:timestamp, -Text:atom) is det.
%
%	Generate a description of a Time in xsd:dateTime format

xsd_timestamp(Time, Atom) :-
	stamp_date_time(Time, Date, 'UTC'),
        format_time(atom(Atom),
                    '%FT%T%:z',
                    Date, posix).

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
	N < 0,
	!.
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


%%	sort_by_arg(+ListOfTerms, +Arg, -SortedList)
%
%	SortedList contains the Terms from ListOfTerms sorted by their
%	nth Arg.

sort_by_arg(List, Arg, Sorted) :-
	maplist(arg_key(Arg), List, Pairs),
	keysort(Pairs, SortedPairs),
	pairs_values(SortedPairs, Sorted).

arg_key(Args, Term, Keys-Term) :-
	is_list(Args),
	!,
	args(Args, Term, Keys).
arg_key(Arg, Term, Key-Term) :-
	arg(Arg, Term, Key).

args([A], Term, [Key]) :- !,
	arg(A, Term, Key).
args([A|As], Term, [Key|Ks]) :-
	arg(A, Term, Key),
	args(As, Term, Ks).

%%	remove_resource(+Resource, +Graph) is det.
%
%	Remove all references to Resource from Graph,
%	including (recursively) all blank nodes that
%	Resource uniquely referred to.

remove_resource(R, G) :-
	ground(R),
	ground(G),
	findall(Blank,
		(   rdf(R,_,Blank, G),
		    rdf_is_bnode(Blank),
		    \+ (rdf(R2, _, Blank, G), R2 \= R)
		),
		BlankNodes),
	forall(member(B, BlankNodes),
	       remove_resource(B, G)
	      ),
	rdf_retractall(R,_,_,G),
	rdf_retractall(_,R,_,G),
	rdf_retractall(_,_,R,G).


html_showlist([]) --> !.
html_showlist([H]) -->  html(H),!.
html_showlist([H1,H2|Tail]) -->  html([H1,', ']), html_showlist([H2|Tail]).

save_perc(0, _, 0) :- !.
save_perc(Value, Total, Percentage) :-
	Percentage is (100 * Value) / Total.

% no longer used
rounded_perc(0, _, 0.0) :- !.
rounded_perc(_, 0, 0.0) :- !.
rounded_perc(Total, V, Perc) :-
	Perc0 is V/Total,
	dyn_perc_round(Perc0, Perc, 100).

dyn_perc_round(P0, P, N) :-
	P1 is round(P0*N),
	(   P1 == 0
	->  N1 is N*10,
	    dyn_perc_round(P0, P, N1)
	;   P is P1/(N/100)
	).


%%	rdf_lang(+Subject, +Predicate, ?Text, +Default) is det.
%
%	Text is unified with the "preferred" textual value of literal
%	property Predicate on Subject.  Order of preference:
%	1. Text is in the user:lang defined by user_preference/2.
%	2. Text is in the English language.
%	3. Text is in a random other language
%	4. Text is unified with Default.

rdf_lang(Subject, Predicate, Text, Default) :-
	(   rdf_lang(Subject, Predicate, Text)
	->  true
	;   Text = Default
	).

rdf_lang(Subject, Predicate, Text) :-
	user_preference(user:lang, literal(Lang)),
	(   rdf(Subject, Predicate, literal(lang(Lang, Text)))
	->  true
	;   rdf(Subject, Predicate, literal(lang(en, Text)))
	->  true
	;   rdf(Subject, Predicate, literal(lang(_, Text)))
	),!.

rdf_lang(Subject, Predicate, Text) :-
	user_preference(user:lang, literal(Lang)),
	findall(Literal,
		literal_object_lit(Subject, Predicate, Literal),
		Literals),
	(   member(literal(lang(Lang, Text)), Literals)
	;   member(literal(lang(en, Text)), Literals)
	;   member(literal(lang(_, Text)), Literals)
	;   member(literal(Text), Literals)
	),
	!.

literal_object_lit(Subject, Predicate, Literal) :-
	rdf(Subject, Predicate, Object),
	rdf_is_resource(Object),
	(   rdf_has(Object, rdf:value, Literal)
	;   rdf_has(Object, skosxl:literalForm, Literal)
	),
	rdf_is_literal(Literal).


