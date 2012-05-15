:- module(eq_util,
	  [ html_eq_header//1,
	    assert_user_provenance/2,
	    amalgame_alignment/2,
	    js_mappings/2,
	    js_focus_node/3,
	    js_alignment_nodes/2,
	    now_xsd/1,
	    xsd_timestamp/2,
	    is_edm_collection/1,
	    has_write_permission/0
	  ]).


:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).
:- use_module(user(user_db)).
:- use_module(cliopatria(components/label)).
:- use_module(library(amalgame/caching)).
:- use_module(library(amalgame/alignment)).
:- use_module(library(amalgame/ag_evaluation)).

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
	->  rdf_assert(Agent, rdfs:label, literal(Realname))
	),
	now_xsd(Time),
	rdf_assert(R, dcterms:creator, Agent, Graph),
	rdf_assert(R, dcterms:date, literal(type(xsd:date, Time)), Graph).


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
		    % mapping_counts(M, Strategy, MN, SN, TN, SPerc, TPerc),
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
%	Nodes contains all nodes in alignment with their OPM type.

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
node_prop(_S, R, label, Label) :-
	rdf_display_label(R, Lit),
	literal_text(Lit, Label).
node_prop(_S, R, type, Type) :-
	(   rdfs_individual_of(R, amalgame:'AlignmentStrategy')
	->  Type = strategy
	;   rdfs_individual_of(R, amalgame:'Mapping')
	->  Type = mapping
	;   rdfs_individual_of(R, opmv:'Process')
	->  Type = process
	;   Type = vocab
	).
node_prop(_S, EDM, type, vocab) :-
	is_edm_collection(EDM).
node_prop(S, R, secondary_inputs, Inputs) :-
	findall(I,
		(   rdf_has(R, amalgame:secondary_input, I, RP),
		    rdf(R, RP, I, S)

		), Inputs).
node_prop(S, R, namespace, NS) :-
	rdf(R, amalgame:publish_ns, NS, S).
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
%	Text is the current time in xsd:date format.

now_xsd(Text) :-
	get_time(TimeStamp),
	xsd_timestamp(TimeStamp, Text).

%%	xsd_timestamp(+Time:timestamp, -Text:atom) is det.
%
%	Generate a description of a Time in xsd:date format

xsd_timestamp(Time, Atom) :-
	stamp_date_time(Time, Date, 'UTC'),
        format_time(atom(Atom),
                    '%FT%T%:z',
                    Date, posix).

is_edm_collection(EDM) :-
	once(rdf(_,edm:country, _, EDM:_)).

is_edm_collection(EDM) :-
	findall(Target-Graph-Class, is_edm_collection_(Target, Graph, Class), Results0),
	sort(Results0, Results),
	forall(member(Target-Graph-Class, Results),
	       (   rdf_assert(Target, rdf:type, amalgame:'Alignable', amalgame),
		   rdf_assert(Target, amalgame:graph, Graph, amalgame),
		   rdf_assert(Target, amalgame:class, Class, amalgame)
	       )
	      ),
	!,
	member(EDM-_-_, Results).

is_edm_collection_(EDM, Graph, Class) :-
	rdf_equal(Class,  edm:'Agent'),
	rdfs_individual_of(Agent, Class),
	rdf(Agent, rdf:type, _, Graph:_),
	atom_concat(Graph, '_Agent', EDM).



