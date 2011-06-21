:- module(eq_util,
	  [ html_eq_header//2,
	    assert_user_provenance/2,
	    amalgame_alignment/2,
	    js_mappings/2,
	    js_alignment_nodes/2,
	    now_xsd/1,
	    xsd_timestamp/2,
	    is_edm_collection/1
	  ]).

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).
:- use_module(user(user_db)).
:- use_module(cliopatria(components/label)).

:- multifile
	eq:menu_item/2.


%%	html_eq_header(+Active, +Alignment)
%
%	Emit page header with menu bar

html_eq_header(Active, Alignment) -->
	{ findall(Path-Label, eq:menu_item(Path, Label), Items)
	},
	html(div(id(header),
		 [ div(class(title),
		       a(href(location_by_id(http_eq)), 'Amalgame')),
		   ul(\html_eq_menu(Items, Active, Alignment))
		 ])).

html_eq_menu([], _, _) --> !.
html_eq_menu([Handler-Label|Is], Active, Alignment) -->
	html_menu_item(Handler, Label, Active, Alignment),
	html_eq_menu(Is, Active, Alignment).

html_menu_item(Handler, Label, Active, _Alignment) -->
	{ Handler = Active
	},
	!,
	html(li(class(selected), span(Label))).
html_menu_item(Handler, Label, _Active, Alignment) -->
	{ http_link_to_id(Handler, [alignment(Alignment)], Link)
	},
	html(li(a(href(Link), Label))).

%%	assert_user_provenance(+Resource, -NamedGraph)
%
%	Assert provenance about create process.

assert_user_provenance(R, Graph) :-
	logged_on(User),
	user_property(User, url(Agent)),
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


js_mappings(Alignment, Mappings) :-
	findall(json([uri=M, label=L]),
		mapping(Alignment, M, L),
		Mappings).

mapping(Alignment, URI, Label) :-
	rdf(URI, rdf:type, amalgame:'Mapping',Alignment),
	rdf_display_label(URI, Label).


%%	js_alignment_nodes(+Alignment, -Nodes)
%
%	Nodes contains all nodes in alignment with their OPM type.

js_alignment_nodes(Alignment, Nodes) :-
	findall(S, graph_resource(Alignment, S), Nodes0),
	sort(Nodes0, Nodes1),
	maplist(node_data, Nodes1, Nodes).

graph_resource(Graph, R) :-
	rdf(R,rdf:type,_,Graph).
graph_resource(Graph, R) :-
	rdf(_,amalgame:source,R,Graph).
graph_resource(Graph, R) :-
	rdf(_,amalgame:target,R,Graph).
graph_resource(Graph, R) :-
	rdf(Graph, amalgame:includes, R).

node_data(R, R=json(Props)) :-
	findall(Type=Value, node_prop(R, Type, Value), Props).

node_prop(R, label, Label) :-
	rdf_display_label(R, Lit),
	literal_text(Lit, Label).
node_prop(R, type, Type) :-
	(   rdfs_individual_of(R, amalgame:'AlignmentStrategy')
	->  Type = alignment
	;   rdfs_individual_of(R, amalgame:'Mapping')
	->  Type = mapping
	;   rdfs_individual_of(R, opmv:'Process')
	->  Type = process
	;   Type = vocab
	).
node_prop(EDM, type, vocab) :-
	is_edm_collection(EDM).

node_prop(R, status, Status) :-
	rdf(R, amalgame:status, Status).
node_prop(R, comment, Comment) :-
	rdf(R, rdfs:comment, literal(Lit)),
	literal_text(Lit, Comment).
node_prop(R, link, Link) :-
	resource_link(R, Link).

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
	->  sub_atom(In, 0, B, _, NS),
	    rdf_global_id(NS:Local, URI)
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
