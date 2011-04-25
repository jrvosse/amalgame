:- module(eq_util,
	  [ assert_user_provenance/2,
	    amalgame_alignment/2,
 	    js_alignment_nodes/2,
	    now_xsd/1,
	    xsd_timestamp/2
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).
:- use_module(user(user_db)).



%%	assert_user_provenance(+Resource, -NamedGraph)
%
%	Assert provenance about create process.

assert_user_provenance(R, Graph) :-
	current_user(User),
	now_xsd(Time),
	rdf_assert(R, dc:creator, User, Graph),
	rdf_assert(R, dc:date, literal(type(xsd:date, Time)), Graph).


%%	amalgame_alignment(?Alignment, ?Schemes)
%
%	Alignment is an amalgame alignment and schemes are the
%       conceptSchemes that it includes.

amalgame_alignment(Alignment, Schemes) :-
	rdfs_individual_of(Alignment, amalgame:'Alignment'),
	findall(S,  rdf(Alignment, amalgame:includes, S), Schemes),
	Schemes \== [].


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
	rdf(Graph, amalgame:includes, R).

node_data(R, R=json([type=Type, label=Label])) :-
	rdf_display_label(R, Lit),
	literal_text(Lit, Label),
	(   node_type(R, T)
	->  Type = T
	;   Type = vocab
	).

node_type(R, Type) :-
	rdf(R, rdf:type, Class),
	(   rdf_equal(Class, amalgame:'Alignment')
	->  Type = alignment
	;   rdfs_subclass_of(Class, amalgame:'Mapping')
	->  Type = mapping
	;   rdfs_subclass_of(Class, opmv:'Process')
	->  Type = process
	).


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
