:- module(eq_util,
	  [ html_eq_header//2,
	    assert_user_provenance/2,
	    amalgame_alignment/2,
	    js_mappings/2,
	    js_alignment_nodes/2,
	    now_xsd/1,
	    xsd_timestamp/2,
	    is_edm_collection/1,
	    mapping_counts/7,
	    concept_count/3,
	    flush_stats_cache/0
	  ]).

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).
:- use_module(user(user_db)).
:- use_module(cliopatria(components/label)).
:- use_module(library(amalgame/expand_graph)).
:- use_module(library(amalgame/vocabulary)).

:- multifile
	eq:menu_item/2.

:- dynamic
	stats_cache/2.

flush_stats_cache :-
	retractall(stats_cache(_,_)),
	flush_expand_cache.

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
	findall(json([uri=M, label=L, stats=json(Stats)]),
		mapping(Alignment, M, L, Stats),
		Mappings).

mapping(Strategy, URI, Label, Stats) :-
	Stats = [
		 numberOfMappings(MN),
		 numberOfSourceConcepts(SN),
		 numberOfTargetConcepts(TN),
		 psources(SPerc),
		 ptargets(TPerc)
		],
	rdf(URI, rdf:type, amalgame:'Mapping',Strategy),
	rdf_display_label(URI, Label),
	!,
	mapping_counts(URI, Strategy, MN, SN, TN, SPerc, TPerc).



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

%%	mapping_counts(+MappingURI, +Strategy, -MappingN, -SourceN, -TargetN)
%
%	Counts for the mappings in MappingURI.
%
%       @param MappingN is the number of total correspondences
%       @param SourceN is the number of source concepts mapped
%       @param TargetN is the number of target concepts mapped

mapping_counts(URL, Strategy, MN, SN, TN, SPerc, TPerc) :-
	stats_cache(URL-Strategy, stats(MN, SN, TN, SPerc, TPerc)),
	!.
mapping_counts(URL, Strategy, MN, SN, TN, SPerc, TPerc) :-
	expand_mapping(Strategy, URL, Mapping),
	mapping_sources(URL, Strategy, InputS, InputT),
	concept_count(InputS, Strategy, SourceN),
	concept_count(InputT, Strategy, TargetN),

	maplist(align_source, Mapping, Ss0),
	maplist(align_target, Mapping, Ts0),
	sort(Ss0, Ss),
	sort(Ts0, Ts),
	length(Mapping, MN),
	length(Ss, SN),
	length(Ts, TN),

	rounded_perc(SourceN, SN, SPerc),
	rounded_perc(TargetN, TN, TPerc),
	retractall(stats_cache(_,_)),
	assert(stats_cache(URL-Strategy, stats(MN, SN, TN, SPerc, TPerc))).

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

%%	concept_count(+Vocab, +Strategy, -Count)
%
%	Count is the number of concepts in Vocab when expanded in Strategy

concept_count(Vocab, Strategy, Count) :-
	stats_cache(Vocab-Strategy, stats(Count)),
	!.
concept_count(Vocab, Strategy, Count) :-
	expand_vocab(Strategy, Vocab, Scheme),
	findall(C, vocab_member(C, Scheme), Cs),
	length(Cs, Count),
	retractall(stats_cache(_,_)),
	assert(stats_cache(Vocab-Strategy, stats(Count))).


%%	mapping_sources(+MappingURI, Strategy, -Source, -Target)
%
%	Source and Target are the recursive source and target
%	vocabularies of Mapping.

mapping_sources(URL, Strategy, S, T) :-
	rdf_has(URL, opmv:wasGeneratedBy, Process, RealProp),
	rdf(URL, RealProp, Process, Strategy),
	!,
	(   rdf(Process, amalgame:source, S0, Strategy),
	    rdf(Process, amalgame:target, T0, Strategy)
	->  vocab_source(S0, Strategy, S),
	    vocab_source(T0, Strategy, T)
	;   rdf(Process, amalgame:input, Input, Strategy)
	->  mapping_sources(Input, Strategy, S, T)
	).

vocab_source(V, Strategy, S) :-
	rdf_has(V, opmv:wasGeneratedBy, Process, Strategy),
	rdf_has(Process, amalgame:input, Input, Strategy),
	!,
	vocab_source(Input, Strategy, S).
vocab_source(V, _S, V).

align_source(align(S,_,_), S).
align_target(align(_,T,_), T).
