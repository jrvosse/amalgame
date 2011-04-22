:- module(eq_util,
	  [ new_alignment/2,
	    amalgame_alignment/2,
 	    js_alignment_nodes/2
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

%%	new_alignment(+Schemes, -AlignmentURI)
%
%	Assert a new alignment graph.

new_alignment(Schemes, Alignment) :-
	rdf_bnode(Alignment),
	rdf_transaction((rdf_assert(Alignment, rdf:type, amalgame:'Alignment', Alignment),
			 add_schemes(Schemes, Alignment))).

add_schemes([], _).
add_schemes([Scheme|Ss], A) :-
	rdf_assert(A, amalgame:includes, Scheme, A),
	add_schemes(Ss, A).


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
	findall(S, rdf(S,_,_,Alignment), Nodes0),
	sort(Nodes0, Nodes1),
 	maplist(node_type, Nodes1, Nodes).

node_type(R, R=Type) :-
	rdf(R, rdf:type, Class),
	(   rdf_equal(Class, amalgame:'Alignment')
	->  Type = alignment
	;   rdfs_subclass_of(Class, amalgame:'Mapping')
	->  Type = mapping
	;   rdfs_subclass_of(Class, opmv:'Process')
	->  Type = process
	),
	!.
node_type(R, R=vocab).



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
