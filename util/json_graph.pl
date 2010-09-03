/*  This file is part of ClioPatria.

	Author:
	HTTP:	http://e-culture.multimedian.nl/
	GITWEB:	http://gollem.science.uva.nl/git/ClioPatria.git
	GIT:	git://gollem.science.uva.nl/home/git/ClioPatria.git
	GIT:	http://gollem.science.uva.nl/home/git/ClioPatria.git
	Copyright:  2007, E-Culture/MultimediaN

	ClioPatria is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 2 of the License, or
	(at your option) any later version.

	ClioPatria is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with ClioPatria.  If not, see <http://www.gnu.org/licenses/>.
*/

:- module(json_graph,
	[	iface_key_resource_graph/4,
		graph_to_json/2,		% +Graph:[rdf(s,p,v)], -JSONTerm
		graph_to_json/3,		% +Type, +Graph:[rdf(s,p,v)], -JSONTerm
		graph_to_json/4,		% +Type, +Graph:[rdf(s,p,v)], -JSONTerm, +Options
		triples_to_json/4,              % +Type, +Graph:[rdf(s,p,v)], -JSONTerm, +Options
		rdf_group_by/3,			% +Type, +Graph:[rdf(s,p,v)], -GroupedGraph
		rdf_resource_to_json/2		% +Resource, -JSONTerm
	]).

% http
:- use_module(library('http/json')).
:- use_module(library('http/json_convert')).
:- use_module(library('http/http_json')).

:- use_module(components(label)).

:- use_module(rdfs_plus_skos).


%%	graph_to_json(+Type, +Graph, -JSON, +Options)
%
%	JSON is a prolog json term of Graph.
%	Based on "http://n2.talis.com/wiki/RDF_JSON_Specification".
%
%	Usage reply_json(JSON) to write JSON to stream.
%
%	@Param Type	triples={subject, predicate, object}
%			spo={subject:predicate:[object]}
%			ops={object:predicate:[subject]}
%			etc.

graph_to_json(Graph, JSON) :-
	graph_to_json(spo, Graph, JSON, []).

graph_to_json(Type, Graph, JSON) :-
	graph_to_json(Type, Graph, JSON, []).

graph_to_json(triples, Graph, JSON, Options) :- !,
	triples_to_json(spo, Graph, JSON, Options).

graph_to_json(Type0, Graph, json(JSON), Options) :-
	output_type(Type0, Type),
	option(smax(SMax), Options, -1),
	option(pmax(PMax), Options, -1),
	option(omax(OMax), Options, -1),
	(  option(prefix(true), Options)
	-> JSON = [prefixes=Prefixes|JSONGraph],
	   graph_prefixes_to_json(Graph, Prefixes)
	;  JSON = JSONGraph
	),
	graph_to_json(Type, Graph, SMax, PMax, OMax, JSONGraph).

output_type(display, spo) :- !.
output_type(subject, spo) :- !.
output_type(predicate, pso) :- !.
output_type(object, ops) :- !.
output_type(Type, Type).


%%	graph_to_json(+IndexType, +Graph, +Max1, +Max2, +Max3, -JSON)
%
%	JSON is a json term of graph indexed according to IndexType.

graph_to_json(spo, Graph, SMax, PMax, OMax, JSON) :- !,
	indexed_triples_to_json(1,2,3, Graph, SMax, PMax, OMax, JSON).

graph_to_json(ops, Graph, SMax, PMax, OMax, JSON) :- !,
	indexed_triples_to_json(3,2,1, Graph, OMax, PMax, SMax, JSON).

graph_to_json(pso, Graph, SMax, PMax, OMax, JSON) :- !,
	indexed_triples_to_json(2,1,3, Graph, PMax, SMax, OMax, JSON).

graph_to_json(pos, Graph, SMax, PMax, OMax, JSON) :- !,
	indexed_triples_to_json(2,3,1, Graph, PMax, OMax, SMax, JSON).

graph_to_json(sop, Graph, SMax, PMax, OMax, JSON) :- !,
	indexed_triples_to_json(1,3,2, Graph, SMax, OMax, PMax, JSON).

graph_to_json(osp, Graph, SMax, PMax, OMax, JSON) :- !,
	indexed_triples_to_json(3,1,2, Graph, OMax, SMax, PMax, JSON).

graph_to_json(Type, _, _, _, _, _) :-
	domain_error('output type', Type).


%%	triples_to_json(+Graph, -JSON, +Options)
%
%	JSON is a RDF graph in prolog json term notation.
%	use json_reply to output.

triples_to_json(Type, Graph, JSON, Options) :-
	option(source(Source), Options, false),
	option(subject_info(SP), Options, []),
	option(object_info(OP), Options, []),
	option(predicate_info(PP), Options, []),
	triples_to_json(Graph, Type, Source, SP,PP,OP, JSON).

triples_to_json([], _, _, _, _, _, []).
triples_to_json([Triple|T], Type, Source, SP,PP,OP, [json(JSON)|Rest]) :-
	(   Triple = rdf(S,P,O,Src),
	    Source = true
	->  (   Src = File:Lineno
	    ->  rdf_resource_to_json(File,SrcList),
	        JSON = [source=json(SrcList),
			lineno=Lineno|
			JSON0
		       ]
	    ;   rdf_resource_to_json(Src,SrcList),
		JSON = [source=json(SrcList)|
			JSON0
		       ]
	    )
	;   Triple = rdf(S,P,O,_)
	->  JSON = JSON0
	;   Triple = rdf(S,P,O)
	->  JSON = JSON0
	),
	triple_to_json(Type, S,P,O, SP,PP,OP, JSON0),
	triples_to_json(T, Type, Source, SP,PP,OP, Rest).


%%	triple_to_json(+Type,+Subject,+Predicate,+Object)
%
%	JSONList contains json terms for S,P and O.

triple_to_json(spo, S,P,O, SP,PP,OP, JSONList) :-
	JSONList = [subject=json(SList),
		    predicate=json(PList),
		    object=json(OList)],
	rdf_resource_to_json(S, SP, SList),
	rdf_resource_to_json(P, PP, PList),
	rdf_resource_to_json(O, OP, OList).
triple_to_json(ps, S,P,_, SP,PP,_, JSONList) :-
	JSONList = [subject=json(SList),
		    predicate=json(PList)],
	rdf_resource_to_json(S, SP, SList),
	rdf_resource_to_json(P, PP, PList).
triple_to_json(po, _,P,O, _,PP,OP, JSONList) :-
	JSONList = [object=json(OList),
		    predicate=json(PList)],
	rdf_resource_to_json(O, OP, OList),
	rdf_resource_to_json(P, PP, PList).
triple_to_json(so, S,_,O, SP,_,OP, JSONList) :-
	JSONList = [subject=json(SList),
		    object=json(OList)],
	rdf_resource_to_json(S, SP, SList),
	rdf_resource_to_json(O, OP, OList).


% %	indexed_triples_to_json(+Arg1, +Arg2, +Arg3, +Graph, +Max1, +Max2, +Max3, -JSON)
%
%	JSON is a prolog term in json notation indexed first by the Arg1
%	argument, then Arg2 argument.

indexed_triples_to_json(Arg1, Arg2, Arg3, Graph, Max1, Max2, Max3, JSON) :-
	rdf_group_by(Arg1, Graph, Pairs),
	pairs_to_json(Arg2, Arg3, Pairs, Max1, Max2, Max3, JSON).


% %	pairs_to_json(+Arg2, +Arg3, +Pairs:arg1-[triple], +Max, +Max2, +Max3, -JSON)
%
%	Convert triples grouped by Key to a JSON term.
%
%	@Param Max	maximum Arg1 items that are shown.
%	@param Max2     maximum Arg2 items that are shown.
%	@param Max3     maximum Arg3 items that are shown.

pairs_to_json(_, _, [], _, _, _, []) :- !.
pairs_to_json(_, _, Rest, 0, _, _, [Msg]) :- !,
	rest_to_json(Rest, Msg).
pairs_to_json(Arg2, Arg3, [R0-Triples|T], N, Max2, Max3, [R=json(JSON)|Rest]) :-
	N1 is N - 1,
	resource_to_value(R0, R), % if R0 is a literal we loose information !
	rdf_group_by(Arg2, Triples, Pairs),
	pairs_to_json(Arg3, Pairs, Max2, Max3, JSON),
	pairs_to_json(Arg2, Arg3, T, N1, Max2, Max3, Rest).


% %	pairs_to_json(+Arg3, +Pairs:arg2-[triple], +Max2, +Max3, -JSON)
%
%	Convert triples grouped by arg2 to a JSON.

pairs_to_json(_, [], _, _, []) :- !.
pairs_to_json(_, Rest, 0, _, [Msg]) :- !,
	rest_to_json(Rest, Msg).
pairs_to_json(Arg3, [R0-Triples|T], N, Max3, [R=JSON|Rest]) :-
	N1 is N - 1,
	resource_to_value(R0, R),
	resources_to_json(Arg3, Triples, Max3, JSON),
	pairs_to_json(Arg3, T, N1, Max3, Rest).


%%	resources_to_json(+Nth, +Triples, +Max, -JSON)
%
%	Convert all the Nth arguments from Triple to a JSON term.

resources_to_json(_, [], _, []) :- !.
resources_to_json(_, Rest, 0, [Msg]) :- !,
	rest_to_json(Rest, Msg).
resources_to_json(Arg, [Triple|Ts], N, [json(JSON)|Vs]) :-
	N1 is N-1,
	arg(Arg, Triple, R),
	rdf_resource_to_json(R, JSON0),
	(   arg(4,Triple,Src:Line)
	->  JSON = [src=Src,lineno=Line|JSON0]
	;   arg(4,Triple,Src)
	->  JSON = [src=Src|JSON0]
	;   JSON = JSON0
	),
	resources_to_json(Arg, Ts, N1, Vs).


%%	rdf_resource_to_json(+Resource, +Ps, -JSON)
%
%	Convert an RDF Resource to a JSON term and the properties from
%	Ps.

rdf_resource_to_json(R, [], JSON) :- !,
	rdf_resource_to_json(R, JSON).
rdf_resource_to_json(R, Ps, JSON) :-
	rdf_resource_to_json(R, Vs0),
	iface_resource_properties(R, Ps, Vs1),
	append(Vs0, Vs1, JSON).


%%	rdf_resource_to_json(+Resource, -JSON)
%
%	Convert an RDF Resource to a JSON term.

rdf_resource_to_json(Bool, Object) :-
	boolean_to_json(Bool, Boolean), !,  % Why did we need this?
	Object = [value=Boolean, type=boolean].

rdf_resource_to_json(literal(Lit), Object) :- !,
	Object = [value=Txt, type=literal|Rest],
	literal_to_json(Lit, Txt, Rest).

rdf_resource_to_json(URI0, Object) :-
	rdf_global_id(URI0, URI),
	Object = [value=URI, type=Type],
	object_uri_type(URI, Type).


%%	literal_to_json(+Literal, -Text, -Attributes)
%
%	Extract text and Attributes from Literal resource.

literal_to_json(lang(Lang, Txt), Txt, [lang=Lang]) :- !.
literal_to_json(type(Type, Txt0), Txt, [datatype=Type]) :- !,
	text_of_literal(type(Type,Txt0), Txt). % hack to handle XML data
literal_to_json(Txt, Txt, []).


%%	boolean_to_json(?Bool, ?JSONBool)
%
%	JSONBool has an extra @ in front of true or false.

boolean_to_json(false, @false).
boolean_to_json(true, @true).


%%	rest_to_json(+Rest, -JSON)
%
%	JSON is a json term with information about
%	the number of resources that are not in the output.

rest_to_json(Rest, json(Msg)) :-
	length(Rest, C),
	Msg = [type=more, value=C].


%%	object_uri_type(+URI, -Type)
%
%	Type is one of bnode or uri.

object_uri_type(URI, Type) :-
	(   rdf_is_bnode(URI)
	->  Type = bnode
	;   Type = uri
	).


%%	resource_to_value(+Resource, -Value)
%
%	As keys should be atoms we have to remove the literal
%	information and only keep the label.

resource_to_value(literal(L), Txt) :- !,
	text_of_literal(L, Txt).
resource_to_value(R, R).


%%  rdf_group_by(+Argument, +Graph, -Pairs)
%
%   Pairs contains triples from Graph grouped by
%   one of the three arguments.
%   @Param Argument	the argument to group by 1,2 or 3

rdf_group_by(Arg, Graph, Groups) :-
	map_list_to_pairs(arg(Arg), Graph, Pairs0),
	keysort(Pairs0, Pairs),
	group_pairs_by_key(Pairs, Groups).



%%  graph_prefixes_to_json(+Graph, -Prefixes)
%
%

graph_prefixes_to_json(Graph, json(Prefixes)) :-
	iface_used_namespaces(Graph, FullNS),
	add_ns_abbreviations(FullNS, Prefixes).

add_ns_abbreviations([], []).
add_ns_abbreviations([NS|T0], [Abbr=NS|T]) :-
	rdf_db:ns(Abbr, NS), !,
	add_ns_abbreviations(T0, T).
add_ns_abbreviations([_|T0], T) :-
	add_ns_abbreviations(T0, T).

%%      iface_used_namespaces(+Graph, -Namespaces)
%
%       Namespaces is a list of unique namespaces used in Graph.

iface_used_namespaces(Graph, List) :-
        findall(Ns,
                ( rdf_triple(Graph, S,P,O),
                  triple_ns(S,P,O, Ns)
                ),
                List0),
        sort(List0, List).
triple_ns(R,_,_,Ns) :-
        \+ rdf_is_bnode(R),
        rdf_url_namespace(R, Ns).
triple_ns(_,R,_,Ns) :-
        \+ rdf_is_bnode(R),
        rdf_url_namespace(R, Ns).
triple_ns(_,_,R,Ns) :-
        atom(R),
        \+ rdf_is_bnode(R),
        rdf_url_namespace(R, Ns).

rdf_triple(Graph, S,P,O) :-
        member(rdf(S, P, O), Graph).
rdf_triple(Graph, S,P,O) :-
        member(rdf(S, P, O,_), Graph).

%%      iface_resource_properties(+URI, +Ps, -Pairs).
%%      iface_resource_properties(+URI, +Ps, -Pairs, +Options).
%
%       Pairs contains a key=value pair for each element from Ps.
%       Ps is either a list of properties in which case key is a property,
%       or Ps is a list of property-key pairs in which case property is used
%       to get the value and key is returned with the value.
%
%       @see iface_resource_values

iface_resource_properties(R, Ps0, Pairs) :-
        rdf_global_term(Ps0, Ps),
        iface_resource_properties_(Ps, 0, 0, R, Pairs).

iface_resource_properties(R, Ps0, Pairs, Options) :-
        rdf_global_term(Ps0, Ps),
        resource_ext_map(Options, EMap),
        query_ext_map(Options, QMap),
        iface_resource_properties_(Ps, EMap, QMap, R, Pairs).

iface_resource_properties_([], _, _, _, []).
iface_resource_properties_([P0|Ps], EMap, QMap, R, [Key=V|Pairs]) :-
        (   P0 = P-Key
        ->  iface_has(EMap, QMap, R, P, O, _)
        ;   iface_has(EMap, QMap, R, P0, O, _),
            Key = P0
        ),
        !,
        object_value(O, V),
        iface_resource_properties_(Ps, EMap, QMap, R, Pairs).
iface_resource_properties_([_|Ps], EMap, QMap, R, Pairs) :-
        iface_resource_properties_(Ps, EMap, QMap, R, Pairs).

iface_key_resource_graph(Rs, Ps0, Graph, Options) :-
        rdf_global_term(Ps0, Ps),
        resource_ext_map(Options, EMap),
        query_ext_map(Options, QMap),
        findall(rdf(R,Key,V),
                (   member(R, Rs),
                    member(P-Key, Ps),
                    hooked_rdfs_plus_skos(EMap, QMap, R,P,V)
                ),
                Graph
               ).

hooked_rdfs_plus_skos(EMap, QMap, R,P,V) :-
	rdfs_plus_skos(EMap, QMap, R,P,V, _,_,_).

hooked_rdfs_plus_skos(_, _, R,registered_ns,V) :-
        atom(R),
        rdf_db:ns(_,V),
        sub_atom(R, _,_,_, V),!.

object_value(literal(L), Txt) :- !,
        text_of_literal(L, Txt).
object_value(L, L) :-
        is_list(L), !.
object_value(R, Label) :-
        rdf_subject(R), !,
	label_property(P),
	rdf_has(R, P, Value),
	text_of_literal(Value, Label).

object_value(R, R).
