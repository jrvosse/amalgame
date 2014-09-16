:- module(ag_utils,
	  [   mint_node_uri/3,
	      assert_user_provenance/2,

	      now_xsd/1,
	      xsd_timestamp/2,
	      has_write_permission/0,

	      save_perc/3,
	      list_offset/3,
	      list_limit/4,
	      sort_by_arg/3
	  ]).


:- use_module(library(semweb/rdf_db)).
:- use_module(user(user_db)).


%%	mint_node_uri(+Strategy, +Type, -URI) is det.
%
%	URI is a new URI in the publish_ns namespace of Strategy, with a
%	Local part that is equal to gensym(Type, Local),
%	such that URI is not already a RDF subject or RDF named graph.

mint_node_uri(_Strategy, _Type, URI) :-
	ground(URI),!.
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


has_write_permission :-
	logged_on(User, anonymous),
	catch(check_permission(User, write(default,_)), _, fail).


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



save_perc(0, _, 0) :- !.
save_perc(Value, Total, Percentage) :-
	Percentage is (100 * Value) / Total.

