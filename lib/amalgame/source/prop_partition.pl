:- module(prop_partition,[]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/alignment_graph)).

:- public source_select/3.
:- multifile amalgame:component/2.

amalgame:component(source_select, prop_partition(alignment_graph, selected_URI_list, [])).

%%	partition(+Input, -Partition, +Options)
%
%	This partitions the elements in Input based on their value of
%	some RDF property. This property is given in Options as
%	splitprop(P). Note that this gives a true partition only if P is
%	functional. If P is non-functional, an element will be an
%	element of each list associated with all values of P.
%
%	Input should be some term that is a valid second argument for
%	graph_member/2.qqqqqqqq
%	Partition is a list of Key-Value pairs.
%	Each Value is a list of elements (note that Value is thus also a
%	valid second argument for graph_member/2).
%	Each Key is a value of the property P.
%
%	Example:
%       source_select(scheme(wn30:''), Partition, [splitprop(rdf:type)])
%	would split Wordnet 3.0 in 5 subsets, based on the part of
%	speach of the synsets.


source_select(VocGraph, Partition, Options) :-
	option(splitprop(SplitProp),Options),

	findall(Key-Value ,
		(graph_member(Value, VocGraph),
		 rdf_has(Value, SplitProp, Key)
		),
		URIs),
	group_pairs_by_key(URIs, Partition).

