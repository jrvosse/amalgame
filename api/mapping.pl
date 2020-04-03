:- module(ag_mapping,
	  [
	  ]).

/* Return the correspondences and the statistics for a given mapping node
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(settings)).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).

:- use_module(library(skos/util)).

:- use_module(library(amalgame/ag_stats)).
:- use_module(library(amalgame/mapping_graph)).
:- use_module(library(amalgame/expand_graph)).
:- use_module(library(amalgame/util)).

:- setting(amalgame:rows_per_page, integer, 100,
	   'Maximum number of mappings shown.').

% http handlers:

:- http_handler(amalgame(data/mapping), http_data_mapping, []).

%%	http_data_mapping(+Request)
%
%	Emit JSON object with mappings for a URL.

http_data_mapping(Request) :-
	setting(amalgame:rows_per_page, RowsPerPage),
	http_parameters(Request,
			[ url(URL,
			      [description('URL of mapping or evaluation graph')]),
			  strategy(Strategy, [description('URL of strategy')]),
			  sort(SortBy,
			       [default(source),
				oneof([source,target]),
				description('Sort by')]),
			  limit(Limit,
				[default(RowsPerPage), number,
				 description('limit number of mappings returned')]),
			  offset(Offset,
				 [default(0), number,
				  description('first result that is returned')])
		       ]),

	expand_node(Strategy, URL, Mapping0),
	length(Mapping0, Count),
	augment_relations(Strategy, Mapping0, Augmented, []),
	maplist(mapping_label, Augmented, Labeled),
	sort_key(SortBy, SortKey),
	sort_by_arg(Labeled, SortKey, MSorted),
	list_offset(MSorted, Offset, MOffset),
	list_limit(MOffset, Limit, MLimit, _),
	mapping_json(MLimit, Mapping),
	node_stats(Strategy, URL, Stats, []),
	reply_json(jsondict{url:URL,
			    limit:Limit,
			    offset:Offset,
			    stats:Stats,
			    total:Count,
			    mapping:Mapping
			   }).

sort_key(source, 2).
sort_key(target, 4).

mapping_label(align(S, T, Prov), align(S,SLabel, T,TLabel, Relation)) :-
	skos_notation_ish(S, SLabel),
	skos_notation_ish(T, TLabel),
	append(Prov, FlatProv),
	(   option(relation(Rel), FlatProv)
	->  relation_label(Rel, RLabel),
	    option(comment(Comment), FlatProv, ''),
	    Relation = json([uri=Rel, label=RLabel, comment=Comment])
	;   Relation = null
	).

mapping_json([], []).
mapping_json([Align|As], [json(Data)|Os]) :-
	Align = align(Source, SLabel, Target, TLabel, Relation),
	Data = [source=json([uri=Source, label=SLabel]),
		target=json([uri=Target, label=TLabel]),
		relation = Relation
	       ],
	mapping_json(As, Os).

relation_label(R, Label) :-
	mapping_relation(Label, R), !.
relation_label(R, R).

