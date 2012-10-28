:- module(eq_voc_api,
	  [
	  ]).

:- use_module(library(semweb/rdf_db)).  % for rdf_meta
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(count)).
:- use_module(user(preferences)).

:- use_module(library(ag_util)).
:- use_module(library(amalgame/vocabulary)).
:- use_module(library(amalgame/ag_stats)).
:- use_module(library(amalgame/expand_graph)).

:- http_handler(amalgame(data/voc), http_data_voc, []).

:- rdf_meta
	rdf_lang(r,r,-),
	rdf_lang(r,r,+,-).

http_data_voc(Request) :-
	setting(eq_mapping:rows_per_page, RowsPerPage),
	http_parameters(Request,
			[ url(URL,
			      [description('URL of scheme or vocabulary')]),
			  alignment(Strategy, [description('URL of strategy')]),
			  limit(Limit,
				[default(RowsPerPage), number,
				 description('limit number of concepts returned')]),
			  offset(Offset,
				 [default(0), number,
				  description('first result that is returned')])
		       ]),
	expand_node(Strategy, URL, Scheme),
	concept_count(URL, Strategy, Count),
	Max is Limit + Offset,
	answer_set(C, vocab_member(C, Scheme), Max, Concepts),

	list_offset(Concepts, Offset, COffset),
	list_limit(COffset, Limit, CLimit, _),
	maplist(concept_data,CLimit, RichConcepts),
	reply_json(json([url=URL,
			 limit=Limit,
			 offset=Offset,
			 concepts=json(RichConcepts),
			 total=Count])).


concept_data(Concept, Concept=json(Data)) :-
	findall(Prop, concept_prop(Concept, Prop), DataS),
	sort(DataS, Data).

concept_prop(C, prefLabel=L) :-
	rdf_lang(C, skos:prefLabel, L).

concept_prop(C, altLabel=L) :-
	rdf_lang(C, skos:altLabel, L).

concept_prop(C, example=L) :-
	rdf_has(C, skos:example, L).

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
	).
