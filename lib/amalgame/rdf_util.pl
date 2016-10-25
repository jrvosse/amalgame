:- module(ag_rdf_util, [
	      rdf_cp_graphs/2,
	      rdf_cp_graph/3,
	      rdf_remove_resource/2,
	      rdf_has/5,
	      rdf_lang/3,
	      rdf_lang/4,
	      rdf_graph_label/2,
	      rdf_literal_predicates/1
	  ]).

:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(user(preferences)).
:- use_module(library(amalgame/caching)).


:- rdf_meta
	rdf_has(r,r,o,r,r),
	rdf_lang(r,r,-),
	rdf_lang(r,r,+,-).

%%	rdf_has(S,P,O,RP,G) is nondet
%
%	Behaves as rdf_has/4 but the underlying triple needs to be in
%	named graph G.
rdf_has(S,P,O,RP,G) :-
	(   ground(G)
	->  rdf(S,RP,O,G:_),
	    rdf_has(S,P,O,RP)
	;   rdf_has(S,P,O,RP),
	    rdf(S,RP,O,G:_)
	).

rdf_literal_predicates(L) :-
	rdf_literal_predicates_cache(L),!.

rdf_literal_predicates(L) :-
	findall(P, rdf_is_literal_predicate(P), Ps),
	sort(Ps, L),
	assert(rdf_literal_predicates_cache(L)).

rdf_is_literal_predicate(P) :-
	rdf_current_predicate(P),
	once(rdf(_, P, literal(_))).

%%	rdf_cp_graphs(+GraphList, Target) is det.
%
%	Copy all triples in the named graphs in GraphList to the named
%	graph Target.

rdf_cp_graphs([], _Target) :- !.
rdf_cp_graphs([Head|Tail], Target) :-
	rdf_cp_graph(Head, Target, false),
	rdf_cp_graphs(Tail, Target).

%%	rdf_cp_graph(+Source, +Target, +Overwrite) is det.
%
%	Copy all triples from Source to Target.
%	If Overwrite is true, existing triples in Target are removed
%	first.

rdf_cp_graph(Source, Target, true) :-
	rdf_unload_graph(Target), % Delete old graphs under the same name
	rdf_cp_graph(Source, Target, false).

rdf_cp_graph(Source, Target, false) :-
	findall(rdf(S,P,O), rdf(S,P,O,Source), Triples),
	forall(member(rdf(S,P,O), Triples),
	       rdf_assert(S,P,O,Target)).

%%	rdf_remove_resource(+Resource, +Graph) is det.
%
%	Remove all references to Resource from Graph,
%	including (recursively) all blank nodes that
%	Resource uniquely referred to.

rdf_remove_resource(R, G) :-
	ground(R),
	ground(G),
	findall(Blank,
		(   rdf(R,_,Blank, G),
		    rdf_is_bnode(Blank),
		    \+ (rdf(R2, _, Blank, G), R2 \= R)
		),
		BlankNodes),
	forall(member(B, BlankNodes),
	       rdf_remove_resource(B, G)
	      ),
	rdf_retractall(R,_,_,G),
	rdf_retractall(_,R,_,G),
	rdf_retractall(_,_,R,G).
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
	(   rdf_has(Subject, Predicate, literal(lang(Lang, Text)))
	->  true
	;   rdf_has(Subject, Predicate, literal(lang(en, Text)))
	->  true
	;   rdf_has(Subject, Predicate, literal(lang(_, Text)))
	),!.

rdf_lang(Subject, Predicate, Text) :-
	user_preference(user:lang, literal(Lang)),
	findall(Literal,
		literal_object_lit(Subject, Predicate, Literal),
		Literals),
	(   member(literal(lang(Lang, Text)), Literals)
	;   member(literal(lang(en, Text)), Literals)
	;   member(literal(lang(_, Text)), Literals)
	;   member(literal(Text), Literals)
	),
	!.

literal_object_lit(Subject, Predicate, Literal) :-
	rdf(Subject, Predicate, Object),
	rdf_is_resource(Object),
	(   rdf_has(Object, rdf:value, Literal)
	;   rdf_has(Object, skosxl:literalForm, Literal)
	),
	rdf_is_literal(Literal).

rdf_graph_label(Graph, Label) :-
	rdf_display_label(Graph, Lit),
	literal_text(Lit, Label),!.
rdf_graph_label(Graph, Graph).
