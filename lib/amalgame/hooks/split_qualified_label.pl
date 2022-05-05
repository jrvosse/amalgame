:- module(split_qualified_label, []).

:- use_module(library(settings)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(amalgame/ag_strategy)).
:- use_module(library(amalgame/vocabulary)).
:- use_module(library(ag_modules/string_match_util)).

:- setting(amalgame:split_qualified_labels, boolean, true,
        "Add additional term/qualifier properties for qualified labels").

splitted_labels_graph('http://localhost/amalgame/splitted_labels_graph').

:- multifile
	amalgame:prebuilder/1.

:- rdf_meta
    label_labelxl(r,r).

% Call split_qualified_labels on all vocabularies in Strategy.
amalgame:prebuilder(Strategy) :-
    forall(strategy_vocabulary(Strategy, Vocab),
           split_qualified_labels(Vocab)
          ).

%!  label_labelxl(?Property, ?PropertyXL) is det.
%
%   PropertyXL is the SKOS XL version of Property, or
%   amalgame:label if not applicable.

label_labelxl(skos:prefLabel, skosxl:prefLabel) :- !.
label_labelxl(skos:altLabel, skosxl:altLabel)   :- !.
label_labelxl(_, amalgame:label) :- !.


%!  split_qualified_labels(+Vocab) is det.
%
%   Calls split_qualified_labels/1 on all concepts in Vocab.
split_qualified_labels(Vocab) :-
    forall(vocab_member(Concept, Vocab),
           split_qualified_label(Concept)
          ).

%!  split_qualified_label(+Concept) is det.
%
%   If Concept has a qualified label in the format
%   "Term (Qualifier)"@Lang, a bnode is added with properties
%   amalgame:term Term@Lang and
%   amalgame:qualifier Qualifier@Lang
%
%   If such as bnode already exists, this predicate succeeds without
%   adding any triples.

split_qualified_label(Concept) :-
    % Check if not already splitted into a bnode with term (qualifier)
    rdf_has(Concept, amalgame:label, LiteralObject),
    rdf_is_bnode(LiteralObject),
    rdf(LiteralObject, amalgame:term, _),
    !.

split_qualified_label(Concept) :-
    skos_has(Concept, amalgame:label, Label@Lang, MatchProp, []),
    (   sub_string(Label, Before, _, _, "(")
    ->  sub_string(Label, 0, Before, _, LabelTermPaddded),
        split_string(LabelTermPaddded, "", " ", [LabelTerm]),
        split_string(Label, "()", " ", [LabelTerm, Qualifier, ""]),
        splitted_labels_graph(Graph),
        rdf_create_bnode(LiteralObject),
        label_labelxl(MatchProp, MatchPropXL),
        rdf_assert(Concept, MatchPropXL, LiteralObject, Graph),
        rdf_assert(LiteralObject, amalgame:term, LabelTerm@Lang, Graph),
        rdf_assert(LiteralObject, amalgame:qualifier, Qualifier@Lang, Graph)
    ;   true).
