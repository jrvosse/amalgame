:- module(conf_ag_modules, []).

% Candidate correspondence generator components:
:- use_module(library(ag_modules/ancestor_generator)).
:- use_module(library(ag_modules/compound_label_generator)).
:- use_module(library(ag_modules/descendent_generator)).
:- use_module(library(ag_modules/exact_label_generator)).
:- use_module(library(ag_modules/isub_generator)).
:- use_module(library(ag_modules/related_generator)).

% Mapping producing partitioners:
:- use_module(library(ag_modules/ancestor_selecter)).
:- use_module(library(ag_modules/compound_label_selecter)).
:- use_module(library(ag_modules/descendent_selecter)).
:- use_module(library(ag_modules/exact_label_selecter)).
:- use_module(library(ag_modules/isub_selecter)).
:- use_module(library(ag_modules/related_selecter)).

% Vocabulary filters/selecters
:- use_module(library(ag_modules/voc_exclude)).
:- use_module(library(ag_modules/subtree_select)).
:- use_module(library(ag_modules/type_select)).
:- use_module(library(ag_modules/propvalue_select)).

% Modules that can be used as matchers
:- use_module(library(ag_modules/snowball_match)).
:- use_module(library(ag_modules/preloaded_mapping)).

% Alignment filters/selecters
:- use_module(library(ag_modules/arity_select)).
:- use_module(library(ag_modules/ag_sample)).
:- use_module(library(ag_modules/token_arity_select)).
:- use_module(library(ag_modules/best_numeric)).
:- use_module(library(ag_modules/most_methods)).
:- use_module(library(ag_modules/most_labels)).
:- use_module(library(ag_modules/most_generic)).
:- use_module(library(ag_modules/sibling_selecter)).
:- use_module(library(ag_modules/preloaded_selecter)).



% Mergers
:- use_module(library(ag_modules/map_merger)).

% Analyzers
:- use_module(library(ag_modules/ag_overlap)).
% :- use_module(library(ag_modules/evaluate)).
