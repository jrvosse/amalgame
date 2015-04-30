:- module(conf_ag_modules, []).

% Candidate correspondence generator components:
:- use_module(library(ag_modules/ancestor_generator)).
:- use_module(library(ag_modules/compound_label_generator)).
:- use_module(library(ag_modules/descendent_generator)).
:- use_module(library(ag_modules/exact_label_generator)).
:- use_module(library(ag_modules/isub_generator)).
:- use_module(library(ag_modules/numeric_difference_generator)).
:- use_module(library(ag_modules/preloaded_mapping)).
:- use_module(library(ag_modules/related_generator)).
:- use_module(library(ag_modules/snowball_label_generator)).

% Mapping producing partitioners, based on the generators above:
:- use_module(library(ag_modules/ancestor_selecter)).
:- use_module(library(ag_modules/compound_label_selecter)).
:- use_module(library(ag_modules/descendent_selecter)).
:- use_module(library(ag_modules/exact_label_selecter)).
:- use_module(library(ag_modules/isub_selecter)).
:- use_module(library(ag_modules/numeric_difference_selecter)).
:- use_module(library(ag_modules/preloaded_selecter)).
:- use_module(library(ag_modules/related_selecter)).
:- use_module(library(ag_modules/snowball_label_selecter)).

% Other mapping producing partitioners:
:- use_module(library(ag_modules/arity_select)).
:- use_module(library(ag_modules/ag_sample)).
:- use_module(library(ag_modules/best_numeric)).
:- use_module(library(ag_modules/most_generic)).
:- use_module(library(ag_modules/most_labels)).
:- use_module(library(ag_modules/most_methods)).
:- use_module(library(ag_modules/sibling_selecter)).

% Vocabulary partitioners:
:- use_module(library(ag_modules/voc_exclude)).
:- use_module(library(ag_modules/subtree_select)).
:- use_module(library(ag_modules/type_select)).
:- use_module(library(ag_modules/propvalue_select)).
:- use_module(library(ag_modules/uniq_label_voc_select)).

% MultiInput
:- use_module(library(ag_modules/map_merger)).
:- use_module(library(ag_modules/ag_overlap)).



