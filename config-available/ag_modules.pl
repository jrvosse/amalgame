:- module(conf_ag_modules, []).


% Modules that can be used as matchers and as alignment filters/selecters
:- use_module(library(ag_modules/exact_label_match)).
:- use_module(library(ag_modules/compound_match)).
:- use_module(library(ag_modules/snowball_match)).
:- use_module(library(ag_modules/isub_match)).
:- use_module(library(ag_modules/ancestor_match)).
:- use_module(library(ag_modules/descendent_match)).
:- use_module(library(ag_modules/related_match)).

% Alignment filters/selecters
:- use_module(library(ag_modules/arity_select)).
:- use_module(library(ag_modules/best_numeric)).
:- use_module(library(ag_modules/most_methods)).

% Vocabulary filters/selecters
:- use_module(library(ag_modules/voc_exclude)).
:- use_module(library(ag_modules/type_select)).

% Mergers
:- use_module(library(ag_modules/map_merger)).
