:- module(conf_ag_modules, []).


% Modules that can be used as matchers and as alignment filters/selecters
:- use_module(library(ag_modules/exact_label_match)).
:- use_module(library(ag_modules/snowball_match)).
:- use_module(library(ag_modules/isub_match)).

% Alignment filters/selecters 
:- use_module(library(ag_modules/select1_1)).  % I think we should deprecate this, is really select N-1
:- use_module(library(ag_modules/arity_select)).

% Vocubalry filters/selecters 
:- use_module(library(ag_modules/voc_exclude)).
%
% Mergers
:- use_module(library(ag_modules/map_merger)).
