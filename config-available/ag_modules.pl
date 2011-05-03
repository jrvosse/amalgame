:- module(conf_ag_modules, []).

user:file_search_path(ag_modules, library(ag_modules)).

:- use_module(ag_modules(exact_label_match)).
:- use_module(ag_modules(snowball_match)).
:- use_module(ag_modules(select1_1)).  % I think we should deprecate this, is really select N-1
:- use_module(ag_modules(arity_select)).
:- use_module(ag_modules(map_merger)).
