:- module(eq, []).

:- use_module(library(opmv_schema)).
:- use_module(eq_selecter).
:- use_module(eq_builder).
:- use_module(eq_analyser).
:- use_module(eq_evaluater).
:- use_module(eq_publisher).

% add local web directories from which static files are served.

:- prolog_load_context(directory, Dir),
   asserta(user:file_search_path(eq, Dir)).
:- asserta(user:file_search_path(css, eq(web/css))).
:- asserta(user:file_search_path(js, eq(web/js))).
:- asserta(user:file_search_path(icon, eq(web/icon))).
