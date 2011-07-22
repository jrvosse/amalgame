:- module(eq, []).

:- use_module(library(http/http_path)).
:- use_module(library(http/http_dispatch)).

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
:- asserta(user:file_search_path(alignment_results, web(alignment_results))).

:- multifile
	http:location/3.		% Alias, Expansion, Options
:- dynamic
	http:location/3.		% Alias, Expansion, Options

http:location(alignment_results,    root(alignment_results),    [ priority(-100) ]).

:- multifile
	user:file_search_path/2.
:- dynamic
	user:file_search_path/2.

user:file_search_path(alignment_results, library('http/web/alignment_results')).
:- http_handler(alignment_results(.), serve_files_in_directory(alignment_results), [prefix]).

serve_files_in_directory(Alias, Request) :-
	memberchk(path_info(PathInfo), Request),
	Term =.. [Alias,PathInfo],
	http_reply_file(Term, [], Request).
