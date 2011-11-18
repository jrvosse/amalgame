:- module(eq, []).

:- use_module(library(http/http_path)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_dirindex)).


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
:- asserta(user:file_search_path(img, web(img))).

:- multifile
	http:location/3.		% Alias, Expansion, Options
:- dynamic
	http:location/3.		% Alias, Expansion, Options

http:location(alignment_results, root(alignment_results),    [ priority(-100) ]).
http:location(img,		 root(img),                  [ priority(-100) ]).

:- http_handler(alignment_results(.), serve_static(alignment_results), [prefix]).
:- http_handler(img(.),               serve_static(img),               [prefix]).

serve_static(Alias, Request) :-
	memberchk(path(PathInfo), Request),
	sub_atom(PathInfo, 1, Len, End, Alias),
	(   End < 2
	->  Path='.'
	;   debug(foo, '~w', [End]),
	    Start is Len + 2,
	    sub_atom(PathInfo, Start, _, 0, Path)
	),
	Term =.. [Alias,Path],
	(   absolute_file_name(Term, _,
			       [file_type(directory),
				access(read),
				file_errors(fail)
			       ])
	->  http_reply_dirindex(Term, [unsafe(true)], Request)
	;   http_reply_file(Term, [], Request)
	).
