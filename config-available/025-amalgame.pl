:- module(conf_amalgame, []).

/** <module> Setup amalgame
*/

:- current_prolog_flag(version, Version),
   (   Version < 60318
   ->  format('Warning: amalgame needs SWI-Prolog version 6.3.18 or higher~n', [])
   ;   true
   ).

:- use_module(library(semweb/rdf_library)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_dirindex)).
:- use_module(library(http/http_server_files)).

:- use_module(library(void_schema)).
:- use_module(library(prov_schema)).
:- use_module(library(skos_schema)).
:- use_module(library(skos_xl_schema)).

:- multifile
	http:location/3.		% Alias, Expansion, Options
:- dynamic
	http:location/3.		% Alias, Expansion, Options

http:location(amalgame,          cliopatria(amalgame),       []).
http:location(alignment_results, root(alignment_results),    [ priority(-100) ]).
http:location(img,		 root(img),                  [ priority(-100) ]).

:- rdf_attach_library(amalgame(rdf)).
:- rdf_load_library(amalgame).
:- rdf_load_library(dc).

/* Now all namespaces should have been defined, we can load the amalgame code: */
:- use_module(applications(startpage)).
:- use_module(applications(builder)).
:- use_module(applications(analyser)).
:- use_module(applications(evaluater)).
:- use_module(applications(publisher)).
:- use_module(applications(strategy_viz)).

% :- use_module(api(http_mapping)).

% add local web directories from which static files are served.

user:file_search_path(alignment_results, web(alignment_results)).
user:file_search_path(img, web(img)).

:- http_handler(alignment_results(.),
		serve_static(alignment_results), [prefix]).
:- http_handler(img(.),
		serve_files_in_directory(img),	 [prefix]).

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
