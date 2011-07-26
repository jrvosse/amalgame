:- module(eq_publisher, []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_host)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/js_write)).
:- use_module(library(yui3_beta)).
:- use_module(user(user_db)).

:- use_module(library(amalgame/expand_graph)).

:- use_module(eq_util).
:- use_module(controls).

:- multifile
	eq:menu_item/2.

:- setting(default_namespace, atom, 'http://example.com/',
	   'Default namespace to use on alignment results. Can be changed later.').

% http handlers for this applications
:- http_handler(amalgame(publish),      http_eq_publish, []).
:- http_handler(amalgame(publish_form), http_eq_publish_form, []).

eq:menu_item(http_eq_publish_form, 'publish').

%%	http_eq_publish_form(+Request)
%
%	HTTP handler for web page with interactive vocabulary alignment
%	exporter.

http_eq_publish_form(Request) :-
	authorized(write(default, _)),
	http_parameters(Request,
			[ alignment(Alignment,
				    [uri,
				     description('URI of an alignment workflow')])
			]),
	html_page(Alignment).

http_eq_publish(Request) :-
	http_parameters(Request,
			[ alignment(Alignment,
				    [uri,
				     description('URI of an alignment workflow')]),
			  status(Status, [uri, description('amalgame:status value')])
			]),

	expand_file_search_path(alignment_results(.), L),
	exists_directory(L),
	absolute_file_name(L,BaseDir),!,
	file_base_name(Alignment, AlignmentB),
	atomic_list_concat([BaseDir, AlignmentB], '/', Dir),
	atomic_list_concat(['file:/', BaseDir, AlignmentB], '/', FileDir),
	http_absolute_location(alignment_results(AlignmentB/'void.ttl'), VoidLink, []),
	save_mappings(Alignment, Dir, [status(Status)]),

	reply_html_page(equalizer(main),
			[ title(['Saved alignments'])
			],
			[\html_requires(css('eq.css')),
			 div([
			      'Saved alignments in server directory ',
			      a([href(VoidLink)], FileDir)
			     ])
			]).

		 /*******************************
		 *	      HTML		*
		 *******************************/

%%	html_page(+Alignment)
%
%	Emit html page with layout for the alignment exporter
%	application.

html_page(Alignment) :-
	html_set_options([dialect(html)]),
	findall(R, status_option(R), StatusOptions),
	reply_html_page(equalizer(main),
			[ title(['Align vocabularies'])
			],
			[
			  \html_requires(css('eq.css')),
			 div(class('yui3-skin-sam yui-skin-sam'),
			      [ \html_eq_header(http_eq_publish_form, Alignment),
				div([id(main)],
				    [ form([id(export_form), action(location_by_id(http_eq_publish))],
					   ['Publish ',
					    input([type(hidden), name(alignment), value(Alignment)]),
					    select([name(status), autocomplete(off)],
						   [ option(value('all')),
						     \html_options(StatusOptions)]),
					    ' mappings.',
					    button([type(submit)],'Go')
					   ])
				    ])
			      ])

			]).
