:- module(ag_publisher, []).

:- use_module(user(user_db)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/ag_publish)).
:- use_module(library(amalgame/map)).
:- use_module(library(amalgame/util)).
:- use_module(components(amalgame/util)).

:- multifile
	ag:menu_item/2.

:- setting(amalgame:default_publish_namespace, atom, 'http://localhost/ns/',
	   'Default namespace to use on alignment results. Can be changed later.').

% http handlers for this applications
:- http_handler(amalgame(app/publish),	http_ag_publish, []).
:- http_handler(amalgame(form/publish), http_ag_publish_form, []).

ag:menu_item(280=http_ag_publish_form, 'publish').

%%	http_ag_publish_form(+Request)
%
%	HTTP handler for web page with interactive vocabulary alignment
%	exporter.

http_ag_publish_form(Request) :-
	authorized(write(default, _)),
	http_parameters(Request,
			[ strategy(Strategy,
				    [uri,
				     description('URI of an alignment strategy')]),
			  focus(Focus,
				[uri,
				 description('URI of current focus node'),
				 default(Strategy)
				])
			]),
	html_page(Strategy, Focus).

http_ag_publish(Request) :-
	http_parameters(Request,
			[ strategy(Strategy,
				    [uri,
				     description('URI of an alignment strategy')]),
			  status(Status, [uri, description('amalgame:status value')]),
			  format(Format, [one_of([both, simple, edoal]), description('Format to publish in')]),
			  default_relation(DefaultRelation,
					   [uri,
					    description('URI of the default mapping relation, to be used if no other relation has been assigned')])

			]),

	expand_file_search_path(alignment_results(.), L),
	exists_directory(L),
	absolute_file_name(L,BaseDir),!,
	file_base_name(Strategy, AlignmentB),
	atomic_list_concat([BaseDir, AlignmentB], '/', Dir),
	save_mappings(Strategy, Dir, [status(Status), format(Format),default_relation(DefaultRelation)]),
	http_redirect(moved, alignment_results(AlignmentB), Request).


		 /*******************************
		 *	      HTML		*
		 *******************************/

%%	html_page(+Strategy)
%
%	Emit html page with layout for the alignment strategy exporter
%	application.

html_page(Strategy, Focus) :-
	findall(R, status_option(R), StatusOptions),
	rdf_equal(amalgame:final, DefaultStatus),
	supported_map_relations(MapRelations),
	rdf_equal(skos:closeMatch, DefaultRelationIfNoneGiven),
	reply_html_page(amalgame(app),
			[ title(['Align vocabularies'])
			],
			[ \html_requires(css('publisher.css')),
			  div(class(publisher),
			      [ \html_ag_header(
				     [active(http_ag_publish_form),
				      focus(Focus),
				      strategy(Strategy)]),
				div([id(main), class('yui3-g')],
				    [ form([class('yui3-u'),
					    id(export_form),
					    action(location_by_id(http_ag_publish)), method(post)],
					   ['Publish the strategy file along with ',
					    input([type(hidden), name(strategy), value(Strategy)]),
					    select([name(status), autocomplete(off)],
						   [ \html_options([no,all|StatusOptions],DefaultStatus)]),
					    ' mappings.',
					    br([]),
					    'Default map relation to use when no relation is specified: ',
					    select([name(default_relation)],
						   [
						   \html_options(MapRelations, DefaultRelationIfNoneGiven)]),
					    br([]),
					    'RDF format to use:',
					    select([name(format)],
						   [
						    option([value(both)], 'EDOAL and simple flat triples'),
						    option([value(edoal)], 'EDOAL cells only'),
						    option([value(simple), selected(selected)], 'Simple mapping triples only')
						   ]),

					    button([type(submit)],'Go')
					   ])
				    ])
			      ])

			]).
