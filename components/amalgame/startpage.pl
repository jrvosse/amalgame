:- module(ag_component_startpage,
	  [ html_new//1, % +Schemes
	    html_open//1, % +Strategies
	    html_publish//1, % +Strategies
	    html_reference//0,
	    html_import//0
	  ]).

:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).

:- use_module(library(semweb/rdf_label)).
:- use_module(components(label)).

:- use_module(library(amalgame/util)).
:- use_module(library(amalgame/rdf_util)).
:- use_module(library(amalgame/ag_stats)).
:- use_module(library(amalgame/expand_graph)).
:- use_module(components(amalgame/util)).

html_new([]) -->
	html_acc_item(new,
		      'new alignment strategy: no SKOS ConceptSchemes found',
		      div([style('padding: 1%')],[
			  'Please use the Repository drop-down menu to load ',
			  'the ConceptSchemes you would like to align ',
			  'into the repository/triple store. ',
			  'Alternatively, you might want to enable the consider_all_labeled_resources setting to be able to',
			  'align any rdfs:labeled RDF resource in any named graph (see Admin/Settings menu).'
		      ]),
		      [active]
		     ).

html_new(Schemes) -->
	{ has_write_permission, !,
	  ButtonsBottom = div(\html_submit('Start')),
	  length(Schemes, N),
	  (   N > 7
	  ->  ButtonsTop = ButtonsBottom
	  ;   ButtonsTop = div([],[])
	  )
	},
	html_acc_item(new,
		      'new alignment strategy',
		      [ form([ method('POST'),
			       action(location_by_id(http_ag_form_new_strategy))],
			     [  ButtonsTop,
				\html_vocab_table(Schemes),
				ButtonsBottom
			     ])
		      ],
		      [active]
		     ).

html_new(_) -->
	{
	 http_location_by_id(http_amalgame_main_page, This),
	 http_link_to_id(cliopatria_openid:login_page,
			 ['openid.return_to'(This)], Login)
	},
	html_acc_item(new,
		      'please login to access other functions',
		      [
			  div(a([class(login), href(Login)], ['login']))
		      ],
		      [inactive]
		     ).

%%	html_open(+StrategySchemePairs)
%
%
html_open([]) -->
	html_acc_item(open,
		      div([style('font-style: italic; color: gray')],
			  'no strategies have been created yet'),
		      [],
		      [inactive]),
	!.
html_open(Strategies) -->
	{ ButtonsBottom = div([ \html_submit('View selected'),
			  \html_submit('Merge selected'),
			  \html_submit('Delete selected')
			]),
	  length(Strategies, N),
	  (   N > 7
	  ->  ButtonsTop = ButtonsBottom
	  ;   ButtonsTop = div([],[])
	  )
	},
	html_acc_item(open,
		      'edit/delete pre-loaded alignment strategy',
		      [ form([ method('POST'),
			       action(location_by_id(http_ag_form_select_strategy))],
			     [
				 ButtonsTop,
				 \html_strategy_table(Strategies,
						       [linkto(http_ag_build)]),
				 ButtonsBottom

			     ])
		      ],
		      [active]
		     ).
html_publish([]) -->
	html_acc_item(open,
		      div([style('font-style: italic; color: gray')],
			  'no mappings have been created yet'),
		      [],
		      [inactive]),
	!.
html_publish(Strategies) -->
	{
	 has_write_permission,
	 L=http_ag_publish_form,
	 !
	},
	html_acc_item(publish,
		      'publish alignment strategy results',
		      [ form([method('POST'),
			      action(location_by_id(L))],
			     [ \html_strategy_table(Strategies, [linkto(L)]),
			       \html_submit('Publish')
			     ])
		      ],
		      [inactive]).
html_publish(_) -->  !.


html_reference -->
	{ has_write_permission,
	  !
	},
	html_acc_item(reference,
		      'upload existing/reference alignment',
		      form([action(location_by_id(http_ag_form_upload_reference)),
			    method('POST'),
			    enctype('multipart/form-data') ],
			   [ p(['Upload an exisiting alignment to build upon, ',
				'or to use as reference (ground truth)' ]),
			     input([type(file), name(data),
				    size(50), autocomplete(off)
				   ]),
			     input([type(submit), value('Upload')])
			   ]),
		      [inactive]
		     ).

html_reference --> !.

html_import -->
	{
	 has_write_permission,
	 !
	},
	html_acc_item(import,
		      'upload strategy or clone execution trace',
		      [ form([action(location_by_id(http_ag_form_upload_strategy_resource)),
			      method('POST')
			     ],
			     [ 'URL: ',
			       input([type(text), name(url), value('http://'),
				      autocomplete(off), size(50)
				     ]),
			       input([type(submit), value('Upload')])
			   ]),
			form([action(location_by_id(http_ag_form_upload_strategy_data)),
			      method('POST'),
			      enctype('multipart/form-data')
			     ],
			     [ 'File: ',
			       input([type(file), name(data),
				      size(50)%, autocomplete(off)
				     ]),
			       input([type(submit), value('Upload')])
			     ])
		      ],
		      [inactive]).

html_import --> !.

/* Helper rules to implement the stuff above: */

html_vocab_table(Vs) -->
	html([
	    \html_requires(sortable),
	    table([class(sortable)],
		   [thead(tr(\html_vocab_head)),
		    tbody(\html_vocab_rows(Vs))
		   ])
	]).

html_vocab_head -->
	html([th([]),
	      th(class(name),       name),
	      th(class(version),    version),
	      th(class(count),     '(estimated) #concepts'),
	      th(class(preflangs), 'prefLabel langs'),
	      th(class(altlangs),  'altLabels langs')
	     ]).

html_vocab_rows([]) --> !.
html_vocab_rows([Scheme|Vs]) --> {
    (	node_stats(_Strategy, Scheme, Stats, [compute(false)])
    ->	option(totalCount(ConceptCount), Stats),
	option('@properties'(PDict), Stats, pdict{}),
	rdf_equal(skos:prefLabel, PL),
	rdf_equal(skos:altLabel, AL),
	(   get_dict(PL, PDict, PrefLangs)-> true; PrefLangs = []),
	(   get_dict(AL, PDict, AltLangs)-> true;   AltLangs = []),
	option(version(Version0), Stats),
	(   Version0 == ''
	->  option(revision(Version), Stats, '')
	;   Version = Version0
	)
    ;   rdf_estimate_complexity(_, skos:inScheme, Scheme, ConceptCount),
	precompute_node(_Strategy, Scheme),
	PrefLangs = [?], AltLangs = [?], Version = '?'
    )
},
	html([
	    tr([td(input([type(checkbox), autocomplete(off), class(option),
			  name(scheme), value(Scheme)])),
		td(class(name),    \html_scheme_name(Scheme)),
		td(class(version), Version),
		td(class(count), ConceptCount),
		td([span(class(preflangs), \html_showlist(PrefLangs))]),
		td([span(class(altlangs),  \html_showlist(AltLangs) )])
	       ])
	]),
	html_vocab_rows(Vs).

html_vocab_rows([Scheme|Vs]) -->
	html(tr(td(['error: ', Scheme]))),
	html_vocab_rows(Vs).


%%	html_strategy_table(+Graphs, +Options)
%
%	Emit HTML table with strategy graph properties.

html_strategy_table(Strategies, Options) -->
	html([
	    \html_requires(sortable),
	    table(
		[class(sortable)],
		[ thead(tr(\html_alignment_head)),
		  tbody(\html_alignment_rows(Strategies, Options))
		])
	]).

html_alignment_head -->
	html([th([]),
	      th(name),
	      th(includes),
	      th('Created by:'),
	      th('Comment:')
	     ]).

html_alignment_rows([],_) --> !.
html_alignment_rows([URI-Schemes|Gs], Options) -->
	{
	 (   rdf(URI, dcterms:creator, Author, URI)
	 ->  true
	 ;   Author = anonymous
	 ),
	 (   rdf(URI, rdfs:comment, CommentR, URI)
	 ->  literal_text(CommentR, Comment)
	 ;   Comment = ''
	 )
	},
	html(tr([td(input([type(checkbox), autocomplete(off), class(option), name(strategy), value(URI)])),
		 td(\html_strategy_name(URI, Options)),
		 td(\html_scheme_labels(Schemes)),
		 td(\turtle_label(Author)),
		 td([class(comment)],Comment)
		])),
	html_alignment_rows(Gs, Options).

html_scheme_labels([]) --> !.
html_scheme_labels([S|Ss]) -->
	html(div(\turtle_label(S))),
	html_scheme_labels(Ss).

html_strategy_name(Graph, Options) -->
	{ rdf_graph_label(Graph, Label),
	  option(linkto(LinkTo), Options, http_ag_build),
	  http_link_to_id(LinkTo, [strategy(Graph)], Link)
	},
	html(a([href(Link)],Label)).

html_scheme_name(Graph) -->
	{ rdf_graph_label(Graph, Label),
	  http_link_to_id(http_skos_browser, [scheme(Graph)], Link)
	},
	html(a([href(Link)],Label)).


%%	html_submit(+Label)
%
%

html_submit(Label) -->
	html(span(class(controls),
		 [ input([type(submit), autocomplete(off), class(start),
			  name(submit), disabled(true), value(Label)])
		 ])).


%%	html_acc_item(+Id, +Label, +HTMLBody, +Options)
%
%	Emit html markup for a YUI3 accordion item.
%	Options:
%
%	* active/inactive

html_acc_item(Id, Label, Body, Options) -->
	{ (   option(active, Options)
	  ->  Class = 'yui3-accordion-item yui3-accordion-item-active'
	  ;   Class = 'yui3-accordion-item'
	  )
	},
	html(div([class(Class), id(Id)],
		 [ div(class('yui3-accordion-item-hd'),
		       a([href('javascript:{}'), class('yui3-accordion-item-trigger')],
			   Label)),
		   div(class('yui3-accordion-item-bd'),
		       Body)
		 ])).


