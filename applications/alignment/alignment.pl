:- module(ag_app_alignment,
	  [
	  ]).

/** <module> Amalgame alignment services

This module provides (all private) HTTP handlers for alignment-centric
pages and services.

*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(semweb/rdf_db)).

:- use_module(user(user_db)).
:- use_module(components(messages)).

:- use_module(amalgame_apps(vocabularies/components)).
:- use_module(amalgame_apps(align_stats)).

:- use_module(amalgame(matchers/skosmatcher)).
:- use_module(amalgame(mappings/alignment)).
:- use_module(amalgame(mappings/opm)).


:- http_handler(amalgame(align_form),       http_align_form,     []).
:- http_handler(amalgame(align_vocs),       http_align_vocs,     []).

%%	http_align_form(+Request) is det.
%

http_align_form(Request) :-
	http_parameters(Request,
			[source(Source, [description('URI of source ConceptScheme')]),
			 target(Target,[description('URI of target ConceptScheme')])
			]),
	reply_html_page(cliopatria(default),
			[title('Align two SKOS vocabularies')
			],
			[ h4('Align the following two vocabularies:'),
			  table([
				 class(ag_skosvoctable)],
				[
				 \voctable_header,
				 \show_schemes([Source,Target], 1, [0, 0, 0, 0, 0])
				]),
			    ul([class(ag_align_actions)],
			       [
				\li_align(Source, Target)
			       ])
			]).

http_align_vocs(Request) :-
	http_parameters(Request,
			[source(Source, [description('URI of source ConceptScheme')]),
			 target(Target,[description('URI of target ConceptScheme')]),
			 graph(Graph, [description('URI of named graph containing alignment')]),
			 case_sensitive(CaseSensitive, [boolean, description('Matching type')]),
			 sourcelabel(SourceLabel, [oneof([any, pref, alt])]),
			 targetlabel(TargetLabel, [oneof([any, pref, alt])])
			]),
	(   rdf_graph(Graph)
	->  rdf_unload(Graph)
	;   true
	),
	rdf_retractall(Graph,_,_,amalgame),
	match_label_prop(SourceLabel, SourceLabelProp),
	match_label_prop(TargetLabel, TargetLabelProp),
	align(Source, Target,
	      [graph(Graph),
	       case_sensitive(CaseSensitive),
	       sourcelabel(SourceLabelProp),
	       targetlabel(TargetLabelProp)
	      ]),
	align_ensure_stats(all(Graph)),

	reply_html_page(cliopatria(default),
			title('Amalgame: selecting unique alignments from graph'),
			[ \show_alignment_overview(Graph)]).

match_label_prop(any,  P) :- rdf_equal(rdfs:label, P).
match_label_prop(pref, P) :- rdf_equal(skos:prefLabel, P).
match_label_prop(alt,  P) :- rdf_equal(skos:alyLabel, P).

li_align(Source, Target) -->
	{
	 http_link_to_id(http_align_vocs, [], AlignLink),
	 Base=align,
	 reset_gensym(Base),
	 repeat,
	 gensym(Base, AlignGraph),
	 \+ rdf_graph(AlignGraph),!
	},
	html(li([],
		[form([action(AlignLink)],
		      [input([type(hidden),name(source),  value(Source)],[]),
		       input([type(hidden),name(target), value(Target)],[]),
		       input([type(submit), class(submit), value('Align')], []),
		       ' above vocabularies',
		       ul([],[
			      li([], ['into graph ',
				      input([type(text), class(aligngraph),
					     name(graph), size(10), value(AlignGraph)],[])
				     ]),
			      li([], [
				      ' matching case senstive: ',
				      select([name(case_sensitive)],
					     [option([value(false), selected(selected)],['false']),
					      option([value(true)],['true'])
					     ])
				     ]),
			      li([],[ ' matching source label type: ',
				      select([name(sourcelabel)],
					     [
					      option([value(any), selected(selected)], 'all labels'),
					      option([value(pref)], 'Preferred labels only'),
					      option([value(alt)], 'Alternative labels only')
					     ])
				    ]),
			      li([],[ ' matching target label type: ',
				      select([name(targetlabel)],
					     [
					      option([value(any), selected(selected)], 'all labels'),
					      option([value(pref)], 'Preferred labels only'),
					      option([value(alt)], 'Alternative labels only')
					     ])
				    ])
			     ])
		      ]
		     )
		]
	       )
	    ).

align(Source, Target, Options) :-
	findall(SourceConcept, rdf(SourceConcept, skos:inScheme, Source), SourceConcepts),
	forall(member(SourceConcept, SourceConcepts),
	       rdf_transaction(skos_find_candidates(SourceConcept,
                                                    Target,
						    [candidate_matchers([labelmatch])|Options]))
              ),
	option(graph(Graph), Options, align),
	rdf_bnode(Process),
	rdf_assert(Graph, rdf:type, amalgame:'AmalgameAlignment', Graph),
	rdf_assert(Process, rdfs:label, literal('amalgame skos:matcher:skos_find_candidates/3'), Graph),
	opm_was_generated_by(Process, Graph, Graph, [was_derived_from([Source, Target])]).



