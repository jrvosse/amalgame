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
:- use_module(amalgame(matchers/skosmatcher)).
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
			 graph(Graph, [description('URI of named graph containing alignment')])
			]),
	(   rdf_graph(Graph)
	->  rdf_unload(Graph)
	;   true
	),
	align(Source, Target, [graph(Graph)]),
	http_redirect(moved, location_by_id(http_list_alignments), Request).

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
		       ' into graph ',
		       input([type(text), class(aligngraph),
			     name(graph), size(10), value(AlignGraph)],[])

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



