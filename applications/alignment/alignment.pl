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

:- use_module(library(skos/vocabularies)).
:- use_module(applications(vocabularies/components)).
:- use_module(applications(align_stats)).

:- use_module(library(amalgame/matchers/skosmatcher)).
:- use_module(library(amalgame/alignment)).
:- use_module(library(amalgame/opm)).
:- use_module(library(amalgame/edoal)).

:- http_handler(amalgame(align_form),       http_align_form,     []).
:- http_handler(amalgame(align_vocs),       http_align_vocs,     []).

%%	http_align_form(+Request) is det.
%

http_align_form(Request) :-
	http_parameters(Request,
			[source(Source, [description('URI of source ConceptScheme')]),
			 target(Target,[description('URI of target ConceptScheme')])
			]),
	voc_ensure_stats(all(Source)),
	voc_ensure_stats(all(Target)),

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
			 include_qualifier(InclQualifier, [boolean, description('Include qualifier')]),
                         sourcelabel(SourceLabel, [oneof([any, pref, alt, def])]),
			 targetlabel(TargetLabel, [oneof([any, pref, alt, def])]),
			 label_lang(Language, []), % VIC: language
			 matchacross_lang(MatchAcross, [boolean,description('Match across languages')])
			]),
	authorized(write(default, create(Graph))),
	(   rdf_graph(Graph)
	->  rdf_unload(Graph)
	;   true
	),
	rdf_retractall(Graph,_,_,amalgame),
	match_label_prop(SourceLabel, SourceLabelProp),
	match_label_prop(TargetLabel, TargetLabelProp),
        (   Language == any	->  Lang = []	;   Lang = [language(Language)]	), % VIC: language
	align(Source, Target,
	      [graph(Graph),
	       request(Request),
	       case_sensitive(CaseSensitive),
	       include_qualifier(InclQualifier),
	       sourcelabel(SourceLabelProp),
	       targetlabel(TargetLabelProp),
	       matchacross_lang(MatchAcross)
	      |Lang
	      ]),
	voc_clear_stats(Source),
	voc_clear_stats(Target),
	align_ensure_stats(all(Graph)),

	reply_html_page(cliopatria(default),
			title('Amalgame: selecting unique alignments from graph'),
			[ \show_alignment_overview(Graph)]).

match_label_prop(any,  P) :- rdf_equal(rdfs:label, P).
match_label_prop(pref, P) :- rdf_equal(skos:prefLabel, P).
match_label_prop(alt,  P) :- rdf_equal(skos:altLabel, P).
match_label_prop(def,  P) :- rdf_equal(skos:definition, P).


% VIC: Language selection. Note: this should probably go somewhere else.
:- dynamic
	language_cached/3.

get_languages(Source, Output) :-
	rdf_generation(Gen),
	(   language_cached(Source, Gen, Output)
	->  true
	;   retractall(language_cached(Source, _, _)),
	    get_languages_raw(Source, Output),
	    assert(language_cached(Source, Gen, Output))
	).

get_languages_raw(Source,Output):-
	findall(L,
		(rdf(A,skos:inScheme,Source),
		 rdf_has(A, rdfs:label, literal(lang(L,_)))),
		 List),
	sort(List,Sort),
	maplist(lang_option, Sort, Output).

lang_option(Code,  option([value(Code)], Language)):-
	(   iso_639(Code, Language)
	-> true
	; Language = Code).




li_align(Source, Target) -->
	{
	 get_languages(Source,LangList),
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
		       input([type(submit), class(submit), value('Find alignment candidates')], []),
		       ' for above vocabularies',
		       ul([],[
			      li([], ['put results into graph ',
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
			      li([], [
				      'include qualifier (...) : ',
				      select([name(include_qualifier)],
					     [option([value(true), selected(selected)],['true']),
					      option([value(false)],['false'])
					     ])
				     ]),

			      li([],[ ' matching source label type: ',
				      select([name(sourcelabel)],
					     [
					      option([value(any), selected(selected)], 'Alt & pref labels'),
					      option([value(pref)], 'Preferred labels only'),
					      option([value(alt)], 'Alternative labels only'),
					      option([value(def)], 'Definitions only')
					     ])
				    ]),
			      li([],[ ' matching target label type: ',
				      select([name(targetlabel)],
					     [
					      option([value(any), selected(selected)], 'Alt & pref labels'),
					      option([value(pref)], 'Preferred labels only'),
					      option([value(alt)], 'Alternative labels only'),
					      option([value(def)], 'Definitions only')

					     ])
				    ]),
			       li([],[ ' matching source label language: ',
				      select([name(label_lang)],
					     [
					      option([value(any), selected(selected)], 'all labels')|
					     LangList
					     ])
				    ]),
			       li([],[ ' match across languages: ',
				      select([name(matchacross_lang)],
					     [
					      option([value(true), selected(selected)], 'true'),
					      option([value(false)], 'false')
					     ])
				    ])


			     ])
		      ]
		     )
		]
	       )
	    ).

alignx(Source, Target, Options) :-
	findall(SourceConcept, rdf(SourceConcept, skos:inScheme, Source), SourceConcepts),
	forall(member(SourceConcept, SourceConcepts),
	       rdf_transaction(skos_find_candidates(SourceConcept, Source,
                                                    Target,
						    [candidate_matchers([labelmatch])|Options]))
              ),
	option(graph(Graph), Options, align),
	rdf_bnode(Process),
	rdf_assert(Graph, rdf:type, amalgame:'AmalgameAlignment', Graph),
	rdf_assert(Graph, amalgame:source, Source, Graph),
	rdf_assert(Graph, amalgame:target, Target, Graph),

	rdf_assert(Process, rdfs:label, literal('amalgame skos:matcher:skos_find_candidates/3'), Graph),
	opm_was_generated_by(Process, Graph, Graph, [was_derived_from([Source, Target])|Options]).

align(SourceScheme, TargetScheme, Options) :-
	findall([Source,Target],
		candidate_generator(_Alignment, SourceScheme, TargetScheme, Source, Target, [labelmatch], Options),
		CandidatesDoubles),
	sort(CandidatesDoubles, Candidates),
	findall(Source-Target-MatchUsed-MethodUsed,
		(   member([Source, Target], Candidates),
		    matcher(Source, Target, labelmatch, MatchUsed, MethodUsed, Options)
		), Cells
	       ),
	forall(member(Source-Target-MatchUsed-MethodUsed, Cells),
	       assert_cell(Source, Target, [method(MethodUsed), match(MatchUsed) | Options ])
	      ),
	option(graph(Graph), Options, align),
	rdf_bnode(Process),
	rdf_assert(Graph, rdf:type, amalgame:'AmalgameAlignment', Graph),
	rdf_assert(Graph, amalgame:source, SourceScheme, Graph),
	rdf_assert(Graph, amalgame:target, TargetScheme, Graph),

	rdf_assert(Process, rdfs:label, literal('amalgame label matcher'), Graph),
	opm_was_generated_by(Process, Graph, Graph, [was_derived_from([SourceScheme, TargetScheme])|Options]).

