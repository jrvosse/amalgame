:- module(voc_stats_components,
	  [
	   show_schemes//0,
	   show_schemes//3,
	   show_scheme//1,
	   voctable_header//0
	  ]).

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).

:- use_module(components(label)).
:- use_module(components(graphviz)).
:- use_module(applications(browse)).

:- use_module(applications(alignment/alignment)).
:- use_module(amalgame(skos/vocabularies)).


%%	show_schemes// is det.
%
%	Generates an HTML table with an overview of all SKOS Concept
%	Schemes loaded in the repository, or a warning if none have been
%	loaded.

show_schemes -->
	{
	 \+  rdf_has(_Voc, skos:inScheme, _Scheme),!,
	 http_link_to_id(load_library_ontology_form, [], LoadLink)
	},
	html(p([class('error novocs')],
		 ['Warning: No SKOS concept schemes have been ',
		  a([href(LoadLink)],'loaded'),
		  ' yet.'
		 ])
	    ).

show_schemes -->
	{
	 findall(Voc, rdfs_individual_of(Voc, skos:'ConceptScheme'), SchemesDoubles),
	 sort(SchemesDoubles, Schemes),
	 http_link_to_id(http_clear_voc_stats, [], CacheLink),
	 http_link_to_id(http_compute_voc_stats, [voc(all)], ComputeLink)
	},
	html([
	      table([
		     class(ag_skosvoctable)],
		    [
		     \voctable_header,
		     \show_schemes(Schemes, 1, [0, 0, 0, 0, 0])
		    ]),
	      ul([class(ag_voc_actions)],
		 [
		  li([a([href(ComputeLink)], 'compute'), ' missing statistics.']),
		  li(a([href(CacheLink)], 'clear vocabulary statistics cache')),
		  \li_del_derived
		 ])
	     ]).


%%	voctable_header// is det.
%
%	Generates the head of the vocabulary table so it can be easily
%	reused to generate sub tables.

voctable_header -->
	html([tr([th([class(nr)],        'Nr'),
		  th([class(name)],	'Name'),
		  th([class(count)],     '# Concepts'),
		  th([class(preflabels)],'# prefLabels'),
		  th([class(altlabels)], '# altLabels'),
		  th([class(notmapped)], '# not mapped'),
		  th([class(mapped)],    '# mapped'),
		  th([class(pmapped)],   '%'),
		  th([class(example)],   'Example concept'),
		  th([class(license)],	'License')
		 ])
	     ]).

show_schemes(Graph) -->
	show_schemes([Graph], 0, [0,0,0,0,0]).

%%	show_schemes(+SchemeList, +Nr, +Countlist)// is det.
%
%	Generate the scheme table with total counts.
%	Countlist is currently a list with the counts for the total
%	number of:
%       * Concepts
%       * Preferred labels
%       * Alternative labels
%       * Concepts mapped
%       * Concepts not mapped

show_schemes([], _, [C, P, A, M , U]) -->
	html(tr([class(finalrow)],
		[
		 td([class(nr)], ''),
		 td([class(name)], 'Total'),
		 td([class(count), style('text-align: right')],C),
		 td([class(preflabels), style('text-align: right')],P),
		 td([clas(altlabels), style('text-align: right')],A),
		 td([class(notmapped), style('text-align: right')],U),
		 td([class(mapped), style('text-align: right')],M),
		 td([class(pmapped)], ''),
		 td([class(example)], ''),
		 td([class(license)], '')
		])).
show_schemes([Voc|Tail], Nr, [C,P,A,M,U]) -->
	{
	 \+ rdf_has(_Concept, skos:inScheme, Voc),
	 rdf_retractall(Voc, _, _, amalgame),
	 rdf_retractall(Voc, _, _, amalgame_vocs)
	},
	show_schemes(Tail, Nr, [C,P,A,M,U]).


show_schemes([Voc|Tail], Nr, [C,P,A,M,U]) -->
	{
	 findall(Label,
		 (   rdfs_individual_of(Voc, Class),
		     rdf_global_id(_NS:Label, Class)
		 ),
		 VocTypes),
	 sort(VocTypes, VocTypesUnique),
	 (   rdf(Voc, amalgame:subSchemeOf, _SuperScheme)
	 ->  SubSuper = subscheme
	 ;   SubSuper = superscheme
	 ),
	 atomic_list_concat([SubSuper|VocTypesUnique], ' ', VocTypesAtom),
	 http_link_to_id(http_compute_voc_stats,
			 [voc(Voc),
			  stat(numberOfConcepts),
			  stat(numberOfPrefLabels),
			  stat(numberOfAltLabels),
			  stat(numberOfMappedConcepts)
			 ],
			 MissingLink),
	 http_link_to_id(http_list_skos_voc, [voc(Voc)], VocLink),
	 rdf_display_label(Voc, VocName),
	 VocLabel = a([href(VocLink)],[VocName]),
	 MissingValue = a([href(MissingLink)],'?'),
	 NewNr is Nr + 1,
	 voc_get_computed_props(Voc, Props),
	 (   memberchk(numberOfConcepts(literal(type(_,  CCount))), Props)
	 ->  (SubSuper = superscheme
	     ->  NewC is C + CCount
	     ;	 NewC is C
	     )
	 ;   NewC = C, CCount = MissingValue
	 ),
	 (   memberchk(numberOfPrefLabels(literal(type(_,PCount))), Props)
	 ->  (SubSuper = superscheme
	     ->  NewP is P + PCount
	     ;	 NewP = P
	     )
	 ;   NewP = P, PCount = MissingValue
	 ),
	 (   memberchk(numberOfAltLabels(literal(type(_, ACount))), Props)
	 ->  (SubSuper = superscheme
	     ->  NewA is A + ACount
	     ;	 NewA is A
	     )
	 ;   NewA = A, ACount = MissingValue
	 ),
	 (   memberchk(numberOfMappedConcepts(literal(type(_, MCount))), Props)
	 ->  UCount is CCount - MCount,
	     (SubSuper = superscheme
	     ->	 NewU is U + UCount, NewM is M + MCount
	     ;	 NewU is U, NewM is M
	     ),
	     (	 CCount = 0
	     ->	 MPercent = '-'
	     ;	 Perc is 100*(MCount/CCount),
		 format(atom(MPercent), '(~2f%)', [Perc])
	     )
	 ;   NewM = M, NewU = U, MCount = MissingValue, UCount = MissingValue
	 ),
	 (rdf_has(Example, skos:inScheme, Voc)
	 ->  true
	 ;   Example = '-'
	 ),
	 (rdf_has(Voc, dcterms:rights, RightsO)
	 ->  (   RightsO = literal(_)
	     ->  literal_text(RightsO, RightsT),
		 truncate_atom(RightsT, 30, RightsTrunc),
		 http_link_to_id(list_resource, [r(Voc)], LinkToVoc),
		 Rights=a([href(LinkToVoc)], RightsTrunc)
	     ;	 Rights=a([href(RightsO)],'License')
	     )
	 ;   Rights = '-'
	 )
	},
	html(tr([class(VocTypesAtom)],[td([class(nr)], Nr),
		 td([class(name)],[VocLabel]),
		 td([class(count), style('text-align: right')],CCount),
		 td([class(preflabels), style('text-align: right')],PCount),
		 td([class(altlabels),  style('text-align: right')],ACount),
		 td([class(notmapped), style('text-align: right')],UCount),
		 td([class(mapped), style('text-align: right')],MCount),
		 td([class(pmapped), style('text-align: right')],MPercent),
		 td([class(example)],\rdf_link(Example, [resource_format(nslabel)])),
		 td([class(license)], Rights)
		])),
	show_schemes(Tail, NewNr, [NewC, NewP, NewA, NewM, NewU]).

%%	show_scheme(+Voc)// is det.
%
%	Generate HTML with detailed info about and actions on given Voc.

show_scheme(Voc) -->
	{
	 voc_ensure_stats(all(Voc)),
	 rdf_display_label(Voc, VocLabel),
	 findall(Graph, rdf(_, skos:inScheme, Voc, Graph:_), GraphsDoubles),
	 sort(GraphsDoubles, Graphs),
	 (   Graphs = [Graph], Graph \= Voc
	 ->  GraphAsResource = div([id(ag_graph_as_resource), class(component)], \graph_as_resource(Graph, []))
	 ;   GraphAsResource = ''
	 ),
	 (   Graphs = [Graph], rdf_statistics(triples_by_file(Graph, TripleCount))
	 ->  (TripleCount < 10000
	     ->	 GraphInfo  = div([id(ag_graph_info)], \graph_info(Graph))
	     ;	 GraphInfo  = div([id(ag_graph_info)], ['Triple count: ', TripleCount])
	     )
	 ;   GraphInfo = ''
	 )

	},
	(   { Graphs = [Graph]; (Graphs = [], Graph=Voc) }
	->  html([
	      h1([class(align_overview_header)],
		 ['Vocabulary actions & details: ', VocLabel]),
		  GraphAsResource,
		  GraphInfo,
		  div([class(graphviz), style('width: 70%')],
		  [\graphviz_graph(cliopatria:context_graph(Voc),
			       [ object_attributes([width('100%')]),
				 wrap_url(cpa_browse:resource_link),
				 graph_attributes([ rankdir('RL')]),
				 shape_hook(cpa_browse:shape(Graph))
			       ])
		  ]),

		  div([id(ag_graph_basic_actions), class(component)],
		   [
		    'Basic actions on (skos:inScheme) graph: ', Graph,
		    \graph_actions(Graph)
		   ])
	     ])
	;   html([
		  h1([class(align_overview_header)], ['Warning: multiple graphs problem']),
		  p([], ['Thesaurus has skos:inScheme triples in multiple files: ',
			 \show_schemes(Graphs),
			 'Please merge into one to access graph-based actions.'])
		 ])
	),
	html([
	      %p('Create a new graph from this one: '),
	      p('Other action: '),
	      ul([
		  \li_partition(Voc),
		  \li_align(Voc)
		 ]),
	      div([id(ag_voc_as_resource), class(component)],
		  [
		   \graph_as_resource(Voc,[])
		  ])
	     ]).

li_partition(Voc) -->
	{
	rdf(Voc, amalgame:numberOfConcepts, literal(type(_,0)))
	}.

li_partition(Voc) -->
	{
	 rdf(Voc, amalgame:numberOfConcepts, literal(type(_,CCount))),
	 rdf(Voc, amalgame:numberOfMappedConcepts, literal(type(_,MCount))),
	 (   (CCount = MCount; MCount =0)
	 ->  MappedPart = ''
	 ;   MappedPart = option([value(mapped)],['Mapped versus unmapped concepts'])
	 ),
	 http_link_to_id(http_partition_voc, [], SplitLink),
	 Partition = form([action(SplitLink)],
			  [input([type(hidden), name(voc), value(Voc)],[]),
			   input([type(submit), value('Partition'), class(submit)],[]),
			   ' based on ',
			   select([name(partition_method)],
			   [
			    MappedPart,
			    option([value(type)],['Type of concepts'])
			   ])
			  ])
	},
	html(li(Partition)).

li_align(Voc) -->
	{
	rdf(Voc, amalgame:numberOfConcepts, literal(type(_,0)))
	},
	html(li('Warning: This vocabulary has no associated concepts')).

li_align(Voc) -->
	{
	 findall(V,
		 (   rdfs_individual_of(V, skos:'ConceptScheme'),
		     \+ rdf_has(V, opmv:wasDerivedFrom, Voc),
		     \+ rdf_has(Voc, opmv:wasDerivedFrom, V),
		     \+ V=Voc
		 ),
		 SchemesDoubles),
	 sort(SchemesDoubles, Schemes),
	 maplist(make_option_element, Schemes, Options),
	 http_link_to_id(http_align_form, [], AlignLink)
	},
	html(li([],
		form([action(AlignLink)],
		[input([type(submit), value(align), class(submit)],[]),
		 'with',
		 select([name(target)],Options),
		 input([type(hidden), name(source), value(Voc)],[])
		]))).


make_option_element(Voc, Options) :-
	rdf_display_label(Voc, Label),
	Options = option([value(Voc)],[Label]).

li_del_derived -->
	{
	 \+ rdfs_individual_of(_, amalgame:'DerivedConceptScheme')
	},!.

li_del_derived -->
	{
	 rdfs_individual_of(_, amalgame:'DerivedConceptScheme'),
	 http_link_to_id(http_delpart_voc, [], DelPartLink)
	},
	html(li(a([href(DelPartLink)], 'delete derived concept schemes'))).
