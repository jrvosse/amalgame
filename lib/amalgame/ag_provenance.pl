:- module(ag_provenance,
	[
	 provenance_graph/2,
	 add_amalgame_prov/3,
	 update_amalgame_prov/2,                      % -Strategy, +Mapping
	 flush_prov_cache/0,
	 remove_old_prov/2,                           % +Process, +ProvGraph
	 is_amalgame_graph/1,

	 prov_was_generated_by/4,   % +Activity, +Entities, +Graph, +Options
	 prov_clear_activity/1,	    % +Process (bnode)
	 prov_get_entity_version/3
	]).

:- use_module(library(option)).
:- use_module(library(http/http_host)).
:- use_module(library(http/http_session)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(user(user_db)).
:- use_module(library(version)).
:- use_module(library(prov_schema)).
:- use_module(library(skos/util)).

:- use_module(ag_stats).
:- use_module(util).

:- rdf_meta
	provenance_graph(r,r),
	add_amalgame_prov(r,r,+),
	update_amalgame_prov(r,r),
	remove_old_prov(r,r),
	is_amalgame_graph(r),
	prov_was_generated_by(r,t,r,+),
	prov_clear_activity(r),
	prov_get_entity_version(r,r,-),
	prov_ensure_entity(r, r, r).

:- dynamic
	current_prov_uri/2.

flush_prov_cache :-
	retractall(current_prov_uri(_,_)),
	forall(provenance_graph(_,G), rdf_unload_graph(G)).


%%	provenance_graph(+Strategy, ?Graph) is det.
%
%	True if Graph is the provenance graph associated with strategy.

provenance_graph(Strategy, Graph) :-
	(   rdf(Graph, amalgame:hasPlan, Strategy, Graph)
	->  true
	;   ground(Strategy),
	    mint_node_uri(Strategy, provBundle, Graph),
	    create_prov_graph(Strategy, Graph)
	).

create_prov_graph(Strategy, Graph) :-
	format(atom(Label), 'Provenance graph for strategy ~p', [Strategy]),
	rdf_assert(Graph, rdf:type, prov:'Bundle', Graph),
	rdf_assert(Graph, amalgame:hasPlan, Strategy, Graph),
	rdf_assert(Graph, rdfs:label, literal(lang(en,Label)), Graph),
	% Copy Strategy triples to empty prov graph:
	findall(rdf(Strategy,P,O), rdf(Strategy,P,O,Strategy), STriples),
	forall(member(rdf(S,P,O), STriples), rdf_assert(S,P,O,Graph)).

update_amalgame_prov(Strategy, Mapping) :-
	provenance_graph(Strategy, ProvGraph),
	rdf_retractall(Mapping, _, _, ProvGraph),
	findall(rdf(Mapping,P,O),
		rdf(Mapping,P,O,Strategy),
		Triples),
	forall(member(rdf(S,P,O), Triples),
	       rdf_assert(S,P,O,ProvGraph)
	      ).

prov_ensure_entity(_S, Entity, Graph) :-
	rdf(Entity, 'http://usefulinc.com/ns/doap#revision' , _, Graph),
	!. % prov already recorded
prov_ensure_entity(Strategy, Entity, Graph) :-
	skos_is_vocabulary(Entity),
	node_stats(Strategy, Entity, Stats, []),
	option(revision(Revision), Stats),
	prov_named_graphs(Repo, Graph),
	rdf_assert(Entity, amalgame:loadedInto, Repo, Graph),
	rdf_assert(Entity, 'http://usefulinc.com/ns/doap#revision',
		   literal(Revision), Graph),
	findall(rdf(Entity, P, O), rdf(Entity, P, O), Triples),
	forall(member(rdf(S,P,O), Triples), rdf_assert(S,P,O,Graph)),
	!.
prov_ensure_entity(_S, Entity, _Graph) :-
	rdfs_individual_of(Entity, amalgame:'Mapping'), !. % do nothing, amalgame generated

prov_ensure_entity(_S, Entity, Graph) :-
	format(atom(Message),
	       'Cannot record provenance for ~p in named graph ~p',
	       [Entity, Graph]),
	throw(error(evalution_error,
		    context(prov_ensure_entity/3, Message))).

add_amalgame_prov(Strategy, Process, Results) :-
	rdf_equal(prov:used, ProvUsed),
	rdf_equal(prov:wasDerivedFrom, ProvWDF),

	provenance_graph(Strategy, ProvGraph),

	% Remove old info about Process from ProvGraph
	remove_old_prov(Process, ProvGraph),

	% Copy all triples about Process from Strategy to ProvGraph
	findall(rdf(Process, P, O), rdf(Process,P,O,Strategy), ProcessTriples),

	% Find inputs of Process
	findall(rdf(Process, ProvUsed, I),
		(   rdf_has(Process, ProvUsed, I, RealProp),
		    rdf(Process, RealProp, I, Strategy),
		    prov_ensure_entity(Strategy, I, ProvGraph)
		),
		InputTriples),


	Artifacts = Results,
	assert_counts(Results, Strategy, ProvGraph),
	prov_was_generated_by(Process, Artifacts, ProvGraph, [strategy(Strategy)]),

	% Generate prov:wasDerivedFrom triples between Mappings
	findall(rdf(Target, ProvWDF, Source),
		(   member(Target, Artifacts),
		    rdf_has(Process, prov:used, Source, RealProp),
		    rdf(Process, RealProp, S, Strategy)
		),
		DerivedTriples),

	% Copy all triples about the Mapping from Strategy to ProvGraph
	findall(rdf(S,P,O),
		(   member(S, Artifacts),
		    rdf(S,P,O,Strategy)
		), ArtifactTriples),

	append([ProcessTriples,
		ArtifactTriples,
		InputTriples,
		DerivedTriples], AllTriples),
	forall(member(rdf(S,P,O), AllTriples), rdf_assert(S,P,O,ProvGraph)).


%%	remove_old_prov(+Process, +Graph) is det.
%
%	Remove all provenance triples related to Process from Graph.
%

remove_old_prov(Process, ProvGraph) :-
	findall(Bnode,
		(   rdf(Process, _, Bnode, ProvGraph),
		    rdf_is_bnode(Bnode),
		    \+ (rdf(OtherProcess, _, Bnode, ProvGraph),
			OtherProcess \= Process),
		    \+ rdfs_individual_of(Bnode, prov:'SoftwareAgent')
		),
		Bnodes),
	forall(member(B,Bnodes), remove_old_prov(B,ProvGraph)),
	rdf_retractall(Process, _, _, ProvGraph),
	rdf_retractall(_, _ ,Process, ProvGraph).


%%     prov_was_generated_by(+Activity, +Entities, +Graph, +Options) is
%%     det.
%
%	Assert provenance information about Entities generated by
%	Activity into named Graph (all three are URLs).
%	Options is a list of options, currently implemented options
%	include:
%
%	* was_derived_from([Sources]) to indicate the entities were
%	derived from the given list of source entities
%	* request(Request) to record information about the request URI
%	used in the web service to create Entities.
%	* strategy(Strategy)

prov_was_generated_by(_, [], _, _) :- !.
prov_was_generated_by(Process, Artifacts, Graph, Options) :-
	is_list(Artifacts),!,
	rdf_assert(Process, rdf:type, prov:'Activity',	Graph),
	forall(member(Artifact, Artifacts),
	       (   rdf_assert(Artifact, rdf:type, prov:'Entity',    Graph),
		   rdf_assert(Artifact, prov:wasGeneratedBy, Process, Graph)
	       )
	      ),
	prov_program(Graph, Program),
	prov_person(Graph, Person),

	option(strategy(Strategy), Options),
	prov_association(Program, Strategy, Graph, ProgramAssociation),
	prov_association(Person,  Strategy, Graph, PersonAssociation),

	rdf_assert(Process, prov:wasAssociatedWith, Program, Graph),
	rdf_assert(Process, prov:wasAssociatedWith, Person,   Graph),
	rdf_assert(Process, prov:qualifiedAssociation,  ProgramAssociation, Graph),
	rdf_assert(Process, prov:qualifiedAssociation,  PersonAssociation,  Graph),

	get_time(Now),
	get_xml_dateTime(Now, NowXML),
	rdf_assert(Process, prov:endedAtTime,   literal(type(xsd:dateTime, NowXML)) , Graph),

	(   memberchk(was_derived_from(Sources), Options)
	->  forall(member(Source, Sources),
		   (   forall(member(Artifact, Artifacts),
			     rdf_assert(Artifact, prov:wasDerivedFrom,  Source,  Graph)
			     ),
		       rdf_assert(Process, prov:used, Source, Graph),
		       (   \+ rdfs_individual_of(Source, prov:'Entity')
		       ->  rdf_assert(Source, rdf:type,	prov:'Entity', Graph)
		       ;   true
		       )
		   )
		  )
	;   true
	),
	(   memberchk(request(Request), Options)
	->  http_current_host(Request, Hostname, Port, [global(true)]),
	    memberchk(request_uri(ReqURI), Request),
	    memberchk(protocol(Protocol), Request),
	    format(atom(ReqUsed), '~w://~w:~w~w', [Protocol,Hostname,Port,ReqURI]),
	    rdf_assert(Process, amalgame:request, ReqUsed, Graph)
	;   true
	),
	true.

prov_was_generated_by(Process, Artifact, Graph, Options) :-
	atom(Artifact),!,
	prov_was_generated_by(Process, [Artifact], Graph, Options).

prov_clear_activity(Activity) :-
	rdf_retractall(Activity, _, _, _),
	rdf_retractall(_, _, Activity, _).

prov_program(Graph, Program) :-
	current_prov_uri(Graph, program(Program)),!.

prov_program(Graph, Program)  :-
	(   current_prolog_flag(version_git, PL_version)
	->  true
	;   current_prolog_flag(version, PL_version)
	),
	working_directory(CWD,CWD),
	file_name_to_url(CWD,CWDF),
	gethostname(LocalHost),
	findall(M-U-V-F,
		(   git_module_property(M, home_url(U)),
		    git_module_property(M, version(V)),
		    git_module_property(M, directory(D)),
		    file_name_to_url(D, F)
		),
		MUVs
	       ),
	Prolog = 'swi-prolog'-'http://www.swi-prolog.org'-PL_version,
	All = [Prolog|MUVs],
	variant_sha1(All, Hash),
	atomic_list_concat(['http://localhost/ns/amalgame/version/x', Hash], Program),
	assert(current_prov_uri(Graph, program(Program))),
	rdf_assert(Program, rdfs:label, literal('Amalgame alignment platform'), Graph),
	rdf_assert(Program, rdf:type,   prov:'SoftwareAgent', Graph),
	rdf_assert(Program, amalgame:cwd, CWDF, Graph),
	rdf_assert(Program, amalgame:host, literal(LocalHost), Graph),
	forall(member(M-U-V-D, All),
	       (   rdf_bnode(B),
	           rdf_assert(Program, amalgame:component, B, Graph),
		   rdf_assert(B, 'http://usefulinc.com/ns/doap#revision',
			      literal(V), Graph),
		   rdf_assert(B, 'http://usefulinc.com/ns/doap#name',
			      literal(M), Graph),
		   rdf_assert(B, rdfs:seeAlso,
			      literal(D), Graph),
		   rdf_assert(B, rdfs:seeAlso,
			      literal(U), Graph)
	       )
	      ),
	!.

prov_person(Graph, Person) :-
	current_prov_uri(Graph, person(Person)),!.

prov_person(Graph, Person) :-
	(
	http_in_session(_)
	->
	   logged_on(User, anonymous),
	   user_property(User, url(Person)),
	   (   user_property(User, realname(UserName))
	   ->  true
	   ;   user_property(User, openid(UserName))
	   ->  true
	   ;   UserName = Person
	   )
	;
	 rdf_bnode(Person),
	 UserName = 'anonymous user (not logged in)'
	),
	assert(current_prov_uri(Graph, person(Person))),
	rdf_assert(Person, rdfs:label, literal(UserName),  Graph),
	rdf_assert(Person, rdf:type,   prov:'Person',	  Graph).

prov_association(Agent, _Strategy, _Graph, Association):-
	current_prov_uri(Agent, association(Association)), !.
prov_association(Agent, Strategy, Graph, Association):-
	(   rdfs_individual_of(Agent, prov:'Person')
	->  rdf_equal(Role, amalgame:user_executing_strategy)
	;   rdf_equal(Role, amalgame:program_executing_strategy)
	),
	rdf_bnode(Association),
	rdf_assert(Association, rdf:type, prov:'Association', Graph),
	rdf_assert(Association, prov:agent, Agent, Graph),
	rdf_assert(Association, prov:hadPlan, Strategy, Graph),
	rdf_assert(Association, prov:hadRole, Role, Graph),
	assert(current_prov_uri(Agent, association(Association))).

repo_version_id(Repo) :-
	findall(G-Hash,
		(   rdf_graph(G),
		    \+ is_amalgame_graph(G),
		    rdf_graph_property(G, hash(Hash))
		),
		Graphs),
	variant_sha1(Graphs, SHA1),
	atomic_concat('http://localhost/ns/cliopatria/triplestore/sha1_',
		      SHA1, Repo).

prov_named_graphs(Repo, Graph) :-
	repo_version_id(Repo),
	current_prov_uri(Graph, repository(Repo)), !.

prov_named_graphs(Repo, Graph) :-
	repo_version_id(Repo),
	findall(G, (rdf_graph(G), \+ is_amalgame_graph(G)), Graphs),
	rdf_assert(Repo, rdf:type, amalgame:'TripleStore', Graph),
	forall(member(G, Graphs),
	       prov_named_graph(G, Repo, Graph)),
	assert(current_prov_uri(Graph, repository(Repo))).

prov_named_graph(NG, Repo, Graph) :-
	rdf_graph_property(NG, modified(NGModified)),
	rdf_graph_property(NG, hash(NGHash)),
	rdf_graph_property(NG, triples(NGCount)),
	(   rdf_graph_property(NG, source(NGsource)), NG \= NGsource
	->  rdf_assert(NG, amalgame:localFileName, NGsource, Graph)
	;   true
	),
	(   rdf_graph_property(NG, source_last_modified(NGsource_lm0))
	->  get_xml_dateTime(NGsource_lm0, NGsource_lm),
	    rdf_assert(NG, amalgame:source_last_modified,
		       literal(type(xsd:dateTime, NGsource_lm)), Graph)
	;   true
	),
	rdf_assert(Repo, amalgame:loaded, NG, Graph),

	rdf_assert(NG, amalgame:hash, literal(NGHash), Graph),
	rdf_assert(NG, amalgame:modified_after_loading, literal(NGModified), Graph),
	rdf_assert(NG, amalgame:triples, literal(type(xsd:int, NGCount)), Graph),
	rdf_assert(NG, rdfs:comment, literal(lang(en, 'This named graph was loaded into the triple store during the alignment process. It may or may not have influenced the results.')), Graph).

get_xml_dateTime(T, TimeStamp) :-
	format_time(atom(TimeStamp), '%Y-%m-%dT%H-%M-%S%Oz', T).

%%	prov_get_entity_version(+Entity,+SourceGraph,Version)
%	is semidet.
%
%	Assert (git) version information about Entity into the named
%	graph TargetGraph. SourceGraph is the main named graph in which
%	Entity is defined.

prov_get_entity_version(Entity, SourceGraph, Version) :-
	rdf_graph_property(SourceGraph, source(SourceFileURL)),
	uri_file_name(SourceFileURL, Filename),
	file_directory_name(Filename, Dirname),
	(   catch(register_git_module(Entity, [directory(Dirname), home_url(Entity)]),Error,
              ( print_message(error, Error),
                fail
              )),
	    git_module_property(Entity, version(GitVersion))
	->  format(atom(Version),  'GIT version: ~w', [GitVersion])
	;   rdf_graph_property(SourceGraph, hash(Hash)),
	    rdf_graph_property(SourceGraph, source_last_modified(LastModified)),
	    format_time(atom(Time), 'Last-Modified: %Y-%m-%dT%H-%M-%S%Oz', LastModified),
	    format(atom(Version), '~w hash: ~w', [Time, Hash])
	).

assert_counts([],_,_).
assert_counts([URI|Tail], Strategy, ProvGraph) :-
	assert_count(URI, Strategy, ProvGraph),
	assert_counts(Tail, Strategy, ProvGraph).

assert_count(VocUri, Strategy, ProvGraph) :-
	skos_is_vocabulary(VocUri),
	node_stats(Strategy, VocUri, Stats, []),
	option(totalCount(Count), Stats),
	rdf_retractall(VocUri, amalgame:totalCount, _, ProvGraph),
	rdf_assert(VocUri, amalgame:totalCount, literal(type(xsd:int, Count)), ProvGraph).

assert_count(MapUri, Strategy, ProvGraph) :-
	rdfs_individual_of(MapUri, amalgame:'Mapping'),!,
	node_stats(Strategy, MapUri, MStats, [compute(false)]),
	option(totalCount(Count), MStats),
	option(mappedSourceConcepts(SN), MStats),
	option(mappedTargetConcepts(TN), MStats),
	rdf_retractall(MapUri, amalgame:totalCount, _, ProvGraph),
	rdf_retractall(MapUri, amalgame:mappedSourceConcepts, _, ProvGraph),
	rdf_retractall(MapUri, amalgame:mappedTargetConcepts, _, ProvGraph),

	rdf_assert(MapUri, amalgame:totalCount,
		   literal(type('http://www.w3.org/2001/XMLSchema#int', Count)), ProvGraph),
	rdf_assert(MapUri, amalgame:mappedSourceConcepts,
		   literal(type('http://www.w3.org/2001/XMLSchema#int', SN)), ProvGraph),
	rdf_assert(MapUri, amalgame:mappedTargetConcepts,
		   literal(type('http://www.w3.org/2001/XMLSchema#int', TN)), ProvGraph).

%%	is_amalgame_graph(?G) is nondet
%
%	True if G is a named graph created by amalgame

is_amalgame_graph(G) :-
	rdf_graph(G),
	(   rdf(G, amalgame:hasPlan, _, _) % G is provenance graph
	;   rdf(S, amalgame:hasPlan, _, G), % G is the void graph
	    S \= G
	;   rdfs_individual_of(G, amalgame:'AlignmentStrategy')
	;   once(rdf(G, align:map, _, G))	 % G is mapping graph
	;   rdf(_, amalgame:evidenceGraph, G)	 % G is an evidence graph
	).

