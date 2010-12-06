:-module(ag_opm, [
		  opm_was_generated_by/4,       % +Process (cause), +Artifact (effect), +RDFGraph, +Options
		  opm_include_dependency/2
		 ]).

/* <module> OPM -- simple support for the OPM Provenance Model (OPM)

@see http://openprovenance.org/

*/

:- use_module(library(http/http_host)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(version)).
:- use_module(library(opmv_schema)).
:- use_module(library(opmvc_schema)).
:- use_module(user(user_db)).

opm_include_dependency(Graph, Target) :-
	opm_include_dependency([Graph], [], DepList),
	expand_deplist(DepList, [], Results),
	rdf_assert_list(Results, Target).

rdf_assert_list([], _).
rdf_assert_list([rdf(S,P,O)|T], Graph) :-
	rdf_assert(S,P,O,Graph),
	rdf_assert_list(T, Graph).

opm_include_dependency([], Results, Results).

opm_include_dependency([H|T], Accum, Results) :-
	opm_include_dependency(T, Accum, TailResults),
	findall(Dep, dependent(H, Dep),Deps),
	opm_include_dependency(Deps, [H], HeadResults),
	append(TailResults, HeadResults, Results).

dependent(S, Dep) :- rdf(S, opmv:wasDerivedFrom, Dep).
dependent(S, Dep) :- rdf(S, opmv:wasGeneratedBy, Dep).
dependent(S, Dep) :-
	rdfs_individual_of(S, opmv:'Process'),
	rdf(S,_,Dep),
	rdf_is_bnode(Dep).

expand_deplist([], Results, Results).
expand_deplist([H|T], Accum, Results) :-
	findall(Triple, opm_triple(H,Triple), Triples),
	append(Triples, Accum, NewAccum),
	expand_deplist(T, NewAccum, Results).

:- rdf_meta
	opm_triple(r, t).

opm_triple(S, rdf(S, opmv:wasDerivedFrom,O)) :- rdf(S, opmv:wasDerivedFrom, O).
opm_triple(S, rdf(S, opmv:wasGeneratedBy,O)) :- rdf(S, opmv:wasGeneratedBy, O).
opm_triple(S, rdf(S,P,O)) :-
	rdfs_individual_of(S, opmv:'Process'),
	rdf(S,P,O).
opm_triple(S, rdf(S,P,O)) :-
	rdf_is_bnode(S),
	(   rdfs_individual_of(S, align:'Cell') -> gtrace; true),
	rdf(S,P,O).

%%     opm_was_generated_by(+Process, +Artifact, +Graph, +Options) is
%%     det.
%
%	Assert OPM provenance information about Artifact generated by
%	Process into named Graph (all three are URLs).
%	Options is a list of options, currently implemented options
%	include:
%
%	* was_derived_from([Sources]) to indicate the artifact was
%	derived from the given list of source artifacts
%	* request(Request) to record information about the request URI
%	used in the web service to create this Artifact.
%
opm_was_generated_by(Process, Artifact, Graph, Options) :-
	rdf_assert(Artifact, rdf:type, opmv:'Artifact',	Graph),
	rdf_assert(Process,  rdf:type, opmv:'Process',	Graph),
	rdf_assert(Artifact, opmv:wasGeneratedBy, Process,  Graph),

	opm_program(Graph, Program),
	opm_agent(Graph, Agent),

	get_xml_dateTime(TimeStamp),
	rdf_bnode(BN_TimeStamp),
	rdf_assert(BN_TimeStamp, rdf:type, time:'Instant',  Graph),
	rdf_assert(BN_TimeStamp, time:inXSDDateTime, literal(type(xsd:dateTime, TimeStamp)), Graph),
	rdf_assert(Process, opmv:wasStartedAt,   BN_TimeStamp , Graph),
	rdf_assert(Process, opmv:wasPerformedBy, Program, Graph),
	rdf_assert(Process, opmv:wasPerformedBy, Agent,   Graph),

	(   memberchk(was_derived_from(Sources), Options)
	->  forall(member(Source, Sources),
		   (   rdf_assert(Artifact, opmv:wasDerivedFrom,  Source,  Graph),
		       rdf_assert(Process, opmv:used, Source, Graph),
		       (   \+ rdfs_individual_of(Source, opmv:'Artifact')
		       ->  rdf_assert(Source, rdf:type,	opmv:'Artifact', Graph)
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

opm_program(Graph, Program):-
	git_module_property('amalgame',   home_url(Program)),
	git_module_property('amalgame',   version(AG_version)),
	git_module_property('ClioPatria', version(CP_version)),
	current_prolog_flag(version_git, PL_version),
	format(atom(AG), 'Amalgame: ~w', [AG_version]),
	format(atom(CP), 'ClioPatria: ~w', [CP_version]),
	format(atom(PL), 'SWI-Prolog: ~w', [PL_version]),
	rdf_assert(Program, rdf:type,	opmvc:'Program', Graph),
	rdf_assert(Program, rdfs:label, literal('Amalgame'), Graph),
	rdf_assert(Program, opmvc:software, literal(AG), Graph),
	rdf_assert(Program, opmvc:software, literal(CP), Graph),
	rdf_assert(Program, opmvc:software, literal(PL), Graph).

opm_agent(Graph, Agent) :-
	logged_on(User, anonymous),
        user_property(User, url(Agent)),
	(   user_property(User, realname(UserName))
	->  true
	;   user_property(User, openid(UserName))
	->  true
	;   UserName = Agent
	),
	rdf_assert(Agent, rdfs:label, literal(UserName),  Graph),
	rdf_assert(Agent, rdf:type,   opmv:'Agent',	  Graph).

get_xml_dateTime(TimeStamp) :-
	get_time(T),
	format_time(atom(TimeStamp), '%Y-%m-%dT%H-%M-%S%Oz', T).
