:-module(ag_opm, [
		  opm_was_generated_by/4       % +Process (cause), +Artifact (effect), +RDFGraph, +Options
		 ]).

/* <module> OPM -- simple support for the OPM Provenance Model (OPM)

@see http://openprovenance.org/

*/

:- use_module(library(http/http_host)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(version)).
:- use_module(user(user_db)).

opm_was_generated_by(Process, Artifact, Graph, Options) :-
	get_xml_dateTime(TimeStamp),
	ensure_logged_on(User),
        user_property(User, url(Agent)),
	(   user_property(User, realname(UserName))
	->  true
	;   user_property(User, openid(UserName))
	->  true
	;   UserName = Agent
	),
	rdf_assert(Agent,    rdfs:label, literal(UserName),     Graph),

	rdf_bnode(Program),

	rdf_assert(Agent,    rdf:type,   opmv:'Agent',          Graph),
	rdf_assert(Artifact, rdf:type,   opmv:'Artifact',       Graph),
	rdf_assert(Process,  rdf:type,   opmv:'Process',        Graph),
	rdf_assert(Program,  rdf:type,   opmvc:'Program',       Graph),

	rdf_bnode(BN_TimeStamp),
	rdf_assert(BN_TimeStamp, rdf:type, time:'Instant',  Graph),
	rdf_assert(BN_TimeStamp, time:inXSDDateTime, literal(type(xsd:dateTime, TimeStamp)), Graph),

	rdf_assert(Artifact, opmv:wasGeneratedBy, Process,	  Graph),

	rdf_assert(Process, opmv:wasPerformedBy, Program, Graph),
	rdf_assert(Process, opmv:wasPerformedBy, Agent, Graph),

	rdf_assert(Process, opmv:wasStartedAt,  BN_TimeStamp , Graph),

	(   memberchk(was_derived_from(Sources), Options)
	->  forall(member(Source, Sources),
		   (   rdf_assert(Artifact, opmv:wasDerivedFrom,  Source,  Graph),
		       rdf_assert(Process, opmv:used, Source, Graph)
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

	git_module_property('amalgame',   version(AG_version)),
	git_module_property('ClioPatria', version(CP_version)),
	current_prolog_flag(version_git, PL_version),
	format(atom(AG), 'Amalgame: ~w', [AG_version]),
	format(atom(CP), 'ClioPatria: ~w', [CP_version]),
	format(atom(PL), 'SWI-Prolog: ~w', [PL_version]),
	rdf_assert(Program, rdfs:label, literal('Amalgame'), Graph),
	rdf_assert(Program, opmvc:software, literal(AG), Graph),
	rdf_assert(Program, opmvc:software, literal(CP), Graph),
	rdf_assert(Program, opmvc:software, literal(PL), Graph),
	true.








get_xml_dateTime(TimeStamp) :-
	get_time(T),
	format_time(atom(TimeStamp), '%Y-%m-%dT%H-%M-%S%Oz', T).

