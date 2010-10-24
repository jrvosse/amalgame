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
	user_property(User, realname(UserName)),

	rdf_assert(Artifact, rdf:type,   opmv:'Artifact',       Graph),
	rdf_assert(Process,  rdf:type,   opmv:'Process',        Graph),
	rdf_assert(Agent,    rdf:type,   opmv:'Agent',          Graph),
	rdf_assert(Agent,    rdfs:label, literal(UserName),     Graph),

	rdf_bnode(BN_TimeStamp),
	rdf_assert(BN_TimeStamp, rdf:type, time:'Instant',  Graph),
	rdf_assert(BN_TimeStamp, time:inXSDDateTime, literal(type(xsd:dateTime, TimeStamp)), Graph),

	% rdf_assert(Artifact, opmv:wasGeneratedAt, BN_TimeStamp,	  Graph),
	rdf_assert(Artifact, opmv:wasGeneratedBy, Process,	  Graph),

	rdf_assert(Process, opmv:wasPerformedBy, Agent, Graph),
	rdf_assert(Process, opmv:wasStartedAt,  BN_TimeStamp , Graph),

	(   memberchk(was_derived_from(Sources), Options)
	->  forall(member(Source, Sources),
		   rdf_assert(Artifact, opmv:wasDerivedFrom,  Source,  Graph))
	;   true
	),
	(   memberchk(request(Request), Options)
	->  http_current_host(Request, Hostname, Port, [global(true)]),
	    memberchk(request_uri(ReqURI), Request),
	    memberchk(protocol(Protocol), Request),
	    format(atom(ReqUsed), '~w://~w:~w~w', [Protocol,Hostname,Port,ReqURI]),
	    rdf_assert(Process, amalgame:request, literal(ReqUsed), Graph)
	;   true
	),

	git_module_property('ClioPatria', version(CP_version)),
	git_module_property('amalgame',   version(AG_version)),
	format(atom(Version), 'Made using Amalgame ~w/Cliopatria ~w', [AG_version, CP_version]),
	rdf_assert(Process, opmvc:software, literal(Version), Graph),
	true.








get_xml_dateTime(TimeStamp) :-
	get_time(T),
	format_time(atom(TimeStamp), '%Y-%m-%dT%H-%M-%S%Oz', T).

