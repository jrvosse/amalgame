:-module(ag_opm, [
		  opm_was_generated_by/4       % +Process (cause), +Artifact (effect), +RDFGraph, +Options
		 ]).

/* <module> OPM -- simple support for the OPM Provenance Model (OPM)

@see http://openprovenance.org/

*/

:- use_module(library(semweb/rdf_db)).
:- use_module(user(user_db)).

opm_was_generated_by(Process, Artifact, Graph, Options) :-
	get_xml_dateTime(TimeStamp),
	ensure_logged_on(User),
        user_property(User, url(Agent)),
	user_property(User, realname(UserName)),

	rdf_assert(Artifact, rdf:type,   opm:'Artifact',       Graph),
	rdf_assert(Agent,    rdf:type,   opm:'Agent',          Graph),
	rdf_assert(Agent,   opm:label,	literal(UserName),     Graph),

	rdf_bnode(BN_TimeStamp),
	rdf_assert(BN_TimeStamp, rdf:type,          opm:'OTime',	                    Graph),
	rdf_assert(BN_TimeStamp, opm:noEarlierThan, literal(type(xsd:dateTime, TimeStamp)), Graph),

	rdf_bnode(BN_GenerationEvent),
	rdf_assert(BN_GenerationEvent, rdf:type,   opm:'WasGeneratedBy',  Graph),
	rdf_assert(BN_GenerationEvent, opm:time,   BN_TimeStamp,	  Graph),
	rdf_assert(BN_GenerationEvent, opm:effect, Artifact,		  Graph),
	rdf_assert(BN_GenerationEvent, opm:cause,  Process,		  Graph),

	rdf_bnode(BN_ControlEvent),
	rdf_assert(BN_ControlEvent, rdf:type,      opm:'WasControlledBy', Graph),
	rdf_assert(BN_ControlEvent, opm:effect,	   Process,               Graph),
	rdf_assert(BN_ControlEvent, opm:startTime, BN_TimeStamp,          Graph),
	rdf_assert(BN_ControlEvent, opm:cause,     Agent,                 Graph),

	(   memberchk(was_derived_from(Source), Options)
	->  rdf_bnode(BN_DerivedEvent),
	    rdf_assert(BN_DerivedEvent, rdf:type,   opm:'WasDerivedFrom',  Graph),
	    rdf_assert(BN_DerivedEvent, opm:cause,  Source              ,  Graph),
	    rdf_assert(BN_DerivedEvent, opm:effect, Artifact            ,  Graph)
	;   true
	),

	true.








get_xml_dateTime(TimeStamp) :-
	get_time(T),
	format_time(atom(TimeStamp), '%Y-%m-%dT%H-%M-%S%z', T).

