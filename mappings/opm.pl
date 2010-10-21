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

	rdf_assert(Artifact, rdf:type,   opmv:'Artifact',       Graph),
	rdf_assert(Agent,    rdf:type,   opmv:'Agent',          Graph),
	rdf_assert(Agent,   opmv:label,	literal(UserName),     Graph),

	rdf_bnode(BN_TimeStamp),
	rdf_assert(BN_TimeStamp, rdf:type,          opmv:'OTime',	                    Graph),
	rdf_assert(BN_TimeStamp, opmv:noEarlierThan, literal(type(xsd:dateTime, TimeStamp)), Graph),

	rdf_bnode(BN_GenerationEvent),
	rdf_assert(BN_GenerationEvent, rdf:type,   opmv:'WasGeneratedBy',  Graph),
	rdf_assert(BN_GenerationEvent, opmv:time,   BN_TimeStamp,	  Graph),
	rdf_assert(BN_GenerationEvent, opmv:effect, Artifact,		  Graph),
	rdf_assert(BN_GenerationEvent, opmv:cause,  Process,		  Graph),

	rdf_bnode(BN_ControlEvent),
	rdf_assert(BN_ControlEvent, rdf:type,      opmv:'WasControlledBy', Graph),
	rdf_assert(BN_ControlEvent, opmv:effect,	   Process,               Graph),
	rdf_assert(BN_ControlEvent, opmv:startTime, BN_TimeStamp,          Graph),
	rdf_assert(BN_ControlEvent, opmv:cause,     Agent,                 Graph),

	(   memberchk(was_derived_from(Source), Options)
	->  rdf_bnode(BN_DerivedEvent),
	    rdf_assert(BN_DerivedEvent, rdf:type,   opmv:'WasDerivedFrom',  Graph),
	    rdf_assert(BN_DerivedEvent, opmv:cause,  Source              ,  Graph),
	    rdf_assert(BN_DerivedEvent, opmv:effect, Artifact            ,  Graph)
	;   true
	),

	true.








get_xml_dateTime(TimeStamp) :-
	get_time(T),
	format_time(atom(TimeStamp), '%Y-%m-%dT%H-%M-%S%Oz', T).

