:- module(conf_lod, []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_describe)).
:- use_module(cliopatria(hooks)).
:- use_module(api(lod)).


/** <module> Configure Linked Data (LOD) access

Load the linked-data server and the   library to register HTTP handlers.
and then register your LOD areas and/or  handlers for locations that are
redirected from e.g., http://www.purl.org. Multiple   handlers can point
to lod_api/1, but one handler should not be  a prefix of another one (as
in /rdf/ and /rdf/time/). The first   example  assumes that requests for
RDF URIs arrive at this server directly   or through a proxy. The latter
assumes that /mydata/ on purl.org is   redirected  to /purl/rdf/ on this
server and all RDF URIs start with http://www.purl.org/mydata/

@see cliopatria(api/lod)
*/

%%      cliopatria:lod_description(+URI, -Graph) is det.
%
%	Override standard CBD by Symmetric CMD (scbd) expansion.

cliopatria:lod_description(URI, Graph) :-
	rdf_bounded_description(rdf, scbd, URI, Graph).

:- http_handler('/lod/purl/vocabularies/', lod_api,
                [ redirected_from('http://purl.org/vocabularies/'),
                  prefix
                ]).

:- http_handler('/lod/purl/collections/', lod_api,
                [ redirected_from('http://purl.org/collections/'),
                  prefix
                ]).

:- http_handler('/gtaa/', lod_api,
                [ redirected_from('http://data.beeldengeluid.nl/gtaa/'),
                  prefix
                ]).



