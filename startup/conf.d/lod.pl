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

:- http_handler('/lod/purl/vocabularies/', lod_api,
                [ redirected_from('http://purl.org/vocabularies/'),
                  prefix
                ]).

:- http_handler('/lod/purl/collections/', lod_api,
                [ redirected_from('http://purl.org/collections/'),
                  prefix
                ]).

:- http_handler('/ns/gtaa/', lod_api,
                [ redirected_from('http://data.beeldengeluid.nl/gtaa/'),
                  prefix
                ]).


%%      cliopatria:lod_description(+URI, -Graph) is det.
%
%	Override standard CBD by Symmetric CMD (scbd) expansion.

cliopatria:lod_description(URI, Graph) :-
	rdf_bounded_description(rdf, scbd, URI, Graph).

cliopatria:redirect_uri(html, URI, SeeOther) :-
	rdf(URI,
	    'http://www.w3.org/2006/03/wn/wn20/schema/senseLabel',
	    literal(lang(_, SL))),
        sub_atom(URI,_,1,0,IndexAtom),
        atom_number(IndexAtom, IndexOffByOne),
        Index is IndexOffByOne - 1,
        Base='http://wordnetweb.princeton.edu/perl/webwn?s=~w&i=~w',
        sformat(SeeOther, Base, [SL,Index]).

% Redirect HTML requests to resource's landing page if defined
cliopatria:redirect_uri(html, URI, SeeOther) :-
	rdf(URI, 'http://www.europeana.eu/schemas/edm/landingPage', SeeOther),!.
cliopatria:redirect_uri(html, URI, SeeOther) :-
	rdf(URI, 'http://purl.org/collections/bibliopolis/landingPage', SeeOther),!.

% Redirect HTML requests for wordnet to Princeton
cliopatria:redirect_uri(html, URI, SeeOther) :-
	rdf(URI,
	    'http://www.w3.org/2006/03/wn/wn20/schema/senseLabel',
	    literal(lang(_, SL))),
        sub_atom(URI,_,1,0,IndexAtom),
        atom_number(IndexAtom, IndexOffByOne),
        Index is IndexOffByOne - 1,
        Base='http://wordnetweb.princeton.edu/perl/webwn?s=~w&i=~w',
        sformat(SeeOther, Base, [SL,Index]),
	!.

% Redirect HTML requests for AAT to Getty
cliopatria:redirect_uri(html, URI, SeeOther) :-
        rdf(URI, skos:inScheme, 'http://purl.org/vocabularies/getty/aat'),
        rdf(URI, vp:id, literal(Id)),
        Base='http://www.getty.edu/vow/AATFullDisplay?find=&logic=AND&note=&english=N&subjectid=',
        atom_concat(Base, Id, SeeOther).
