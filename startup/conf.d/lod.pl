:- module(conf_lod, []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_describe)).
:- use_module(cliopatria(hooks)).
:- use_module(api(lod)).


/** <module> Configure Linked Data (LOD) access for amalgame demo

We serve Symmetric CBDs for a number of resources, most of them redirect
from:
* purl.org/vocabularies/
* purl.org/collections/
* data.beeldengeluid.nl/

@see cliopatria(api/lod)

*/

:- http_handler(root('lod/purl/vocabularies/'), lod_api,
                [ redirected_from('http://purl.org/vocabularies/'),
                  prefix
                ]).

:- http_handler(root('lod/purl/collections/'), lod_api,
                [ redirected_from('http://purl.org/collections/'),
                  prefix
                ]).

:- http_handler(root('ns/gtaa/'), lod_api,
                [ redirected_from('http://data.beeldengeluid.nl/gtaa/'),
                  prefix
                ]).


%%      cliopatria:lod_description(+URI, -Graph) is det.
%
%	Override standard CBD by Symmetric CBD (scbd) expansion.
%	Calls rdf_bounded_description/4 with
%	rdf_bounded_description(rdf, scbd, URI, Graph).

cliopatria:lod_description(URI, Graph) :-
	rdf_bounded_description(rdf, scbd, URI, Graph).

%%	cliopatria:redirect_uri(+Format, +URI, -SeeOther) is det.
%
%	Defines redirections for LOD clients requesting html.
%	Currently supported are:
%	* Redirects to ens:landingPage (Europeana EDM)
%	* Redirects to bibliopolis (FIXME: why is this not a subproperty
%	of ens:landingPage?)
%	* redirects to wordnet.princeton.edu
%	* redirects to the AAT pages at getty.edu

% Redirect HTML requests to resource's landing page if defined
cliopatria:redirect_uri(html, URI, SeeOther) :-
	rdf(URI, 'http://www.europeana.eu/schemas/edm/landingPage', SeeOther),!.
cliopatria:redirect_uri(html, URI, SeeOther) :-
	rdf(URI, 'http://purl.org/collections/bibliopolis/landingPage', SeeOther),!.

cliopatria:redirect_uri(html, URI, SeeOther) :-
	rdf(URI,
	    'http://www.w3.org/2006/03/wn/wn20/schema/senseLabel',
	    literal(lang(_, SL))),
        sub_atom(URI,_,1,0,IndexAtom),
        atom_number(IndexAtom, IndexOffByOne),
        Index is IndexOffByOne - 1,
        Base='http://wordnetweb.princeton.edu/perl/webwn?s=~w&i=~w',
        sformat(SeeOther, Base, [SL,Index]).

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
