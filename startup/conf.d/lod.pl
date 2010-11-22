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

:- http_handler(root('lod/purl/vocabularies/'),
		lod_api([ redirected_from('http://purl.org/vocabularies/'),
			  bounded_description(scbd)
			]),
		[ prefix
                ]).

:- http_handler(root('lod/purl/collections/'),
		lod_api([ redirected_from('http://purl.org/collections/'),
			  bounded_description(scbd)
			]),
		[ prefix
                ]).

:- http_handler(root('ns/gtaa/'),
		lod_api([ redirected_from('http://data.beeldengeluid.nl/gtaa/'),
			  bounded_description(scbd)
			]),
		[ prefix
                ]).


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

% Redirect HTML requests for wordnet to Princeton
cliopatria:redirect_uri(html, URI, SeeOther) :-
	rdf(URI,
	    'http://www.w3.org/2006/03/wn/wn20/schema/senseLabel',
	    literal(lang(_, SL))),
	atomic_list_concat(TokenList, -, SL),
	last(TokenList, IndexAtom),
	term_to_atom(IndexTerm, IndexAtom),
        (   number(IndexTerm)
	->  Index is IndexTerm - 1,
	    Base='http://wordnetweb.princeton.edu/perl/webwn?s=~w&i=~w'
	;   fail
	),
        sformat(SeeOther, Base, [SL,Index]),
	!.

% Redirect HTML requests for AAT to Getty
cliopatria:redirect_uri(html, URI, SeeOther) :-
        rdf(URI, skos:inScheme, 'http://purl.org/vocabularies/getty/aat'),
        rdf(URI, 'http://purl.org/vocabularies/getty/vp/id', literal(Id)),
        Base='http://www.getty.edu/vow/AATFullDisplay?find=&logic=AND&note=&english=N&subjectid=',
        atom_concat(Base, Id, SeeOther).
