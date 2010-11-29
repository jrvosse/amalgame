:- module(amalgame_ns, []).
:- use_module(library(semweb/rdf_db)).

% Register namespaces we use in amalgame's Prolog source code here.
% Namespaces only used in the data should be registered through their Manifest files.
% Also note that many common namespaces have been defined already by
% Prolog or ClioPatria.

% Namespaces from third parties we use/reuse:
:- rdf_register_ns(align, 'http://knowledgeweb.semanticweb.org/heterogeneity/alignment#').
:- rdf_register_ns(time, 'http://www.w3.org/2006/time#').
:- rdf_register_ns(void, 'http://rdfs.org/ns/void#').

% Namespaces we declared ourselves:
:- rdf_register_ns(amalgame,   'http://purl.org/vocabularies/amalgame#').
:- rdf_register_ns(evaluator,  'http://purl.org/vocabularies/amalgame/evaluator#').

% Namespaces that are not used in the code but have no obvious location
% in the data tree... This should be moved to somewhere else in the
% end.  FIXME

:- rdf_register_ns(vulod,      'http://semanticweb.cs.vu.nl/lod/').
