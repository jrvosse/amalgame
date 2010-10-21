:- module(amalgame_ns, []).
:- use_module(library(semweb/rdf_db)).

% Register namespaces we use in amalgame's Prolog source code.
% Namespaces only used in the data should be registered through their Manifest files.
% Also note that many common namespaces have been defined already by
% Prolog or ClioPatria.
%
:- rdf_register_ns(align,      'http://knowledgeweb.semanticweb.org/heterogeneity/alignment#').
:- rdf_register_ns(opmv,       'http://purl.org/net/opmv/ns#').

:- rdf_register_ns(amalgame,   'http://purl.org/vocabularies/amalgame#').
:- rdf_register_ns(evaluator,  'http://purl.org/vocabularies/amalgame/evaluator#').
:- rdf_register_ns(vulod,      'http://semanticweb.cs.vu.nl/lod/').
