:- module(amalgame_ns, []).
:- use_module(library(semweb/rdf_db)).

% Register namespaces we use in the Prolog source code.
% Namespaces only used in the data should be registered through their Manifest files.
%
:- rdf_register_ns(align,      'http://knowledgeweb.semanticweb.org/heterogeneity/alignment#').
:- rdf_register_ns(amalgame,   'http://purl.org/vocabularies/amalgame#').
