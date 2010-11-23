:- module(conf_ag_data, []).

/** <module> Locate the Econnect vocabularies and metadata
*/

:- multifile
	user:file_search_path/2.
:- dynamic
	user:file_search_path/2.

:- use_module(library(semweb/rdf_zlib_plugin)).

user:file_search_path(rdf, '../vocs').
user:file_search_path(rdf, '../metadata').


