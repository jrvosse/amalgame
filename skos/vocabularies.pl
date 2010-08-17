:- module(am_skosvocs,
          [
          ]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(semweb/rdfs)).

:- use_module(components(label)).

:- http_handler(amalgame(list_skos_vocs),     http_list_skos_vocs,     []).

http_list_skos_vocs(_Request) :-
	findall(Scheme,
		rdfs_individual_of(Scheme, skos:'ConceptScheme'),
		Schemes),
	reply_html_page(cliopatria(default),
			title('SKOS vocabularies'),
			[ h4('SKOS concept schemes in the RDF store'),
			  ol(\show_schemes(Schemes))
			]).

show_schemes([]) --> !.
show_schemes([H|Tail]) -->
	{
	 http_link_to_id(list_resource, [r(H)], VLink)
	},
	html(li(a([href(VLink)],\turtle_label(H)))),
	show_schemes(Tail).


