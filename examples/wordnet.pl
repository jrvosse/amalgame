% Example alignment, aligning wn30 to wn20 using amalgame

:- module(wnalign,
	 [run/0
	 ]).

:- use_module(amalgame(align/skosalign)).

wn20schema('http://www.w3.org/2006/03/wn/wn20/schema/').
wn20('http://www.w3.org/2006/03/wn/wn20/').
wn30('http://purl.org/vocabularies/princeton/wn30/').
glossmatches(F) :- wn30(WN30), atom_concat(WN30,'glossmatches-m.ttl', F).

run:-
	wn30(WN30),
	wn20(WN20),
	prepare_data(WN30),
	prepare_data(WN20),
	align_schemes(WN30, WN20, []).

:- dynamic
	prepare_cache/1.

clean_start:-
	retractall(prepare_cache(_)).

prepare_data(Scheme) :-
	prepare_cache(done(Scheme)),
	!.


prepare_data(Scheme) :-
	wn20(Scheme),
	remove_rdfs_label(Scheme),
	fix_gloss,
	assert(prepare_cache(done(Scheme))).

prepare_data(Scheme) :-
	wn30(Scheme),
	remove_rdfs_label(Scheme),
	assert(prepare_cache(done(Scheme))).

remove_rdfs_label(Scheme) :-
	rdf_transaction(
			forall(rdf(S, skos:inScheme, Scheme),
			       rdf_retractall(S, rdfs:label, _)
			      )
		       ).
%%	fix_gloss is det.
%
%	Removes the spurious round brackets around the wn20
%	gloss literals.

fix_gloss:-
	rdf_transaction(
			forall((   rdf(S, wn20schema:gloss, literal(lang(Lang, Gloss))),
				   sub_atom(Gloss,0,1,_,'(') % Just check if we have not done so already ...
			       )
			       ,
			       (   sub_atom(Gloss,1,_,1,NewGloss),
				   rdf_retractall(S, wn20schema:gloss, literal(lang(Lang, Gloss))),
				   rdf_assert(S, wn20schema:gloss, literal(lang(Lang, NewGloss)))
			       )
			      )
		       ).



