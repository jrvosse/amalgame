:- module(skosmatcher,
	  [skos_find_candidates/3 % +SourceConcept, +TargetScheme, +Options, -Results
	  ]
	 ).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_portray)).
:- use_module(amalgame(mappings/edoal)).
:- use_module(levenshtein).

%%	skos_find_candidates(+C, +S, +Options, -Result) is semidet.
%
%	Find all correspondences candidates for for SKOS concept C
%	from SKOS scheme S. Result is an RDF graph in (extended) EDOAL format
%
%	Options include:
%	* candidate_matchers(M): list of matcher to use, defaults to
%	[labelmatch]
%	* lang(Lang): only match labels with given language tag
%	* case_sensitive(boolean): defaults to false
%	* sourcelabel(URI): defaults to rdfs:label
%       * targetlabel(URI): defaults to rdfs:label
%

skos_find_candidates(Source, TargetScheme, Options):-
	ground(Source),
	ground(TargetScheme),
	ground(Options),

	findall(Target,
		find_candidate(Source, TargetScheme, Target, Options),
		Targets),
	sort(Targets, TargetsUnique),
	forall(member(Target, TargetsUnique),
	       (   find_label_match_methods(Source, Target, Methods, Options),
		   (   Methods \= []
		   ->  assert_cell(Source, Target, [method(Methods)|Options])
		   ;   true
		   )
	       )
	      ).

%%	find_candidate(+Source, +TargetScheme, -Target, +Options) is
%%	nondet.
%
%	Find a candidate mapping Target from TargetScheme for Source
%	given Options (see skos_find_candidates/3).
%
%	Warning: this is very efficient for indexed labelmatches, but
%	very inefficient for non-indexed methods.
%

find_candidate(Source, TargetScheme, Target, Options) :-
	option(candidate_matchers(Matchers), Options, [labelmatch]),
	memberchk(labelmatch, Matchers),
	option(language(Lang1),Options, _),
	option(case_sensitive(CaseSensitive), Options, false),
	rdf_equal(rdfs:label, DefaultProp),
	option(sourcelabel(MatchProp1), Options, DefaultProp),
	option(targetlabel(MatchProp2), Options, DefaultProp),

	rdf_has(Source, MatchProp1, literal(lang(Lang1, Label1))),
	(   CaseSensitive == true
	->  rdf_has(Target, MatchProp2, literal(Label1))
	;   rdf_has(Target, MatchProp2, literal(exact(Label1),lang(_TargetLang, _Label2)))
	),
	rdf_has(Target, skos:inScheme, TargetScheme).

find_candidate(Source, TargetScheme, Target, Options) :-
	option(candidate_matchers(Matchers), Options),
	memberchk(stringdist, Matchers),
	option(language(Lang1),Options, _),
	rdf_equal(rdfs:label, DefaultProp),
	option(sourcelabel(MatchProp1), Options, DefaultProp),
	option(targetlabel(MatchProp2), Options, DefaultProp),
	option(prefixdepth(PrefLen), Options, 2),

	rdf_has(Source, MatchProp1, literal(lang(Lang1, Label1))),
	sub_atom(Label1,0,PrefLen,_,Prefix),
	rdf_has(Target, MatchProp2, literal(prefix(Prefix), lang(_TargetLang,_Label2))),
	rdf_has(Target, skos:inScheme, TargetScheme).

%%	find_label_match_methods(+Source, +Target, -Methods, +Options)
%%	is det.
%
%	True if Methods is the list of all label match methods to match
%	Source and Target.

find_label_match_methods(Source, Target, Methods, Options) :-
	findall(Method,
		find_label_match_method(Source, Target, Method, Options),
		Methods
	       ).

%%	find_label_match_method(?Source, ?Target, Method:atom) is
%%	nondet.
%
%	True if Source and Target share at least one pair that matches.
%	Details about this match are encoded in Method

find_label_match_method(Source, Target, Method, _Options):-
	rdf_equal(rdfs:label, DefaultProp),
	option(sourcelabel(MatchProp1), Options, DefaultProp),
	option(targetlabel(MatchProp2), Options, DefaultProp),
	rdf_has(Source, MatchProp1, literal(lang(SourceLang, Label1)), RealLabel1Predicate),
	rdf_has(Target, MatchProp2, literal(exact(Label1),lang(TargetLang, Label2)), RealLabel2Predicate),
	format(atom(Method), 'exact (~p:~w@~w,~p:~w@~w)', [RealLabel1Predicate, Label1, SourceLang,RealLabel2Predicate, Label2, TargetLang]).

find_label_match_method(Source, Target, Method, Options):-
	option(candidate_matchers(Matchers), Options),
	memberchk(stringdist, Matchers),
	rdf_equal(rdfs:label, DefaultProp),
	option(sourcelabel(MatchProp1), Options, DefaultProp),
	option(targetlabel(MatchProp2), Options, DefaultProp),
	rdf_has(Source, MatchProp1, literal(lang(SourceLang, Label1)), RealLabel1Predicate),
	rdf_has(Target, MatchProp2, literal(lang(TargetLang, Label2)), RealLabel2Predicate),
	max_stringdist(Label1, Label2, 1),
	format(atom(Method), 'dist<2 (~p:~w@~w,~p:~w@~w)', [RealLabel1Predicate, Label1, SourceLang,RealLabel2Predicate, Label2, TargetLang]).


% This version of candidate/3 asserts a match when the
% Levenshtein distance between two labels is less than a maximum
% (retrieved from stringdist_setting/2). For reasons of scalability,
% only labels with a common prefix of a certain length (also a parameter
% retrieved from stringdist_setting/2) are considered.
% Here, a confidence of 0.0005 is used.

xcandidate(SourceConcept, TargetConceptScheme, Options) :-
	ground(SourceConcept),
	ground(TargetConceptScheme),
	ground(Options),
	option(candidate_matchers(Matchers), Options, []),
	memberchk(stringdist, Matchers),
       	write('.'),flush,
	rdf_has(TargetConcept, skos:inScheme, TargetConceptScheme),
	rdf_has(SourceConcept, rdfs:label, literal(lang(_, Label1)), RealLabel1Predicate),
	stringdist_setting(prefixdepth,PrefLen),
	sub_atom(Label1,0,PrefLen,_,Prefix),
	rdf_has(TargetConcept, rdfs:label, literal(prefix(Prefix), Label2), RealLabel2Predicate),
	%rdf_has(TargetConcept, rdfs:label, literal(lang(_, Label2)), RealLabel2Predicate),
	max_stringdist(Label1, Label2, 1),
	format(atom(Method), 'stringdist match: ~p-~p', [RealLabel1Predicate, RealLabel2Predicate]),
	CellOptions = [measure(0.0001), % Only label match, this is just a candidate
		       method(Method)
		       |Options
		      ],
	assert_cell(SourceConcept, TargetConcept, CellOptions).


stringdist_setting(prefixdepth,2).
stringdist_setting(maxlevdist,2).

% succeeds if Label1 and Label2 have a levenshtein distance of
% Maxdist or less after a normalization step where whitespaces and
% punctuation is removed.

max_stringdist(Label1, Label2, MaxDist):-
	labnorm(Label1, L1N),
      	labnorm(Label2, L2N),
	levenshtein(L1N,L2N,Dist),!,
	Dist =< MaxDist,!.

% Label1=Label2 case is already taken care of by running exact match
% always before the levenshtein distance ...
% max_stringdist(Label1, Label2, _MaxDist):-
% Label1 = Label2.

labnorm(L,LN):-
	downcase_atom(L, LD),
	atom_chars(LD, LC),
	delete(LC, ' ',  LC1),
	delete(LC1, ',',  LC2),
	delete(LC2, '.',  LC3),
	delete(LC3, '-',  Lchars),
	atom_chars(LN,Lchars),!.
