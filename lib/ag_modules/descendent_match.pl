:- module(descendent_match,
	  []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(amalgame/vocabulary)).
:- use_module(library(amalgame/map)).
:- use_module(library(count)).

:- public amalgame_module/1.
:- public filter/3.
:- public matcher/4.
:- public selecter/5.
:- public parameter/4.

amalgame_module(amalgame:'DescendentMatcher').
amalgame_module(amalgame:'DescendentFilter').
amalgame_module(amalgame:'DescendentSelecter').

parameter(steps, integer, 1, 'depth of search, defaults to 1, e.g. direct children only').

%%	filter(+MappingsIn, -MappingsOut, +Options)
%
%	Filter mappings based on exact matching of labels.

filter(In, Out, Options) :-
	option(snd_input(SecList), Options),
	findall(S-T-P, member(align(S,T,P), SecList), KeyValueList),
	keysort(KeyValueList, Deduped),
	ord_list_to_assoc(Deduped, BackgroundMatches),
	filter_(In, BackgroundMatches, Out, Options).

filter_([], _, [], _).
filter_([align(S,T,P)|Cs], BackgroundMatches, [C|Mappings], Options) :-
	(   T = scheme(_)
	->  match(align(S,_,P), BackgroundMatches, C, Options),
	    C=align(_,T2,_),
	    vocab_member(T2, T)
	;   match(align(S,T,P), BackgroundMatches, C, Options)
	),
	!,
	filter_(Cs, BackgroundMatches, Mappings, Options).
filter_([_|Cs], BackgroundMatches, Mappings, Options) :-
	filter_(Cs, BackgroundMatches, Mappings, Options).


%%	matcher(+Source, +Target, -Mappings, +Options)
%
%	Mappings is a list of matches between instances of Source and
%	Target.

matcher(Source, Target, Mappings, Options) :-
	option(snd_input(SecList), Options),
	findall(S-T-P, member(align(S,T,P), SecList), KeyValueList),
	keysort(KeyValueList, Deduped),
	ord_list_to_assoc(Deduped, BackgroundMatches),
	findall(M, align(Source, Target, BackgroundMatches, M, Options), Mappings0),
	sort(Mappings0, Mappings).

align(Source, Target, BackgroundMatches, Match, Options) :-
	vocab_member(S, Source),
	vocab_member(T, Target),
	match(align(S,T,[]), BackgroundMatches, Match, Options).


match(align(S, T, Prov0), BackgroundMatches, align(S, T, [Prov|Prov0]), Options) :-
	option(steps(MaxSteps), Options),
	descendent(S, MaxSteps, DescS, R1, Steps1),
	descendent(T, MaxSteps, DescT, R2, Steps2),
	get_assoc(DescS-DescT, BackgroundMatches, _),
	Prov = [method(descendent_match),
		source(DescS),
		target(DescT),
		steps(Steps1-Steps2),
		graph([R1,R2])
	       ].

	/* FIXME: need to make a decision about what to do with align:relation ...
        */

descendent(R, MaxSteps, Child, rdf(R, Prop, Child), Steps) :-
	rdf_equal(skos:narrower, Prop),
	rdf_reachable(R, Prop, Child, MaxSteps, Steps),
	\+ R == Child.
descendent(R, MaxSteps, Child, rdf(Child, Prop, R), Steps) :-
	rdf_equal(skos:broader, Prop),
	rdf_reachable(Child, Prop, R, MaxSteps, Steps),
	\+ R == Child,
	\+ rdf_reachable(R, skos:narrower, Child).

selecter(In, Sel, Dis, Und, Options) :-
	option(snd_input(SecList), Options),
	option(type(SourceOrTarget), Options, source),
	findall(S-T-P, member(align(S,T,P), SecList), KeyValueList),
	keysort(KeyValueList, Deduped),
	ord_list_to_assoc(Deduped, BackgroundMatches),
	(   SourceOrTarget = target
	->  selecter_(SourceOrTarget, In, BackgroundMatches, Sel, Dis, Und, Options)
	;   predsort(ag_map:compare_align(target), In, InT),
	    selecter_(SourceOrTarget, InT, BackgroundMatches, Sel0, Dis0, Und0, Options),
	    predsort(ag_map:compare_align(source), Sel0,  Sel),
	    predsort(ag_map:compare_align(source), Dis0,  Dis),
	    predsort(ag_map:compare_align(source), Und0,  Und)
	).

selecter_(_, [], _, [], [], [], _).
selecter_(Type, [Head|Tail], BackgroundMatches, Sel, Dis, Und, Options) :-
	Head = align(S,T,_),
	(   Type == target
	->  same_source(Tail, S, Same, Rest)
	;   same_target(Tail, T, Same, Rest)
	),
	selecter_(Type, Rest, BackgroundMatches, TailSel, TailDis, TailUnd, Options),
	Candidates = [Head|Same],
	maplist(descendent_count(BackgroundMatches, Options), Candidates, Counts),
	keysort(Counts, [BestCount-BestCorr|DisgardedSame0]),
	(   Same \= [], BestCount < 1
	->  append([[BestCorr],    TailSel], Sel),
	    pairs_values(DisgardedSame0, DisgardedSame),
	    append([DisgardedSame, TailDis], Dis),
	    Und = TailUnd
	;   Sel = TailSel,
	    Dis = TailDis,
	    append([Candidates, TailUnd], Und)
	).

descendent_count(BackgroundMatches, Options, Corr, Count-Corr) :-
	answer_count(Pnew,
		     match(Corr, BackgroundMatches, align(_S,_T,Pnew), Options),
		     10,
		     Count0),
	Count is 1/(1+Count0).
