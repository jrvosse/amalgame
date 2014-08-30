:- module(ancestor_selecter,
	  []).

:- use_module(library(apply)).
:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(pairs)).
:- use_module(library(sort)).

:- use_module(library(amalgame/map)).
:- use_module(ancestor).

:- public amalgame_module/1.
:- public selecter/5.
:- public parameter/4.

amalgame_module(amalgame:'AncestorSelecter').

parameter(steps, integer, 1,
	  'depth of search, defaults to 1, e.g. direct parents only').
parameter(type,
          oneof([source, target]), source,
          'Discard other matches with the same source/target').

selecter(In, Sel, Dis, Und, Options) :-
	option(snd_input(SecList), Options),
	option(type(SourceOrTarget), Options, source),
	findall(S-T-P, member(align(S,T,P), SecList), KeyValueList),
	keysort(KeyValueList, Deduped),
	ord_list_to_assoc(Deduped, BackgroundMatches),
	(   SourceOrTarget = source
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
	(   Type == source
	->  same_source(Tail, S, Same, Rest)
	;   same_target(Tail, T, Same, Rest)
	),
	selecter_(Type, Rest, BackgroundMatches, TailSel, TailDis, TailUnd, Options),
	Candidates = [Head|Same],
	maplist(ancestor_count(BackgroundMatches, Options), Candidates, Counts0),
	keysort(Counts0, Counts),
	partition(zero_key, Counts, Zero, NonZero),
	(   NonZero \= []
	->  pairs_values(Zero,   DisgardedSame),
	    pairs_values(NonZero, SelectedSame),
	    append([SelectedSame,  TailSel], Sel),
	    append([DisgardedSame, TailDis], Dis),
	    Und = TailUnd
	;   Sel = TailSel,
	    Dis = TailDis,
	    append([Candidates, TailUnd], Und)
	).

zero_key(0-_Value).

ancestor_count(BackgroundMatches, Options, Corr, Count-Merged) :-
	findall(Match,
		ancestor_match(Corr, BackgroundMatches, Match, Options),
		Matches),
	length(Matches, Count),
	(   Count > 0
	->  merge_provenance(Matches, [Merged])
	;   Merged = Corr
	).
