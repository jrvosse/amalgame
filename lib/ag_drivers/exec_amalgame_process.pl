:- module(ag_exec_process, [
			    exec_amalgame_process/7
			   ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(amalgame/expand_graph)).
:- use_module(library(amalgame/map)).

%%	exec_amalgame_process(+Type,+Process,+Strategy,+Module,-Result,-Time,+Options)
%
%
%	Result is generated by executing Process of Type
%	in Strategy. This is to provide amalgame with a uniform interface to
%	all modules. This predicate is multifile so it is easy to add
%	new modules with different input/output parameters.
%
%       @error existence_error(mapping_process)

:- multifile
	exec_amalgame_process/7.

exec_amalgame_process(Type, Process, Strategy, Module, Mapping, Time, Options) :-
	rdfs_subclass_of(Type, amalgame:'Matcher'),
	!,
	findall(S, rdf(Process, amalgame:secondary_input, S), SecInputs),
	maplist(expand_mapping(Strategy), SecInputs, SecInputNF),
	flatten(SecInputNF, SecInput),
	(   rdf(Process, amalgame:source, SourceId, Strategy),
	    rdf(Process, amalgame:target, TargetId, Strategy)
	->  expand_vocab(Strategy, SourceId, Source, _),
	    expand_vocab(Strategy, TargetId, Target, _),
	    timed_call(Module:matcher(Source, Target, Mapping0, [snd_input(SecInput)|Options]), Time)
	;   rdf(Process, amalgame:input, InputId)
	->  expand_mapping(Strategy, InputId, MappingIn),
	    timed_call(Module:filter(MappingIn, Mapping0, [snd_input(SecInput)|Options]), Time)
	),
	merge_provenance(Mapping0, Mapping).

exec_amalgame_process(Class, Process, Strategy, Module, Result, Time, Options) :-
	rdfs_subclass_of(Class, amalgame:'VocExclude'),
	rdf(NewVocab, opmv:wasGeneratedBy, Process, Strategy),
	NewVocOption = new_scheme(NewVocab),
	!,
	once(rdf(Process, amalgame:input, Input, Strategy)),
	expand_vocab(Strategy, Input, Vocab, _),
	findall(S, rdf_has(Process, amalgame:secondary_input, S), Ss),
	maplist(expand_mapping(Strategy), Ss, Expanded),
	append(Expanded, Mapping),
	timed_call(Module:exclude(Vocab, Mapping, Result, [NewVocOption|Options]), Time).
exec_amalgame_process(Class, Process, Strategy, Module, Result, Time, Options) :-
	rdfs_subclass_of(Class, amalgame:'MappingSelecter'),
	!,
	Result = select(Selected, Discarded, Undecided),
	once(rdf(Process, amalgame:input, InputId, Strategy)),
	expand_mapping(Strategy, InputId, MappingIn),
	timed_call(Module:selecter(MappingIn, Selected, Discarded, Undecided, Options), Time).
exec_amalgame_process(Class, Process, Strategy, Module, Result, Time, Options) :-
	rdfs_subclass_of(Class, amalgame:'VocabSelecter'),
	!,
	once(rdf(Process, amalgame:input, Input, Strategy)),
	expand_vocab(Strategy, Input, Vocab, _),
	timed_call(Module:selecter(Vocab, Result, Options), Time).
exec_amalgame_process(Class, Process, Strategy, Module, Result, Time, Options) :-
	rdfs_subclass_of(Class, amalgame:'MapMerger'),
	!,
	findall(Input, rdf(Process, amalgame:input, Input, Strategy), Inputs),
	maplist(expand_mapping(Strategy), Inputs, Expanded),
	timed_call(Module:merger(Expanded, Result, Options), Time).
exec_amalgame_process(Class, Process, Strategy, Module, Result, Time, Options) :-
	rdfs_subclass_of(Class, amalgame:'OverlapComponent'),
	!,
	findall(Input, rdf(Process, amalgame:input, Input, Strategy), Inputs),
	% We need the ids, not the values in most analyzers
	timed_call(Module:analyzer(Inputs, Process, Strategy, Result, Options), Time).

exec_amalgame_process(Class, Process,_,_, _, _, _) :-
	throw(error(existence_error(mapping_process, [Class, Process]), _)).

timed_call(Goal, Time) :-
	thread_self(Me),
        thread_statistics(Me, cputime, T0),
	call(Goal),
	thread_statistics(Me, cputime, T1),
        Time is T1 - T0.
