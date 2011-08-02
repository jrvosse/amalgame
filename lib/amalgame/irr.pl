:- module(ag_irr, [
		   alpha/3
		  ]).

:- use_module(library('R')).
:- use_module(expand_graph).

/* Inter-rater reliability metrics using R */
:-dynamic encoding_table/2.

alpha(Strategy, Inputs, Out) :-
	length(Inputs, Ninputs), Ninputs > 1,
	retractall(encoding_table(_,_)),
	asserta(encoding_table(next_free_code, 0)),
	Inputs = [Head|Tail],
	expand_mapping(Strategy, Head, Reference),
	columnize(Reference, Reference, RefColumn, true),
	debug(irr, 'ag_irr:alpha ref column: ~w', [RefColumn]),
	findall(Finding_as_an_R_column-Same,
		(member(Id, Tail),
		 expand_mapping(Strategy, Id, Mapping),
		 columnize(Mapping, Reference, Finding_as_an_R_column, Same),
		 debug(irr, 'ag_irr:alpha column to check: ~w', [Finding_as_an_R_column])
		),
		Pairs),
	pairs_keys_values(Pairs, Columns, Sames),
	(   member(false, Sames)
	->  create_matrix([RefColumn|Columns], Matrix),
	    format(atom(Command1), 'a <- kripp.alpha(~w)', [Matrix]),
	    format(atom(Command2), 'a$cm', [Matrix]),
	    R = 'amalgame_R_session',
	    r_open([alias(R), with(non_interactive)]),
	    r_lib(R, irr),
	    r_in(R, Command1),
	    r_out(R, Command2, Out),
	    r_close(R)
	;   Out = [alpha(1.0), comment('Warning: All tested values are identical!')]
	).


columnize(Mapping, Reference, Finding_as_an_R_column,Same) :-
	(Mapping = Reference -> Same = true; Same = false),
	encode_findings(Mapping, Reference, Finding),
	atomic_list_concat(Finding, ', ', FindingS),
	format(atom(Finding_as_an_R_column), 'c(~w)', [FindingS]).

create_matrix(Columns, Matrix) :-
	atomic_list_concat(Columns, ', ', ColumnsS),
	format(atom(Matrix), 'rbind(~w)', [ColumnsS]).

encode_findings([], [], []).
encode_findings([H|T], [RefHead|RefTail], [HeadResult|TailResults]) :-
	encode_finding(H,  RefHead, HeadResult),
	encode_findings(T, RefTail, TailResults).

encode_finding(align(S,T,[P|_]), align(S,T,_), Encoding) :-
	member(relation(R), P),
	(   encoding_table(R,Encoding)
	->  true
	;   encoding_table(next_free_code, Encoding),
	    NextFreeCode is Encoding + 1,
	    retractall(encoding_table(next_free_code, _)),
	    asserta(encoding_table(next_free_code, NextFreeCode)),
	    assertz(encoding_table(R,Encoding))
	).
