:- module(ag_irr, [
		   alpha/4
		  ]).

:- use_module(library('R')).
:- use_module(expand_graph).

/* Inter-rater reliability metrics using R */
:-dynamic encoding_table/2.

%%	alpha(+Strategy, +Inputs, -Out, +Options) is semidet.
%
%	Calculate Krippendorff's alpha reliability statistics for Input,
%	which is assumed to be a list of Mappings defined in Strategy.
%	Out represent the results as an option list, defining:
%       * name(alpha), a constant
%	* value(Alpha), the value of alpha, -1 =< Alpha =< 1.0
%       * subjects(Subjects), # subjects rated
%       * raters(Raters), # raters rating the subjects
%       * dim_cm(D),
%	* concordance_matrix(Cm), Cm is a DxD concordance matrix
%       * value_matrix(Vm), Vm is a #subjects x $raters matrix
%         with the encoded values used in the calculation
%	* encoding(Encoding), expanded encoding table used
%
%	 By default, the predicate builds its own encoding table by
%	 assigning a unique interger value to each relation found in the
%	 input correspondences. If this is not the desired behaviour,
%	 one can provide an encoding table as an option. The table has
%	 the form [Relation1-Encoding1, ..., RelationN,EncodingN], where
%	 each Encoding is an integer and each Relation is (possibly
%	 unexpanded) relation URL (e.g. skos:closeMatch).
%
%	 So the only option currently supported is:
%	 * encoding(Encoding), optional encoding table
%
alpha(Strategy, Inputs, Out, Options) :-
	length(Inputs, Ninputs), Ninputs > 1,
	init_encoding_table(Options),
	Inputs = [Head|Tail],
	expand_mapping(Strategy, Head, Reference),
	columnize(Reference, Reference, RefPrologColumn, RefRColumn, true),
	debug(irr, 'ag_irr:alpha ref column: ~w', [RefRColumn]),
	findall((Finding-Finding_as_an_R_column)-Same,
		(member(Id, Tail),
		 expand_mapping(Strategy, Id, Mapping),
		 columnize(Mapping, Reference, Finding, Finding_as_an_R_column, Same),
		 debug(irr, 'ag_irr:alpha column to check: ~p\n~w', [Id, Finding_as_an_R_column])
		),
		NestedPairs),
	pairs_keys_values(NestedPairs, Pairs, Sames),
	pairs_keys_values(Pairs, PrologColumns, RColumns),
	get_encoding_table(Encoding),
	(   member(false, Sames) % at least one column is different
	->  create_matrix([RefRColumn|RColumns], Matrix),
	    R = 'amalgame_R_session_to_compute_kripp_alpha',
	    format(atom(Command1), 'a <- kripp.alpha(~w)', [Matrix]),
	    r_open([alias(R), with(non_interactive)]),
	    r_lib(R, irr),
	    r_in(R, Command1),
	    r_out(R, 'cat(a$value,"\n")', [AlphaCodes]),
	    r_out(R, 'cat(a$subjects, "\n")', [SubjCodes]),
	    r_out(R, 'cat(a$raters, "\n")', [RaterCodes]),
	    r_out(R, 'cat(a$cm, "\n")', [CmCodes]),
	    r_out(R, 'cat(dim(a$cm), "\n")', [DimCodes]),
	    r_close(R),
	    atom_codes(AlphaAtom, AlphaCodes), term_to_atom(Alpha, AlphaAtom),
	    atom_codes(SubjAtom, SubjCodes), term_to_atom(Subjects, SubjAtom),
	    atom_codes(RtAtom, RaterCodes), term_to_atom(Raters, RtAtom),
	    atom_codes(DimAtom, DimCodes), atomic_list_concat(DimCML, ' ', DimAtom),
	    atom_codes(CmAtom, CmCodes), atomic_list_concat(L,' ', CmAtom),
	    [DimCmA|_] = DimCML, term_to_atom(DimCm, DimCmA),
	    vector_to_matrix(L, DimCm, Cm),
	    Out = [name(alpha),
		   value(Alpha),
		   subjects(Subjects),
		   raters(Raters),
		   concordance_matrix(Cm),
		   dim_cm(DimCm),
		   value_matrix([RefPrologColumn|PrologColumns]),
		   encoding(Encoding)
		  ]
	;   Out = [name(alpha),
		   value(1.0),
		   comment('Warning: All tested values are identical!')]
	).


columnize(Mapping, Reference, Finding, Finding_as_an_R_column,Same) :-
	(Mapping = Reference -> Same = true; Same = false),
	length(Mapping, K), length(Reference, Kref),
	debug(irr, 'ag_irr: columnize column of k=~w (~w)', [K, Kref]),
	encode_findings(Mapping, Reference, Finding),
	atomic_list_concat(Finding, ', ', FindingS),
	format(atom(Finding_as_an_R_column), 'c(~w)', [FindingS]),!.

create_matrix(Columns, Matrix) :-
	atomic_list_concat(Columns, ', ', ColumnsS),
	format(atom(Matrix), 'rbind(~w)', [ColumnsS]).

init_encoding_table(Options) :-
	retractall(encoding_table(_,_)),
	(   option(encoding(Encoding), Options)
	->  create_encoding_table(Encoding)
	;   asserta(encoding_table(next_free_code, 0))
	).

create_encoding_table([]).
create_encoding_table([R-E|T]) :-
	atom(R), % assume R already expanded
	assert(encoding_table(R,E)),
	create_encoding_table(T).
create_encoding_table([NS:L-E|T]) :-
	rdf_global_object(NS:L, R), % expand namespace prefix to get R
	assert(encoding_table(R,E)),
	create_encoding_table(T).

get_encoding_table(Encoding) :-
	findall(R-E, (encoding_table(R,E), R \= next_free_code), Encoding).
encode_findings([], [], []).
encode_findings([H|T], [RefHead|RefTail], [HeadResult|TailResults]) :-
	encode_finding(H,  RefHead, HeadResult),
	encode_findings(T, RefTail, TailResults).

encode_finding(align(S,T,[P|_]), align(S,T,_), Encoding) :-
	member(relation(R), P),
	(   encoding_table(R,Encoding)
	->  true
	;   encoding_table(next_free_code, Encoding)
	->  NextFreeCode is Encoding + 1,
	    retractall(encoding_table(next_free_code, _)),
	    asserta(encoding_table(next_free_code, NextFreeCode)),
	    assertz(encoding_table(R,Encoding))
	;   throw(encoding_error(R))
	).

encode_finding(C, Ref, _Encoding) :-
	debug(irr, 'ag_irr: Warning ~p does not match reference ~w', [C,Ref]),
	fail.

vector_to_matrix([''],_, []).
vector_to_matrix(L, N, [Head|Tail]) :-
	select_first_row(L,N, Head, Rest),
	vector_to_matrix(Rest, N, Tail).

select_first_row(L, 0, [], L).
select_first_row([H|T], N, [Number|FirstRow], Rest) :-
	NewN is N - 1,
	atom_number(H, Number),
	select_first_row(T, NewN, FirstRow, Rest).
