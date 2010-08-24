:- module(levenshtein,
	  [levenshtein/3 % +atom, +atom, -integer
	  ]
	 ).
% Implementation of the computation of the leveshtein distance of two words.
% This implementation works on all ISO-compatible implemenations of Prolog
% which implement append(?list, ?list, ?list) in the usual way.

% levenshtein(+atom, +atom, -integer): Computes the Levenshtein distance between two given words
% W1: Atom
% W2: Atom
% D : Distance
levenshtein(W1, W2, D) :- atom_length(W1, 0), atom_length(W2, L), D is L, !.
levenshtein(W1, W2, D) :- atom_length(W2, 0), atom_length(W1, L), D is L, !.
levenshtein(W1, W2, D) :- atom_length(W1, L1), initialList(L1, StartRow), lev(W1, W2, [], StartRow, 1, 1, 0, D), !.

% lev(+W1, +W2, +CR, +LR, +P1, +P2, +CD, -D): Rule which represents the actual calculation. This rule
% works recursively simulating a loop over the positions (P1, P2) in the words W1 and W2. The outer
% loop is over P2, the inner loop is over P1. To keep track of previously computed distances two lists
% are used.
% W1: Atom
% W2: Atom
% CR: Start of the row (current row)
% LR: Remainder of last row (last row)
% P1: Position in word W1
% P2: Position in word W2
% CD: Current value of distance; this is the last element of CR
% D : Value of distance

% Verify the end-condition for the recursion first: This is achieved when the P2-counter exceeds the length of W2
lev(_, W2, _, _, _, P2, CD, D) :- atom_length(W2, L2), P2 > L2, D is CD, !.

% The initial rule after P2 has been increased by 1
lev(W1, W2, [], LR, 1, P2, _, D) :- CR = [P2], lev(W1, W2, CR, LR, 1, P2, P2, D), !.

% This is the actual computational rule
lev(W1, W2, CR, [H1, H2|LR], P1, P2, CD, D) :-
		% T1 is the distance at (P1, P2) obtained from removing the letter at P1 from W1
		T1 is CD + 1,
		% T2 is the distance at (P1, P2) obtained from adding the letter at P2 from W2
		T2 is H2 + 1,
		charAt(W1, P1, C1), charAt(W2, P2, C2),
		% T3 is the distance at (P1, P2) obtained from exchanging the letters at P1 from W1 and P2 from W2
		(  C1 = C2 -> T3 is H1; T3 is H1 + 1 ),
		% The distance at (P1, P2) is the minimum of the previously computed distances
		min(T1, T2, T3, NCD),
		% Compute the next value for CR, increase the P1 position by i and the recursively call lev(...)
		append(CR, [NCD], NCR), NP1 is P1 + 1,
		lev(W1, W2, NCR, [H2|LR], NP1, P2, NCD, D), !.

% This rule verifies the end of line condition for the loop over P1
lev(W1, W2, CR, [_], _, P2, CD, D) :- NP2 is P2 + 1, lev(W1, W2, [], CR, 1, NP2, CD, D), !.

% initialList(+N, -L): Returns a list L of the form [0, 1, 2, ..., N].
initialList(N, _) :- (\+ integer(N); N < 1) -> fail.
initialList(1, L) :- L = [0, 1], !.
initialList(N, L) :- N1 is N - 1, L2 = [N], initialList(N1, L1), append(L1, L2, L).

% charAt(+A, ?N, -C): Returns the character at position N in the atom A
% The position is 1-based
% A: The atom
% N: The position at which to extract the character
% C: The character of A at position N
charAt(A, N, C) :- P is N - 1, sub_atom(A, P, 1, _, C).

% min(...): These rules compute the minimum of the given integer values
% I1, I2, I3: Integer values
% M:          The minimum over the values
min(I1, I2, M) :- integer(I1), integer(I2), ( I1 =< I2 -> M is I1; M is I2).
min(I1, I2, I3, M) :- min(I1, I2, A), min(I2, I3, B), min(A, B, M).
