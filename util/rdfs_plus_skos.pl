/*  This file is part of ClioPatria.

    Author:
    HTTP:	http://e-culture.multimedian.nl/
    GITWEB:	http://gollem.science.uva.nl/git/ClioPatria.git
    GIT:	git://gollem.science.uva.nl/home/git/ClioPatria.git
    GIT:	http://gollem.science.uva.nl/home/git/ClioPatria.git
    Copyright:  2007, E-Culture/MultimediaN

    ClioPatria is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 2 of the License, or
    (at your option) any later version.

    ClioPatria is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with ClioPatria.  If not, see <http://www.gnu.org/licenses/>.
*/

:- module(rdfs_plus_skos,
	  [ rdfs_plus_skos/5,      % +ExtMap, +QueryMap, ?S, ?P, ?O
	    rdfs_plus_skos/8,      % +ExtMap, +QueryMap, ?S, ?P, ?O, -RealS, -RealP, -RealO

	    rdfs_plus_skos_opt/4,  % ?S, ?P, ?O, +Options
	    rdfs_plus_skos_opt/7,  % ?S, ?P, ?O, -RealS, -RealP, -RealO, +Options

	    representative/3,      % +ExtMap, +R, -Representative
	    resource_ext_map/2,    % +Options, -Map
	    query_ext_map/2        % +Options, -Map
 	  ]).

:- use_module(library(assoc)).
:- use_module(library(error)).
:- use_module(library(debug)).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).


:- rdf_meta
        rdfs_plus_skos(+,+,r,r,o),
	rdfs_plus_skos(+,+,r,r,o,-,-,-),
	rdfs_plus_skos_opt(r,r,o,+),
	rdfs_plus_skos_opt(r,r,o,-,-,-,+),

	same(r,r),
	same(r,r,r),
	skos_broader(r,r),
	same_skos_broader(r,r,r),

	inverse_predicate(r,r).


rdf_optimise:rdf_db_goal(rdfs_plus_skos(_,_,S,P,O), S,P,O).
rdf_optimise:subj_branch_factor(rdfs_plus_skos(_,_,_,_,_), X, rdfs_subject_branch_factor(X)).
rdf_optimise:obj_branch_factor(rdfs_plus_skos(_,_,_,_,_), X, rdfs_object_branch_factor(X)).

rdf_optimise:rdf_db_goal(rdfs_individual_of(S,C), S,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',C).
rdf_optimise:subj_branch_factor(rdfs_individual_of(_,_), X, rdfs_subject_branch_factor(X)).
rdf_optimise:obj_branch_factor(rdfs_individual_of(_,_), X, rdfs_object_branch_factor(X)).


%%	rdfs_plus_skos_opt(?S, ?P, ?O, +Options).
%%      rdfs_plus_skos_opt(?S, ?P, ?O, ?RealS, ?RealP, ?RealO, +Options).
%
%	Same as rdf_has/4, but with optional reasoning.
%
%	Options
%        * owl_sameas
%        * owl_inverse
%        * owl_transitive
%        * owl_symmetric
%        * skos_exact
%        * skos_broader
%        * rdf_value

rdfs_plus_skos_opt(S, P, O, Options) :-
	rdfs_plus_skos_opt(S, P, O, _,_,_, Options).

rdfs_plus_skos_opt(S, P, O, RS, RP, RO, Options) :-
	instantiated(S, O, IMap),
	resource_ext_map(Options, RMap),
	query_ext_map(Options, QMap),
	rdfs_plus_skos_(IMap, RMap, QMap, S,P,O, RS,RP,RO).

%%	rdfs_plus_skos(+ExtMap, +QueryMap, ?S, ?P, ?O).
% %	rdfs_plus_skos(+ExtMap,+QueryMap, ?S, ?P, ?O, -RealS,-RealP,-RealO).
%
%	Do Goal and extend S,P,O to RealS,RealP,RealO based on resource
%	and property extension predicates.

rdfs_plus_skos(RMap, QMap, S, P, O) :-
	instantiated(S, O, IMap),
	rdfs_plus_skos_(IMap, RMap, QMap, S, P, O, _, _, _).
rdfs_plus_skos(RMap, QMap, S, P, O, RealS, RealP, RealO) :-
	instantiated(S, O, IMap),
	rdfs_plus_skos_(IMap, RMap, QMap, S, P, O, RealS, RealP, RealO).

rdfs_plus_skos_(0b00, _RMap, QMap, S,P,O, S,RP,O) :-
	rdf_has_ext(QMap, S, P, O, RP).
rdfs_plus_skos_(0b10, RMap, QMap, S,P,O, RS,RP,O) :-
	resource_ext(RMap, S, RS),
     	rdf_has_ext(QMap, RS, P, O, RP).
rdfs_plus_skos_(0b01, RMap, QMap, S,P,O, S,RP,RO) :-
	resource_ext(RMap, O, RO),
     	rdf_has_ext(QMap, S, P, RO, RP).
rdfs_plus_skos_(0b11, RMap, QMap, S,P,O, RS,RP,RO) :-
	resource_ext(RMap, S, RS),
	resource_ext(RMap, O, RO),
     	rdf_has_ext(QMap, RS, P, RO, RP).


%%	resource_ext(+Map, +R0, -R)
%
%	True if R0 is R or derivable as defined by Map.

resource_ext(0b000, R, R).

resource_ext(0b100, R0, R) :-
	same(owl:sameAs, R0, R).
resource_ext(0b010, R0, R) :-
	same(skos:exactMatch, R0, R).
resource_ext(0b001, R0, R) :-
	skos_broader(R0, R).

resource_ext(0b110, R0, R) :-
	rdf_global_term([owl:sameAs,skos:exactMatch], Ps),
	same(Ps, R0, R).
resource_ext(0b101, R0, R) :-
	same_skos_broader(owl:sameAs, R0, R).
resource_ext(0b011, R0, R) :-
	same_skos_broader(skos:exactMatch, R0, R).

resource_ext(0b111, R0, R) :-
	rdf_equal(P1,owl:sameAs),
	rdf_equal(P2,skos:exactMatch),
	same_skos_broader([P1,P2], R0, R).


%%	rdf_has_ext(+QMap, ?S, ?P, ?O, -RealP)
%
%	As rdf_has/4, but with additional reasoning as indicated by
%	QMap, see query_ext_map/2.

rdf_has_ext(0b0000, S, P, O, RealP) :-
	rdf_has(S, P, O, RealP).

rdf_has_ext(0b1000, S, P, O, RealP) :-
	rdfs_plus_inverse(S, P, O, RealP).
rdf_has_ext(0b0100, S, P, O, RealP) :-
	rdfs_plus_symmetric(S, P, O, RealP).
rdf_has_ext(0b0010, S, P, O, RealP) :-
	rdfs_plus_transitive(S, P, O, RealP).
rdf_has_ext(0b0001, S, P, O, RealP) :-
	rdfs_plus_value(S, P, O, RealP).

rdf_has_ext(0b1100, S, P, O, RealP) :-
	rdfs_plus_inverse_symmetric(S, P, O, RealP).
rdf_has_ext(0b1010, S, P, O, RealP) :-
	rdfs_plus_inverse_transitive(S, P, O, RealP).
rdf_has_ext(0b1001, S, P, O, RealP) :-
	rdfs_plus_inverse_value(S, P, O, RealP).
rdf_has_ext(0b0110, S, P, O, RealP) :-
	rdfs_plus_symmetric_transitive(S, P, O, RealP).
rdf_has_ext(0b0101, S, P, O, RealP) :-
	rdfs_plus_symmetric_value(S, P, O, RealP).
rdf_has_ext(0b0011, S, P, O, RealP) :-
	rdfs_plus_transitive_value(S, P, O, RealP).

rdf_has_ext(0b1110, S, P, O, RealP) :-
	rdfs_plus_inverse_symmetric_transitive(S, P, O, RealP).
rdf_has_ext(0b1011, S, P, O, RealP) :-
	rdfs_plus_inverse_transitive_value(S, P, O, RealP).
rdf_has_ext(0b1101, S, P, O, RealP) :-
	rdfs_plus_inverse_symmetric_value(S, P, O, RealP).
rdf_has_ext(0b0111, S, P, O, RealP) :-
	rdfs_plus_symmetric_transitive_value(S, P, O, RealP).

rdf_has_ext(0b1111, S, P, O, RealP) :-
	rdfs_plus_inverse_symmetric_transitive_value(S, P, O, RealP).










%%	rdfs_plus_value(?S, ?P, ?O, -SP)
%
%	Same as rdf_has/4 but return the rdf:value of Object in case it
%	exists. If Object is ground test both for existence of the
%	normal triple as well as Object as an rdf:value.

rdfs_plus_value(S, P, O, RP) :-
	ground(O), !,
	(   rdf_has(S, P, O, RP)
	;   rdf_has(O0, rdf:value, O),
	    rdf_has(S, P, O0, RP)
	).
rdfs_plus_value(S, P, O, RP) :-
	rdf_has(S, P, O0, RP),
	(   rdf_has(O0, rdf:value, O)
	->  true
	;   O = O0
	).

%%	rdfs_plus_inverse(?S, ?P, ?O, ?RealP)
%
%	As rdf_has/4 but include inverse properties of P.

rdfs_plus_inverse(S, P, O, RP) :-
	rdf_has(S, P, O, RP).
rdfs_plus_inverse(S, P, O, RP) :-
	ground(P), !,
	inverse_predicate(P, IP),
	rdf_has(O, IP, S, RP).

%%	rdfs_plus_transitive(?S, ?P, ?O, ?RealP)
%
%	As rdf_has/4 but use rdf_reachable/3 for transitive properties.
%
%       @TBD RealP is not correct.

rdfs_plus_transitive(S, P, O, P) :-
	ground(P),
	rdfs_individual_of(P, owl:'TransitiveProperty'), !,
	rdf_reachable(S, P, O).
rdfs_plus_transitive(S, P, O, RP) :-
	rdf_has(O, P, S, RP).

%%	rdfs_plus_symmetric(?S, ?P, ?O, ?RealP)
%
%	As rdf_has/4 but include the inverse for symmetric properties.

rdfs_plus_symmetric(S, P, O, RP) :-
	rdf_has(S, P, O, RP).
rdfs_plus_symmetric(S, P, O, RP) :-
	ground(P),
	rdfs_individual_of(P, owl:'SymmetricProperty'), !,
	rdf_has(O, P, S, RP).

%%	rdfs_plus_inverse_symmetric(?S, ?P, ?O, ?RealP).
%%	rdfs_plus_inverse_transitive(?S, ?P, ?O, ?RealP).
%%	rdfs_plus_inverse_value(?S, ?P, ?O, ?RealP).
%%	rdfs_plus_symmetric_transitive(?S, ?P, ?O, ?RealP).
%%	rdfs_plus_symmetric_value(?S, ?P, ?O, ?RealP).
%%	rdfs_plus_transitive_value(?S, ?P, ?O, ?RealP).
%
%	As rdf_has/4 but include two types of reasoning.

rdfs_plus_inverse_symmetric(S, P, O, RP) :-
	rdf_has(S, P, O, RP).
rdfs_plus_inverse_symmetric(S, P, O, RP) :-
	ground(P),
	inverse_predicate(P, IP),
	rdf_has(O, IP, S, RP).
rdfs_plus_inverse_symmetric(S, P, O, RP) :-
	ground(P),
	rdfs_individual_of(P, owl:'SymmetricProperty'), !,
	rdf_has(O, P, S, RP).

rdfs_plus_inverse_transitive(S, P, O, RP) :-
	rdfs_plus_transitive(S, P, O, RP).
rdfs_plus_inverse_transitive(S, P, O, RP) :-
	ground(P), !,
	inverse_predicate(P, IP),
	rdfs_plus_transitive(O, IP, S, RP).

rdfs_plus_inverse_value(S, P, O, RP) :-
	rdfs_plus_value(S, P, O, RP).
rdfs_plus_inverse_value(S, P, O, RP) :-
	ground(P), !,
	inverse_predicate(P, IP),
	rdfs_plus_value(O, IP, S, RP).

rdfs_plus_symmetric_transitive(S, P, O, RP) :-
	rdfs_plus_transitive(S, P, O, RP).
rdfs_plus_symmetric_transitive(S, P, O, RP) :-
	ground(P),
	rdfs_individual_of(P, owl:'SymmetricProperty'), !,
	rdfs_plus_transitive(O, P, S, RP).

rdfs_plus_symmetric_value(S, P, O, RP) :-
	rdfs_plus_value(S, P, O, RP).
rdfs_plus_symmetric_value(S, P, O, RP) :-
	ground(P),
	rdfs_individual_of(P, owl:'SymmetricProperty'), !,
	rdfs_plus_value(O, P, S, RP).

rdfs_plus_transitive_value(S, P, O, RP) :-
	var(O), !,
	rdfs_plus_transitive(S, P, O0, RP),
	(   rdf_has(O0, rdf:value, O)
	->  true
	;   O = O0
	).
rdfs_plus_transitive_value(S, P, O, RP) :-
	ground(O), !,
	(   rdfs_plus_transitive(S, P, O, RP)
	;   rdf_has(O0, rdf:value, O),
	    rdfs_plus_transitive(S, P, O0, RP)
	).

%%	rdfs_plus_inverse_symmetric_transitive(?S, ?P, ?O, -RealP).
%%	rdfs_plus_inverse_transitive_value(?S, ?P, ?O, -RealP).
%%	rdfs_plus_symmetric_transitive_value(?S, ?P, ?O, -RealP).
%%	rdfs_plus_inverse_symmetric_value(?S, ?P, ?O, -RealP).
%
%	As rdf_has/4, but include 3 types of reasoning.

rdfs_plus_inverse_symmetric_transitive(S, P, O, RP) :-
	rdfs_plus_transitive(S, P, O, RP).
rdfs_plus_inverse_symmetric_transitive(S, P, O, RP) :-
	ground(P),
	inverse_predicate(P, IP),
	rdfs_plus_transitive(O, IP, S, RP).
rdfs_plus_inverse_symmetric_transitive(S, P, O, RP) :-
	ground(P),
	rdfs_individual_of(P, owl:'SymmetricProperty'), !,
	rdfs_plus_symmetric(O, P, S, RP).

rdfs_plus_inverse_transitive_value(S, P, O, RP) :-
	var(O), !,
	rdfs_plus_inverse_transitive(S, P, O0, RP),
	(   rdf_has(O0, rdf:value, O)
	->  true
	;   O = O0
	).
rdfs_plus_inverse_transitive_value(S, P, O, RP) :-
	ground(O), !,
	(   rdfs_plus_inverse_transitive(S, P, O, RP)
	;   rdf_has(O0, rdf:value, O),
	    rdfs_plus_inverse_transitive(S, P, O0, RP)
	).

rdfs_plus_symmetric_transitive_value(S, P, O, RP) :-
	var(O), !,
	rdfs_plus_symmetric_transitive(S, P, O0, RP),
	(   rdf_has(O0, rdf:value, O)
	->  true
	;   O = O0
	).
rdfs_plus_symmetric_transitive_value(S, P, O, RP) :-
	ground(O), !,
	(   rdfs_plus_symmetric_transitive(S, P, O, RP)
	;   rdf_has(O0, rdf:value, O),
	    rdfs_plus_symmetric_transitive(S, P, O0, RP)
	).

rdfs_plus_inverse_symmetric_value(S, P, O, RP) :-
	var(O), !,
	rdfs_plus_inverse_symmetric(S, P, O0, RP),
	(   rdf_has(O0, rdf:value, O)
	->  true
	;   O = O0
	).
rdfs_plus_inverse_symmetric_value(S, P, O, RP) :-
	ground(O), !,
	(   rdfs_plus_inverse_symmetric(S, P, O, RP)
	;   rdf_has(O0, rdf:value, O),
	    rdfs_plus_inverse_symmetric(S, P, O0, RP)
	).


rdfs_plus_inverse_symmetric_transitive_value(S, P, O, RP) :-
	var(O), !,
	rdfs_plus_inverse_symmetric_transitive(S, P, O0, RP),
	(   rdf_has(O0, rdf:value, O)
	->  true
	;   O = O0
	).
rdfs_plus_inverse_symmetric_transitive_value(S, P, O, RP) :-
	ground(O), !,
	(   rdfs_plus_inverse_symmetric_transitive(S, P, O, RP)
	;   rdf_has(O0, rdf:value, O),
	    rdfs_plus_inverse_symmetric_transitive(S, P, O0, RP)
	).



%%	same_skos_broader(+Eq, +R0, -R).
%
%	True if R is RO or reachable through Eq or skos:broader.

same_skos_broader(Eq, R0, R) :-
	same(Eq, R0, R1),
	skos_broader(R1, R2),
	same(Eq, R2, R).

%%	skos_broader(?R0, ?R)
%
%	True if R = R0 or reachable true skos:broader or inversly
%	through skos:narrower.

skos_broader(R0, R) :-
	rdf_reachable(R, skos:broader, R0).
skos_broader(R0, R) :-
	rdf_reachable(R0, skos:narrower, R).

%%      same(+EqP, +R0, -R) is nondet.
%%	same(+EqP, -R0, +P) is nondet.
%
%	True if R is R0 or reachable through P.

same(P, R0, R) :-
	atom(R0), !,
	empty_assoc(V0),
	put_assoc(R0, V0, true, V),
	same_(R0, R, P, V).
same(P, R0, R) :-
	atom(R), !,
	same(P, R, R0).
same(_P, R0, _R) :-
	instantiation_error(R0).

same_(R, R, _, _).
same_(R0, R, Ps, V) :-
	(   is_list(Ps)
	->  member(P, Ps)
	;   P = Ps
	),
	same_inv(R0,P,R1),
	\+ get_assoc(R1, V, true),
	put_assoc(R1, V, true, V2),
	same_(R1, R, P, V2).


same_inv(R0,P,R1) :-
	rdf_has(R0,P,R1).
same_inv(R0,P,R1) :-
	rdf_has(R1,P,R0).

%%	inverse_predicate(+P1, +P2) is semidet.
%
%	True if P1 and P2 are each others inverses.

inverse_predicate(P1, P2) :-
	rdf_has(P1, owl:inverseOf, P2), !.
inverse_predicate(P1, P2) :-
	rdf_has(P2, owl:inverseOf, P1).


%%      representative(+EMap, +R, -Representative)
%
%	Representative is the representative URI for a set of resources
%       equivalent with R.

representative(EMap, R, Represent) :-
	EMap > 1, !,
	equivalence_set(EMap, R, Set0),
	sort(Set0, [Represent|_]).
representative(_EMap, R, R).

%%	equivalence_set(+EMap, +R, -Set)
%
%	Set contains R and all its equivalent resources.

equivalence_set(EMap, R, Set) :-
	(   EMap >= 6
	->  Ps = [owl:sameAs,skos:exactMatch]
	;   EMap >= 4
	->  Ps = [owl:sameAs]
	;   EMap >= 2
	->  Ps = [skos:exactmatch]
	),
	findall(S, same(Ps, R, S), Set).

%%	instantiated(?S,?O,-Bitmap)
%
%	Bitmap indicates instantiation of S and O.

instantiated(S,O,I) :-
	( atom(S) -> I0  =     0b10 ; I0  =     0b00 ),
	( atom(O) -> I  is I0\/0b01 ; I  is I0\/0b00 ).

%%	resource_map(+Options, -Map)
%
%	Bitmap indicates reseaning over resources.

resource_ext_map(Opt, M) :-
	( memberchk(owl_sameas, Opt)   -> M0 = 0b100 ; M0 = 0b000 ),
	( memberchk(skos_exact, Opt)   -> M1 is M0\/0b010 ; M1 is M0\/0b000 ),
	( memberchk(skos_broader, Opt) -> M is M1\/0b001 ; M is M1\/0b000 ).

%%	query_ext_map(+Options, -Map)
%
%	Bitmap indicates reasoning.

query_ext_map(Opt, M) :-
	( memberchk(owl_inverse, Opt)    -> M0 = 0b1000 ; M0 = 0b0000 ),
	( memberchk(owl_symmetric, Opt)  -> M1 is M0\/0b0100 ; M1 is M0\/0b0000 ),
	( memberchk(owl_transitive, Opt) -> M2 is M1\/0b0010 ; M2 is M1\/0b0000 ),
	( memberchk(rdf_value, Opt)      -> M  is M2\/0b0001 ; M  is M2\/0b0000 ).





:- rdf_register_ns(t, 'http://test.com/').

%%	assert_test_graph
%
%	Create simple test graph.

assert_test_graph :-
 	rdf_assert(t:work1, t:location, t:paris1),
	rdf_assert(t:work1, owl:sameAs, t:work2),
	rdf_assert(t:work3, owl:sameAs, t:work1),
	rdf_assert(t:work3, t:location, t:paris3),
	rdf_assert(t:paris3, skos:exactMatch, t:paris4),
	rdf_assert(t:paris4, skos:broader, t:france4),
	rdf_assert(t:paris1, skos:broader, t:france1),
	rdf_assert(t:france1, skos:exactMatch, t:france2),
	rdf_assert(t:location, owl:inverseOf, t:locationOf),
	rdf_assert(t:work4, t:location, t:place),
	rdf_assert(t:place, rdf:value, t:paris4).


%%	rdfs_plus_skos_test(+Test)
%
%	Tests rdfs_plus_skos reasoning.

rdfs_plus_skos_test(1) :-
	\+ rdfs_plus_skos_opt(t:work1, t:location, t:france1, []).
rdfs_plus_skos_test(2) :-
	rdfs_plus_skos_opt(t:work1, t:location, t:france1,
			   [skos_broader]).
rdfs_plus_skos_test(3) :-
	\+ rdfs_plus_skos_opt(t:work1, t:location, t:france2,
			      [skos_broader]).
rdfs_plus_skos_test(4) :-
	rdfs_plus_skos_opt(t:work1, t:location, t:france2,
			   [skos_broader, skos_exact]).
rdfs_plus_skos_test(5) :-
	\+ rdfs_plus_skos_opt(t:work2, t:location, t:paris3,
			      [skos_broader]).
rdfs_plus_skos_test(6) :-
	rdfs_plus_skos_opt(t:work2, t:location, t:paris3,
			   [skos_broader, owl_sameas]).
rdfs_plus_skos_test(7) :-
	\+ rdfs_plus_skos_opt(t:work2, t:location, t:france4,
			      [skos_broader, owl_sameas]).
rdfs_plus_skos_test(8) :-
	rdfs_plus_skos_opt(t:work2, t:location, t:france4,
			   [skos_broader, owl_sameas, skos_exact]).
rdfs_plus_skos_test(9) :-
	rdfs_plus_skos_opt(t:paris1, t:locationOf, t:work1,
		       [owl_inverse]).
rdfs_plus_skos_test(10) :-
	rdfs_plus_skos_opt(t:france1, t:locationOf, t:work1,
			   [owl_inverse, skos_broader]).
rdfs_plus_skos_test(11) :-
	rdfs_plus_skos_opt(t:france2, t:locationOf, t:work1,
			   [owl_inverse, skos_broader, skos_exact]).
rdfs_plus_skos_test(12) :-
	rdfs_plus_skos_opt(t:france4, t:locationOf, t:work2,
			   [owl_inverse, skos_broader, owl_sameas, skos_exact]).
rdfs_plus_skos_test(13) :-
	\+ rdfs_plus_skos_opt(t:work4, t:location, t:paris4, []).
rdfs_plus_skos_test(14) :-
	rdfs_plus_skos_opt(t:work4, t:location, t:paris4,
			   [rdf_value]).
rdfs_plus_skos_test(15) :-
	rdfs_plus_skos_opt(t:work4, t:location, t:france4,
			   [skos_broader, rdf_value]).



