:- module(ag_preferences, [
			   set_user_preferences/2
			  ]).

:- use_module(library(http/http_session)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(settings)).
:- use_module(cliopatria(hooks)).
:- use_module(user(user_db)).

:- rdf_meta
	cliopatria:user_preference_db(r,o),
	cliopatria:user_preference_default(r,o),
	set_user_preferences(r,o).


:- setting(user:lang, atom, en, 'Preferred language').

                 /*******************************
                 *   USER/SESSION PREFERENCES   *
                 *******************************/

%%      user_preference_db(?Property:atom, ?Value:rdf_object) is nondet.
%
%       Query properties for the current   user/session.  This mechanism
%       allows code to access information about the user/session without
%       committing to a particular  implementation.   The  predicate and
%       values are compatible with RDF to   allow  implementing the user
%       database in RDF, typically using the OpenID as subject.

cliopatria:user_preferences_db(Property, Value) :-
	logged_on(User, anonymous),
	http_session_data(rdf(User, Property, Value)).

set_user_preferences(Property, Value) :-
	cliopatria:user_preferences_db(Property, Value), !.

set_user_preferences(Property, Value) :-
	logged_on(User, anonymous),
	http_session_assert(rdf(User, Property, Value)).

%%      user_preference_default(?Property:atom, ?Value:rdf_object) is nondet.
%
%       Provides defaults for the user_preference/2.
%
%       @see user_preference_db/2

cliopatria:user_preference_default(Prop, literal(Value)) :-
	rdf_equal(Prop, user:lang),
	setting(user:lang, Value).
