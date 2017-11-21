/** <module> wn_subset
 *	define caching and counting scheme for
 *	more efficient subset/2 relation
 */

:- module(wn_subset, [setup_hyp_counters/0]).
:- use_module(wn3_db).

% since we are going to remove the cycle from the hyp/2 relation
% use this inclusion, alternative to ensure_loaded

% this is the desired relation
% synset_anc_desc(SynsetId, NumAncestors, NumDescendants)
%
:- dynamic synset_anc_desc/3.

% service counters
:- dynamic hyp_ancestors/2.
:- dynamic hyp_descendants/2.

% initialize service counters, and construct synset_anc_desc/3 issuing
%  a full counting up/down hierarchy for *each* synset_id existing in hyp/2
%
setup_hyp_counters :-
	clear_hyp_counters,
	kill_hyp_bug,
	% get all synset_ids from hyp/2
	setof(Hyp, X^(wn3_db:hyp(Hyp,X) ; wn3_db:hyp(X,Hyp)), AllHyp),
	maplist(setup_hyp_counters, AllHyp),
	feedback(hyp_counters_done).

% remove all the service counters from memory
%
clear_hyp_counters :-
	retractall(hyp_ancestors(_, _)),
	retractall(hyp_descendants(_, _)),
	retractall(synset_anc_desc(_, _, _)).

% remove the bug from hyp/2
kill_hyp_bug :-
	retract(wn3_db:hyp(202422663, 202423762)), feedback(bug_killed(hyp(202422663, 202423762))), ! ; true.

setup_hyp_counters(Hyp) :-
	count_hyp_ancestors(Hyp, As),
	count_hyp_descendants(Hyp, Ds),
	assertz(synset_anc_desc(Hyp, As, Ds)).

%% count_hyp_ancestors(H, N) is nondet.
%
%  counts the number N of ancestors of hypernim H
%
%  @arg H is an hypernim synset id
%  @arg N number of ancestors induced by relation hyp/2
%
count_hyp_ancestors(H, N) :-
	(	hyp_ancestors(H, N)
	->	true
	;	(	setof(P, wn3_db:hyp(P, H), Ps)
		->	maplist(count_hyp_ancestors, Ps, Ns),
			sum_list(Ns, Nc),
			length(Ps, Pl),
			N is Nc + Pl
		;	N = 0
		),
		assertz(hyp_ancestors(H, N))
	).

%% count_hyp_descendants(H, N) is nondet.
%
%  counts the number N of descendants of hypernim H<
%
%  @arg H is an hypernim synset id
%  @arg N number of descendants induced by relation hyp/2
%
count_hyp_descendants(H, N) :-
	(	hyp_descendants(H, N)
	->	true
	;	(	setof(D, wn3_db:hyp(H, D), Ds)
		->	maplist(count_hyp_descendants, Ds, Ns),
			sum_list(Ns, Nc),
			length(Ds, Dl),
			N is Nc + Dl
		;	N = 0
		),
		assertz(hyp_descendants(H, N))
	).

% start a visit from H, binding *either* Descendants, Cycle
% depending on the fact that a cycle is actually found (that should not be)
%
descendants(H, Descendants, Cycle) :-
	descendants(H, [], Descendants, Cycle).

descendants(H, Visited, Descendants, Cycle) :-
	(	memberchk(H, Visited)
	->	Cycle = Visited
	;	(	wn3_db:hyp(H, D)
		->	descendants(D, [H|Visited], Descendants, Cycle)
		;	Descendants = Visited
		)
	).

% by mail 2/6/2014
above(I, K):-
	hyp(I, J),
	(J = K; above(J, K)).

below(K, I):-
	inv_hyp(K, J),
	(J = I; below(J, I)).

ancestor(I, J):-		% I just reformatted it
	syn(I, X1, Y1),	% X1 is singleton - why ?
	syn(J, X2, Y2),
	(	(Y2 < Y1)
	-> 	fail
	;	Y1 < X2
	->	above(I, J)
	;	below(J, I)
	).
% by mail 2/6/2014

% give feedback about critical operations
feedback(M) :- print_message(informational, M).

:- multifile prolog:message//1.

prolog:message(bug_killed(Rec)) -->
        [ 'bug_killed ~w'-[Rec] ].

prolog:message(hyp_counters_done) -->
	{	predicate_property(hyp_ancestors(_,_), number_of_clauses(TotAnc)),
		predicate_property(hyp_descendants(_,_), number_of_clauses(TotDesc)),
		predicate_property(synset_anc_desc(_,_,_), number_of_clauses(TotSyn))
	},
        [ 'ancestors: ~d, descendants ~d, synsets_anc_desc ~d'-[TotAnc, TotDesc, TotSyn] ].
