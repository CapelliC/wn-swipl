% since we are going to remove the cycle from the hyp/2 relation
% use this inclusion, alternative to ensure_loaded
:- dynamic hyp/2.
:- include('WN/wn_hyp').
:- dynamic hyp_ancestors/2.
:- dynamic hyp_descendants/2.

% this is the desired relation
% synset_anc_desc(SynsetId, NumAncestors, NumDescendants)
%
:- dynamic synset_anc_desc/3.

% remove all the service counters from memory
%
clear_hyp_counters :-
	retractall(hyp_ancestors(_, _)),
	retractall(hyp_descendants(_, _)),
	retractall(synset_anc_desc(_, _, _)).

% initialize service counters, and construct synset_anc_desc/3 issuing
%  a full counting up/down hierarchy for *each* synset_id existing in hyp/2
%
setup_hyp_counters :-
	clear_hyp_counters,
	% remove the bug
	( retract(hyp(202422663, 202423762)) ; true ),
	% get all synset_ids from hyp/2
	setof(Hyp, X^(hyp(Hyp,X) ; hyp(X,Hyp)), AllHyp),
	maplist(setup_hyp_counters, AllHyp).

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
	;	(	setof(P, hyp(P, H), Ps)
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
	;	(	setof(D, hyp(H, D), Ds)
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
	;	(	hyp(H, D)
		->	descendants(D, [H|Visited], Descendants, Cycle)
		;	Descendants = Visited
		)
	).

hyp_descendants_cycle(H, C) :-
	root_hyp(H), descendants(H, _, C), nonvar(C).

root_hyp(H) :- hyp(H, _), \+ hyp(_, H).
leaf_hyp(L) :- hyp(_, L), \+ hyp(L, _).

% debug helpers
search_and_show_hyp_cycles :-
	forall(hyp_descendants_cycle(X,L),writeln(X/L)).

% debug helper: let we know the inclusion code is working
print_one :-
	hyp(X,Y), write(hyp(X,Y)), nl.

/*
ancestors2(H, Visited, Ancestors) :-
	(hyp(A,H)->
?	ancestors2(A, [H|Visited], Ancestors);
	Ancestors = Visited).
*/
ancestors2(H, Visited, Ancestors) :-
	hyp(A,H), ancestors2(A, [H|Visited], Ancestors)
	;
	Ancestors = Visited.

descendants2(H, Visited, Descendants) :-
	(hyp(H, D)->
	 descendants2(D, [H|Visited], Descendants);
	 Descendants = Visited).
