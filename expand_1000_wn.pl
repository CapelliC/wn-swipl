/** <module> expand_1000_wn
 */

:- module(expand_1000_wn, [expand_1000_wn/0]).

:- dynamic hyp/2.
:- include('WN/wn_hyp').
:- ensure_loaded('WN/wn_s').

:- include('1000W').

:- dynamic word_derived_distance/3.

%%  expand_1000_wn is det.
%
%   .
%
expand_1000_wn :-
	retractall(word_derived_distance(_,_,_)),
	% kill the bug
	( retract(hyp(202422663, 202423762)) ; true ),
	forall(word(W), compute_derived(W)).

compute_derived(Word) :-
	synset_word(Synset, Word),
	hyp(Synset, SubSynset),
	compute_derived(Word, SubSynset, 1).

compute_derived(Word, SubSynset, Distance) :-
	synset_word(SubSynset, SubWord),
	remember(Word, SubWord, Distance),
	hyp(SubSynset, Deeper),
	Distance1 is Distance+1,
	compute_derived(Word, Deeper, Distance1).

synset_word(Synset, Word) :-
	s(Synset, _,Word,_,_,_)
/*
the line below is a filter, that restricts
derived synsets to *only* the words in top 1000s

without the line, it took a long time

5 ?- time(expand_1000_wn).
% 12,516,616 inferences, 408.794 CPU in 499.028 seconds (82% CPU, 30618 Lips)
true.

with that line, we have only N = 5672.

6 ?- time(expand_1000_wn).
% 174,890 inferences, 0.307 CPU in 0.319 seconds (96% CPU, 568976 Lips)
true.
*/
	,word(Word).

remember(Word, SubWord, Distance) :-
	(	word_derived_distance(Word, SubWord, Distance)
	->	true
	;	assertz(word_derived_distance(Word, SubWord, Distance))
%,writeln(word_derived_distance(Word, SubWord, Distance))
	).
