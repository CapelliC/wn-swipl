/** <module> wordnet
 *
 *  explore Wordnet,NodeBox basic resources
 *  -----
 *
 *  utilities to compare Wordnet versions, qcompile WN3, and list verb tenses from NodeBox
 *
 *  source file /home/carlo/prolog/wordnet.pl, created at Mar 16 2013
 *
 *  @author carlo
 *  @version 0.9.9
 *  @copyright carlo
 *  @license LGPL v2.1
 */

:- module(wordnet,
	[load_full_db/0
	,load_relation/2
	,load_all_relations/0
	,export_all_relations/0
	,load_all_relations/1
	,make_qlf_full/0
	,make_qlf_full/1
	,compare_2_vs_3/0
	,compare_subset/0
	,verb_tenses_load/0
	,verb_tenses/12
	,isa_graph/0
	,wordnet_schema/0
	,predicate_indicator/1
	,graph_hyp/1
	,subsynset/2
	,subsynset/3
	,subsynset_path/3
	,subsynset_topdown/3
	,hyp_chain_words/2
	,root_hyp/1
	,leaf_hyp/1
	,count_synset_daughters/2
	,s/6
	,sk/3
	,g/2
	,syntax/3
	,hyp/2
	,ins/2
	,ent/2
	,sim/2
	,mm/2
	,ms/2
	,mp/2
	,der/4
	,cls/5
	,cs/2
	,vgp/4
	,at/2
	,ant/4
	,sa/4
	,ppl/4
	,per/4
	,fr/3
	]).

:- dynamic
	 s/6
	,sk/3
	,g/2
	,syntax/3
	,hyp/2
	,ins/2
	,ent/2
	,sim/2
	,mm/2
	,ms/2
	,mp/2
	,der/4
	,cls/5
	,cs/2
	,vgp/4
	,at/2
	,ant/4
	,sa/4
	,ppl/4
	,per/4
	,fr/3.

% :- use_module(library(semweb/rdf_db)).
:- use_module(library(ordsets)).
:- use_module(library(record)).
:- use_module(carlo(lifter)).
:- use_module(loqt(pqGraphviz/test/odbc/graph_schema)).

/*
:- ensure_loaded(wordnet(wn_s)).
:- ensure_loaded(wordnet(wn_sk)).
:- ensure_loaded(wordnet(wn_g)).
:- ensure_loaded(wordnet(wn_syntax)).
:- ensure_loaded(wordnet(wn_hyp)).
:- ensure_loaded(wordnet(wn_ins)).
:- ensure_loaded(wordnet(wn_ent)).
:- ensure_loaded(wordnet(wn_sim)).
:- ensure_loaded(wordnet(wn_mm)).
:- ensure_loaded(wordnet(wn_ms)).
:- ensure_loaded(wordnet(wn_mp)).
:- ensure_loaded(wordnet(wn_der)).
:- ensure_loaded(wordnet(wn_cls)).
:- ensure_loaded(wordnet(wn_cs)).
:- ensure_loaded(wordnet(wn_vgp)).
:- ensure_loaded(wordnet(wn_at)).
:- ensure_loaded(wordnet(wn_ant)).
:- ensure_loaded(wordnet(wn_sa)).
:- ensure_loaded(wordnet(wn_ppl)).
:- ensure_loaded(wordnet(wn_per)).
:- ensure_loaded(wordnet(wn_fr)).
*/

%%	wn(+V,-A) is det
%
%	expand file name on version number
%
wn(V, A)  :- format(atom(A), '~~/prolog/wordnet~d/prolog', [V]).

%w3(P)     :- wn(30, P).
w3(P)     :- P = '~/prolog/wordnet-3.0/prolog'.
mkp(L, P) :- atomic_list_concat(L, /, P).
pls(P, Q) :- mkp([P, '*.pl'], Q).

%%	load_full_db is det.
%
%	consult all files of WN3
%
load_full_db :-
	maplist(user:consult, expand_file_name(pls(w3(°), °), °)).

%%	compare_2_vs_3 is det.
%
%	make a comparison of files available in WN2 and WN3
%
compare_2_vs_3 :-
	maplist(get_file_names, [20,30], [N2,N3]),
	ord_symdiff(N2,N3,ND),
	writeln(ND).

get_file_names(V, S) :-
	expand_file_name(pls(wn(V, °), °), L),
	maplist(get_file_name, L, N),
	list_to_ord_set(N, S).

get_file_name(F, N) :-
	file_base_name(F, B),
	file_name_extension(N, pl, B).

%%	compare_subset is det.
%
%	compare Witzig generated subset files with WN3
%
compare_subset :-
	maplist(writeln, sort(expand_file_name('~/prolog/pronto/witzig/wn_subset/wn_*_subset.pl', °), °)),
	nl,
	maplist(writeln, sort(expand_file_name('~/prolog/wordnet30/prolog/wn_*.pl', °), °)).

%%	load_relation(+R, ?A) is semidet.
%
%	consult the relation R specified, extract arity A
%
load_relation(R, A) :-
	format(atom(P), '~s/wn_~s', [w3(°), R]),
	consult(P),
	current_predicate(R/A).

%%	load_all_relations is det.
%
%	load all relations of WN3
%
load_all_relations :- load_all_relations(wordnet).

export_all_relations :-
	load_all_relations,
	schema(S),
	forall(member(E/_, S), (E =.. [F|As], length(As, Ac), export(F/Ac))).

	/*expand_file_name(mkp([w3(°), 'wn_*'], °), L),
	setof(N, X^(member(X, L), file_name_extension(N, pl, X)), Q),
	maplist(user:consult, Q).*/

%% load_all_relations(+Module) is det.
%
%  load all relation (21 as for V3) of Wordnet in required Module
%
%  @arg Module module context
%
load_all_relations(Module) :-
	expand_file_name(mkp([w3(°), 'wn_*.pl'], °), L),
	forall(member(X, L), (file_name_extension(N, pl, X), call(Module:consult, N))).

%%	make_qlf_full is det.
%
%	force updating of QLF files
%
make_qlf_full :-
	make_qlf_full(w3(°)).

%% make_qlf_full(+Folder) is det.
%
%  expand name of all Prolog files in Folder and qcompile each
%
%  @arg Folder describe Folder
%
make_qlf_full(Folder) :-
	maplist(qcompile, expand_file_name(pls(Folder, °), °)).

% from NodeBox: verb tenses
%
verb_tenses_columns(
['infinitive', 'present participle', 'past plural', '2nd singular present',
 '2nd singular past', 'past', '3rd singular present', 'past participle',
 '1st singular present', '1st singular past', '3rd singular past', 'present plural']).

verb_tenses_file('~/lang/NodeBox Linguistic/en/verb/verb.txt').
:- dynamic verb_tenses/12.

:- record verb(
	'inf',
	'1sgpres',
	'2sgpres',
	'3sgpres',
	'pl',
	'prog',
	'1sgpast',
	'2sgpast',
	'3sgpast',
	'pastpl',
	'ppart').

verb_tenses_aliases('inf',     'infinitive').
verb_tenses_aliases('1sgpres', '1st singular present').
verb_tenses_aliases('2sgpres', '2nd singular present').
verb_tenses_aliases('3sgpres', '3rd singular present').
verb_tenses_aliases('pl',      'present plural').
verb_tenses_aliases('prog',    'present participle').
verb_tenses_aliases('1sgpast', '1st singular past').
verb_tenses_aliases('2sgpast', '2nd singular past').
verb_tenses_aliases('3sgpast', '3rd singular past').
verb_tenses_aliases('pastpl',  'past plural').
verb_tenses_aliases('ppart',   'past participle').

verb_tenses_load :-
	open(verb_tenses_file(°), read, S),
	length(verb_tenses_columns(°), NCols),
	repeat,
	read_line_to_codes(S, Cs),
	(   Cs == end_of_file
	->  close(S)
	;   atom_codes(A, Cs),
	    atomic_list_concat(As, ',', A),
	    length(As, Las),
	    (	Las < NCols
	    ->  format('wrong ~d ~s~n', [Las, A])
	    ;	length(Fas, NCols),
		append(Fas, _, As),
		T =.. [verb_tenses|Fas],
		assertz(T)
	    ),
	    fail
	).

%%	isa_graph is det
%
%	make a graph of hyponyms *data*
%
isa_graph :-
	true.

schema(	[s(	synset_id,w_num,word,ss_type,sense_number,tag_count	)/'synset pointers'
	,sk(	synset_id,w_num,sense_key				)/'sense keys'
	,g(	synset_id,gloss						)/'gloss pointers'
	,syntax(	synset_id,w_num,syntax					)/'syntactic markers'
	,hyp(	synset_id,synset_id					)/'hypernym pointers'
	,ins(	synset_id,synset_id					)/'instance pointers'
	,ent(	synset_id,synset_id					)/'entailment pointers'
	,sim(	synset_id,synset_id					)/'similar pointers'
	,mm(	synset_id,synset_id					)/'member meronym pointers'
	,ms(	synset_id,synset_id					)/'substance meronym pointers'
	,mp(	synset_id,synset_id					)/'part meronym pointers'
	,der(	synset_id,w_num,synset_id,w_num				)/'derivational morphology pointers'
	,cls(	synset_id,w_num,synset_id,w_num,class_type		)/'class (domain) pointers'
	,cs(	synset_id,synset_id					)/'cause pointers'
	,vgp(	synset_id,w_num,synset_id,w_num				)/'grouped verb pointers'
	,at(	synset_id,synset_id					)/'attribute pointers'
	,ant(	synset_id,w_num,synset_id,w_num				)/'antonym pointers'
	,sa(	synset_id,w_num,synset_id,w_num				)/'see also pointers'
	,ppl(	synset_id,w_num,synset_id,w_num				)/'participle pointers'
	,per(	synset_id,w_num,synset_id,w_num				)/'pertainym pointers'
	,fr(	synset_id,f_num,w_num					)/'frame pointers'
	]).

wordnet_schema :-
	schema(Rs),
	maplist(spec(Rs), Rs, Ts),
	graph_schema(schema{name:wordnet3, tables:Ts}).

predicate_indicator(F/A) :-
	schema(S),
	member(R/_, S),
	functor(R, F, A).

spec(Rs, T/Desc, table{name:N, columns:Cs, primaryKey:[], foreignKeys:[], namedKeys:NKs}) :-
	T =.. [Funct|Args],
	(	Funct == s
	->	NKs = [] 
	;	Rs = [S/_|_],
		S =.. [s|As],
		findall(namedKey{key:F, targetTable:'s/6:synset pointers'}, (
			member(F, Args),
			memberchk(F, As)), NKs)
	),
	length(Args, Arity),
	format(atom(N), '~s/~d:~s', [Funct,Arity,Desc]),
	maplist(column,Args,Cs).

column(N,column{name:N, type:atom}).

graph_hyp(Word) :-
	graph_window(graph_hyp(Word), []).

graph_hyp(Word, G) :-
	forall(s(K,_,Word,S,_,_), (
		make_node(G, K, [label:Word/S, shape:box], P),
		chain_up(G, K-S, P),
		chain_down(G, K-S, P)
	)).

chain_up(G, K-S, P) :-
	(	hyp(K, U)
	->	s(U,_,Word,S,_,_),
		(	find_node(G, U, Q)
		->	edge_unique(G, Q, P, filled)
		;	make_node(G, U, [label:Word/S, shape:ellipse], Q),
			edge_unique(G, Q, P, filled),
			chain_up(G, U-S, Q)
		)
	;	true
	).

chain_down(G, K-S, P) :-
	forall((hyp(U, K), s(U,_,Word,S,_,_)),
	(
		(	find_node(G, U, Q)
		->	edge_unique(G, P, Q, dotted)
		;	make_node(G, U, [label:Word/S, shape:diamond], Q),
			edge_unique(G, P, Q, dotted),
			chain_down(G, U-S, Q)
		)
	)).

edge_unique(G, Source, Target, Style) :-
	find_edge(G, Source, Target, _) -> true ; new_edge(G, Source, Target, E), set_attrs(E, [style:Style]).

subsynset(Sub, Super) :-
	s(I,_,Sub,Kind,_,_),
	hyp(I, J),
	s(J,_,Hyp,Kind,_,_),
	( Super = Hyp ; subsynset(Hyp, Super) ).

%	s(I,_,Sub,Kind,_,_), ( hyp(I, J) -> s(J,_,Hyp,Kind,_,_), Hyp = Super ; subsynset(Hyp, Super) ).

subsynset(Sub, Super, Distance) :-
	s(I,_,Sub,Kind,_,_),
	s(J,_,Super,Kind,_,_),
	hyp_chain(I, J, Distance).

hyp_chain(I, J, Distance) :-
	I == J -> Distance = 0 ; hyp(I, H), hyp_chain(H, J, Distance1), Distance is Distance1 + 1.

subsynset_path(Sub, Super, Path) :-
	s(I,_,Sub,Kind,_,_),
	s(J,_,Super,Kind,_,_),
	hyp_chain_path(I, J, Path).

subsynset_topdown(Sub, Super, Path) :-
	s(I,_,Sub,Kind,_,_),
	s(J,_,Super,Kind,_,_),
	hyp_chain_topdown(I, J, RPath), reverse(RPath, Path).

hyp_chain_topdown(I, I, [I]) :- !.
hyp_chain_topdown(I, J, [J|Path]) :-
	hyp(H, J), 
	hyp_chain_topdown(I, H, Path).

hyp_chain_path(I, I, [I]) :- !.
hyp_chain_path(I, J, [I|Path]) :-
	hyp(I, H), hyp_chain_path(H, J, Path).

%hyp_chain_words(Chain, Words) :- maplist(hyp_chain_word, Chain, Words).
hyp_chain_words([], []).
hyp_chain_words([C|Chain], [W|Words]) :- 
	hyp_chain_word(C,W),
	hyp_chain_words(Chain, Words).

hyp_chain_word(SynsetId, Word) :-
	s(SynsetId, _, Word,_,_,_).

root_hyp(H) :- hyp(H, _), \+ hyp(_, H).
leaf_hyp(L) :- hyp(_, L), \+ hyp(L, _).

count_synset_daughters(S, N) :-
	s(S,_,_,_,_,_),
	setof(D, hyp(S,D), L),
	length(L, N).
