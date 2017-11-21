/** <module> wn
 *
 *  Incapsulate wordnet files, with precompilation option for faster consulting.
 *	
 *  Issue ?- load_wn3_relations. to load all relation in memory.
 *  It's slow the first time, when it's compiling, then it's really fast.
 */

:- module(wn3_db,
	[s/6
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
	,load_wn3_relation/1	% load named relations to memory, qcompile if needed
	,load_wn3_relations/0	% load all relations to memory
	,path_wn3_files/1	% storage location of all DB data files
	,path_wn3_file/2		% storage location of a DB data file
	,schema_wn3_table/2	% summary description of a DB table
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

:- use_module(library(debug)).

%% load_wn3_relations is det.
%
%  load all relations to memory
%  handle transparent compiling to QLF for fast upload
%
load_wn3_relations :-
	debug_(load_wn3_relations),
	forall(schema_wn3_table(Id, _), load_wn3_relation(Id)).

%% load_wn3_relation(+Id) is det.
%
%  load relation to memory
%  handle transparent compiling to QLF for fast upload
%
%  @arg Id wordnet 3 relation id
%
load_wn3_relation(Id) :-
	path_wn3_file(Id, Base),
	file_name_extension(Base, qlf, QLF),
	file_name_extension(Base, pl, PL),
	( exists_file(QLF) -> consult(QLF) ; feedback(compiling(Id -> QLF)), qcompile(PL) ).

%% path_wn3_files(-Path) is det.
%
%  get the directory from where data files are read
%	now just get info from *wn3* key
%	for instance: put in .plrc
%		user:file_search_path(wn3, '~/prolog/WN').
%	and restart
%
%  @arg Path directory path
%
path_wn3_files(Path) :-
	debug_(path_wn3_files(Path)),
	user:file_search_path(wn3, Path).

%% path_wn3_file(+Id, -Path) is det.
%
%  storage location of required DB data files
%
%  @arg Id Wordnet table identifier
%  @arg Path loadable file name
%	note: file extension will change accordingly to availability
%	of fast load precompiled files
%
path_wn3_file(Id, Path) :-
	debug_(path_wn3_file(Id, Path)),
	path_wn3_files(PathFiles),
	format(atom(Path), '~w/wn_~s', [PathFiles, Id]).

%% schema_wn3_table(?Id, -Schema) is det.
%
%  schema_wn3_table get the schema dict for required table identifier
%
%  @arg Id relation identifier
%  @arg Schema dict structure with table,columns,brief,...
%
schema_wn3_table(Id, Schema) :-
	debug_(schema_wn3_table(Id, Schema)),
	schemas(Ss),
	member(S/Brief, Ss),
	S =.. [Id|Cols],
	Schema = schema_wn3_table{table:Id, columns:Cols, brief:Brief}.

%%%%%%%% private interface from here

schemas(	[s(	synset_id,w_num,word,ss_type,sense_number,tag_count	)/'synset pointers'
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

%% debug_(Info) is det.
%
%  shortcut for common logging requirements
%
%  @arg Info generic structure to display
%
debug_(Info) :-
	debug(wn3_db, '~w', [Info]).

%%%%%%%%%%%%%%%
% feedback

feedback(M) :- print_message(informational, M).

:- multifile
        prolog:message//1.

prolog:message(compiling(Table)) -->
        [ 'Compiling ~w'-[Table] ].

%%%%%%%%%%%%%%%%%%%%
%% PlUnit interface

:- begin_tests(wn3_db).

test(1) :-
	schema_wn3_table(s, S),
	S.table == s,
	length(S.columns, 6).

:- end_tests(wn3_db).
