:- module(dleak,
	  [ dleak/2			% +File, +Options
	  ]).
:- use_module(library(option)).
:- use_module(library(pairs)).
:- use_module(library(apply)).
:- use_module(library(lists)).

dleak(File, Options) :-
	cleanup,
	size_file(File, Size),
	setup_call_cleanup(
	    open(File, read, In),
	    process_log(In, Size, State, Options),
	    close(In)),
	report(Options),
	dump_state(State, _, 0).

process_log(In, Size, State, Options) :-
	read(In, Term),
	option(follow(Follow), Options, 10),
	State0 = dleak{ events:0,
			contexts:0,
			malloc:0,
			calloc:0,
			realloc:0,
			free:0,
			total:0,
			context_info:context{},
			wtime:0,
			follow:Follow,
			file_size:Size
		      },
	options_state(Options, State1),
	put_dict(State1, State0, State),
	process(Term, In, State).

options_state([], []).
options_state([dump(N)|T0], [dump-N|T]) :- !,
	options_state(T0, T).
options_state([_|T0], T) :-
	options_state(T0, T).

wtime(MS) :-
	get_time(T),
	MS is round(T*1000.0).

wtime(State, Time) :-
	wtime(Wtime),
	Time is Wtime - State.wtime,
	nb_set_dict(wtime, State, Wtime).


process(end_of_file, _, _) :- !.
process(Term, In, State) :-
	action(Term, State),
	inc(events, State, 1),
	(   Dump = State.get(dump),
	    State.events mod Dump =:= 0
	->  wtime(State, Time),
	    dump_state(State, In, Time)
	;   true
	),
	read(In, Term2),
	process(Term2, In, State).

dump_state(State, _, 0) :-
	format('Events: ~D, memory: ~D, freed: ~D~n',
	       [State.events, State.total, State.free]),
	dump_top_contexts(State).
dump_state(State, In, Time) :-
	file_percentage(State, In, Perc),
	format('Events: ~D, memory: ~D, freed: ~D (~D msec, ~1f%)~n',
	       [State.events, State.total, State.free, Time, Perc]),
	dump_top_contexts(State).

file_percentage(State, In, Perc) :-
	byte_count(In, Here),
	Perc is ((Here*1000)//State.file_size)/10.

dump_top_contexts(State) :-
	Count = State.get(follow),
	Count > 0, !,
	dict_pairs(State.context_info, _, Pairs),
	sort(2, >=, Pairs, Decreasing),
	top(Decreasing, Count, Top),
	maplist(list_context, Top),
	nl.
dump_top_contexts(_).

list_context(Ctx-Mem) :-
	format(' ~d: ~D; ', [Ctx, Mem]).

top([], _, []).
top(_, 0, []) :- !.
top([H|T0], N, [H|T]) :-
	N2 is N - 1,
	top(T0, N2, T).

:- dynamic
	cc/2,				% calling context
	chunk/3,
	location/3.			% Location caching

cleanup :-
	retractall(cc(_,_)),
	retractall(chunk(_,_,_)),
	retractall(location(_,_,_)).

inc(Field, State, Extra) :-
	New is State.Field+Extra,
	nb_set_dict(Field, State, New).

inc_usage(State, Ctx, Extra) :-
	get_dict(context_info, State, Info),
	(   get_dict(Ctx, Info, Old)
	->  New is Old + Extra,
	    nb_set_dict(Ctx, Info, New)
	;   put_dict(Ctx, Info, Extra, NewInfo),
	    nb_set_dict(context_info, State, NewInfo)
	).

action(cc(Id, Stack), State) :-
	inc(contexts, State, 1),
	assertz(cc(Id, Stack)).
action(malloc(Ctx,Size,Ptr), State) :-
	inc(malloc, State, 1),
	inc(total, State, Size),
	inc_usage(State, Ctx, Size),
	asserta(chunk(Ptr,Size,Ctx)).
action(calloc(Ctx,N,Len,Ptr), State) :-
	Size is N*Len,
	inc(calloc, State, 1),
	inc(total, State, Size),
	inc_usage(State, Ctx, Size),
	asserta(chunk(Ptr,Size,Ctx)).
action(realloc(Ctx,Ptr,Size,NPtr), State) :-
	inc(realloc, State, 1),
	(   Ptr == nil
	->  inc(total, State, Size),
	    inc_usage(State, Ctx, Size),
	    asserta(chunk(NPtr,Size,Ctx))
	;   retract(chunk(Ptr,OSize,OrigCtx))
	->  Added is Size-OSize,
	    inc(total, State, Added),
	    inc_usage(State, OrigCtx, Added),
	    asserta(chunk(NPtr,Size,Ctx))
	;   print_message(error, realloc(Ctx,Ptr,Size,NPtr)),
	    abort
	).
action(free(Ctx,Ptr), State) :-
	inc(free, State, 1),
	(   Ptr == nil
	->  true
	;   retract(chunk(Ptr,OSize,AllocCtx))
	->  Freed is -OSize,
	    inc_usage(State, AllocCtx, Freed),
	    inc(total, State, Freed)
	;   print_message(error, free(Ctx,Ptr))
	).

%%	report(+Options)
%
%	Report not freed memory with its call stack

report(Options) :-
	findall(Ctx-mem(Ptr,Size), chunk(Ptr,Size,Ctx), NotFree),
	keysort(NotFree, Sorted),
	group_pairs_by_key(Sorted, Grouped),
	maplist(sum_not_freed, Grouped, Summed),
	sort(Summed, ByLeak),
	(   option(top(N), Options, 100),
	    N \== all
	->  bottom(ByLeak, N, List)
	;   List = ByLeak
	),
	maplist(not_freed, List).

sum_not_freed(Ctx-Mems, not_freed(Bytes,Count,Ctx)) :-
	length(Mems, Count),
	maplist(arg(2), Mems, Sizes),
	sum_list(Sizes, Bytes).

not_freed(not_freed(Bytes,Count,Ctx)) :-
	print_message(warning, not_freed(Ctx, Count, Bytes)).

bottom(List, N, Bottom) :-
	length(List, Len),
	(   Len > N
	->  Skip is Len - N,
	    skip(Skip, List, Bottom)
	;   Bottom = List
	).

skip(N, [_|T0], T) :-
	N > 0, !,
	N1 is N - 1,
	skip(N1, T0, T).
skip(_, List, List).


:- multifile prolog:message//1.

prolog:message(not_freed(Ctx, Count, Bytes)) -->
	{ cc(Ctx, Stack) },
	[ '~d bytes not freed in ~D allocations at (ctx=~d)'-
	  [Bytes,Count, Ctx], nl],
	context(Stack).
prolog:message(realloc(Ctx,Ptr,_Size,_NPtr)) -->
	{ cc(Ctx, Stack) },
	[ 'realloc() of unknown pointer 0x~16r'-[Ptr], nl ],
	context(Stack).
prolog:message(free(Ctx,Ptr)) -->
	{ cc(Ctx, Stack) },
	[ 'free() of unknown pointer 0x~16r'-[Ptr], nl ],
	context(Stack).
prolog:message(cc(Ctx)) -->
	{ cc(Ctx, Stack) },
	[ 'Calling context:'-[], nl ],
	context(Stack).

context(Stack) -->
	{ maplist(addr2line, Stack, Human)
	},
	stack(Human).

stack([]) --> [].
stack([H|T]) -->
	[ '    ~w'-[H], nl ],
	stack(T).

addr2line(SO+Offset, Human) :-
	location(SO, Offset, Human), !.
addr2line(SO+nil, SO) :- !.
addr2line(SO+Offset, Human) :-
	format(string(Cmd), 'addr2line -fe "~w" 0x~16r', [SO, Offset]),
	setup_call_cleanup(
	    open(pipe(Cmd), read, In),
	    read_string(In, _, Reply),
	    close(In)),
	split_string(Reply, "\n", "", [Func,Location|_]), !,
	format(atom(Human), '~w() at ~w', [Func, Location]),
	asserta(location(SO, Offset, Human)).
addr2line(Spec, Spec).
