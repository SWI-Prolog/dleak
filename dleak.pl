:- module(dleak,
	  [ dleak/1			% +File
	  ]).

:- use_module(library(pairs)).
:- use_module(library(apply)).
:- use_module(library(lists)).

dleak(File) :-
	cleanup,
	setup_call_cleanup(
	    open(File, read, In),
	    process(In),
	    close(In)),
	report.

process(In) :-
	read(In, Term),
	process(Term, In).

process(end_of_file, _) :- !.
process(Term, In) :-
	action(Term),
	read(In, Term2),
	process(Term2, In).

:- dynamic
	cc/2,				% calling context
	chunk/3,
	location/3.			% Location caching

cleanup :-
	retractall(cc(_,_)),
	retractall(chunk(_,_,_)),
	retractall(location(_,_,_)).

action(cc(Id, Stack)) :-
	assertz(cc(Id, Stack)).
action(malloc(Ctx,Size,Ptr)) :-
	assertz(chunk(Ptr,Size,Ctx)).
action(calloc(Ctx,N,Len,Ptr)) :-
	Size is N*Len,
	assertz(chunk(Ptr,Size,Ctx)).
action(realloc(Ctx,Ptr,Size,NPtr)) :-
	(   Ptr == nil
	->  true
	;   retract(chunk(Ptr,_,_))
	->  true
	;   print_message(error, realloc(Ctx,Ptr,Size))
	),
	assertz(chunk(NPtr,Size,Ctx)).
action(free(Ctx,Ptr)) :-
	(   Ptr == nil
	->  true
	;   retract(chunk(Ptr,_,_))
	->  true
	;   print_message(error, free(Ctx,Ptr))
	).

%%	report
%
%	Report not freed memory with its call stack

report :-
	findall(Ctx-mem(Ptr,Size), chunk(Ptr,Size,Ctx), NotFree),
	keysort(NotFree, Sorted),
	group_pairs_by_key(Sorted, Grouped),
	maplist(sum_not_freed, Grouped, Summed),
	sort(Summed, ByLeak),
	maplist(not_freed, ByLeak).

sum_not_freed(Ctx-Mems, not_freed(Bytes,Count,Ctx)) :-
	length(Mems, Count),
	maplist(arg(2), Mems, Sizes),
	sum_list(Sizes, Bytes).

not_freed(not_freed(Bytes,Count,Ctx)) :-
	print_message(warning, not_freed(Ctx, Count, Bytes)).


:- multifile prolog:message//1.

prolog:message(not_freed(Ctx, Count, Bytes)) -->
	{ cc(Ctx, Stack) },
	[ '~d bytes not freed in ~D allocations at (ctx=~d)'-
	  [Bytes,Count, Ctx], nl],
	context(Stack).

context(Stack) -->
	{ maplist(addr2line, Stack, Human)
	},
	stack(Human).

stack([]) --> [].
stack([H|T]) -->
	[ '    ~p'-[H], nl ],
	stack(T).

addr2line(SO+Offset, Human) :-
	location(SO, Offset, Human), !.
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
