:- module(dlevent,
	  [ dlevent/1			% +Term
	  ]).

:- use_foreign_library(dlevent).

dlevent(Term) :-
	format(string(Evt), '~q', [Term]),
	dlevent_(Evt).
