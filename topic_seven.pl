derivative(X, X, 1).
derivative(C, X, 0) :- 
	not(has_function(C, X)).
derivative(F+G, X, DF+DG) :-
	derivative(F, X, DF),
	derivative(G, X, DG).
derivative(F-G, X, DF-DG) :-
	derivative(F, X, DF),
	derivative(G, X, DG).
derivative(F*G, X, DF*G+F*DG) :-
	derivative(F, X, DF),
	derivative(G, X, DG).
derivative(F/G, X, (DF*G-F*DG)/G^2) :-
	derivative(F, X, DF),
	derivative(G, X, DG).
derivative(X^N, X, N*X^(N-1)) :-
	integer(N).
derivative(A^X, X, A^X*ln(A)) :-
	integer(A).
derivative(e^X, X, e^X).
derivative(sin(X), X, cos(X)).
derivative(cos(X), X, -sin(X)).
derivative(ln(X), X, X^(-1)).
derivative(log(X, A), X, 1/(X*ln(A))).
derivative(sec(X), X, sin(X)/cos(X)^2).
derivative(cosec(X), X, -cos(X)/sin(X)^2).	

derivative(thx(X), X, 1/ch(X)^2).
derivative(arctg(X), X, 1/1+X^2).
derivative(arcch(X), X, 1/sqrt(X^2-1)).
derivative(cth(X), X, -1/sh(X)^2).
derivative(arcctg(X), X, 1/(1-X^2)).

derivative(GH, X, G_h*H_x) :-
	GH =..[F|Args],
	choose_arg(F, Args, X, Arg),
	derivativeGH(F, GH, Arg, G_h),
	functor(Arg, F1, _),
	derivativeGH(F1, Arg, X, H_x), !.

choose_arg(_, [Arg], _, Arg).
choose_arg(^, [e, Arg2], _, Arg2) :- !.
choose_arg(^, [Arg1, _], X, Arg1) :-
	has_function(Arg1, X), !.
choose_arg(^, [_, Arg2], _, Arg2) :- !.
choose_arg(log, [Arg1, _], _, Arg1) :- !.
choose_arg(sec, [Arg], Arg).
choose_arg(thx, [Arg], Arg).
choose_arg(arctg, [Arg], Arg).
choose_arg(arcch, [Arg], Arg).
choose_arg(cth, [Arg], Arg).
choose_arg(arcctg, [Arg], Arg).


derivativeGH(F, GH, Arg, G_h) :-
	in_derivative_table(F), !,
	derivative(GH, Arg, G_h).

derivativeGH(_, GH, Arg, G_h) :-
	term_to_atom(GH, GH_Atom),
	term_to_atom(Arg, ArgAtom),
	concat_atom([derivative, from, GH_Atom, on, ArgAtom], '_', G_h), !.

in_derivative_table(F) :-
	member(F, [sin, con, ^, +, -, *, /, log, sec, cosec, thx, arctg, arcch, cth, arcctg]).

has_function(X, X) :- !.
has_function(F, X) :-
	F=..[_|Args],
	member(F1, Args),
	has_function(F1, X), !.

taylorFunction(F,X,A,N,First_result+R):-
	calculate_function(F,X,A,First_result),
	taylorFunctionN(F,X,A,N,1,R),!.

calculate_function(F,X,A,R):-
	F=..T,
	replace_contains(T,[],X,A,R1),
	R=..R1.

replace_contains([], L, X, A, L).
replace_contains([Y|Ys], L, X, A, R):-
	replace_contains(Ys,L,X,A,R2),
	in_table1(Y,X,A,R1),
	append([R1], R2, R).

in_table1(F,X,A,R):-
	member(F,
		[ln,arcsech,arccos,ctg,ch,sin,cos,^,+,-,*,/]
		),
	term_to_atom(F,R).
in_table1(F,X,A,R):-
	integer(F),
	R is F, !.
in_table1(F,X,A,R):-
	F==X,
	R is A, !.
in_table1(F,X,A,R):-
	F=..L,replace_contains(L,[],X,A,R1),
	R=..R1, !.

derivativeN(R,_,0,R).
derivativeN(F,X,N,R):-
	N1 is N-1,
	derivative(F,X,R1),
	derivativeN(R1,X,N1,R).

taylorFunctionN(F,_,_,0,_,'0').
taylorFunctionN(F, X, A, N, N2, F2*O+R):-
	derivativeN(F, X, N, F1),
	calculate_function(F1,X,A,F2),
	factorial(N2,N3),
	term_to_atom((X-A)^N2,GH_Atom),
	term_to_atom(1/N3,GH_Atom1),
	concat_atom([GH_Atom,GH_Atom1],*,O),
	N4 is N2 + 1,
	N1 is N - 1,
	taylorFunctionN(F1, X, A, N1, N4, R).


factorial(0, 1).
factorial(0, 1).	
factorial(N, R) :-
	N1 is N - 1,
	factorial(N1, R1),
	R is N * R1.



:- begin_tests(topic_seven).


:- end_tests(topic_seven).