tail_recursive_reverse(List, R) :-
	tail_recursive_reverse(List, [], R).
tail_recursive_reverse([], Acc, Acc).
tail_recursive_reverse([X|Xs], Acc, R) :-
	tail_recursive_reverse(Xs, [X|Acc], R).

% ------------------------------------------------------------------------------

:- begin_tests(topic_five).
:- use_module(library(lists)).
	test(tail_recursive_reverse_1) :-
		tail_recursive_reverse([1,2,3,4], [4,3,2,1]).
	test(tail_recursive_reverse_2) :-
		tail_recursive_reverse([1], [1]).
	test(tail_recursive_reverse_3) :-
		tail_recursive_reverse([], []).
:- end_tests(topic_five).
