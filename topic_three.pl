contains(X, [X|_]).
contains(X, [_|Tail]) :-
	contains(X, Tail).

my_concat([], List, List).
my_concat([X|L1], L2, [X|L3]) :-
	my_concat(L1, L2, L3).

add(X, L, [X|L]).

remove(X, [X|L], L).
remove(X, [_|L], L1) :-
	remove(X, L, L1).

sequence_of_natural_numbers(0, []).
sequence_of_natural_numbers(N, R) :-
	N1 is N - 1,
	sequence_of_natural_numbers(N1, R1),
	append(R1, [N], R).

sequence_of_natural_numbers_two(N, R) :- 
	sequence_of_natural_numbers_two(N, 1, R).
sequence_of_natural_numbers_two(0, _, []).
sequence_of_natural_numbers_two(N, I, [I|R1]) :-
	I =< N,
	I1 is I + 1,
	sequence_of_natural_numbers_two(N, I1, R1).	
sequence_of_natural_numbers_two(_, _, []).

odd_sequence_condition(N) :-
	is_not_divisible(N, 2).

even_sequence_condition(N) :-
	is_divisible(N, 2).
	
is_divisible(N, By) :-
	Divisible is mod(N, By),
	Divisible = 0.

is_not_divisible(N, By) :-
	Divisible is mod(N, By),	
	\+(Divisible = 0). 

sequense_of_even_natural_numbers(N, R) :-
	sequence_with_condition(N, 1, even_sequence_condition, R).

sequense_of_odd_natural_numbers(N, R) :-
	sequence_with_condition(N, 1, odd_sequence_condition, R).

sequence_with_condition(0, []).
sequence_with_condition(N, Condition, R) :-
	sequence_with_condition(N, 1, Condition, R).

sequence_with_condition(0, _, _, []).
sequence_with_condition(N, I, Condition, [I|R1]) :-
	Goal =.. [Condition, I], 
	call(Goal),
	I1 is I + 1,
	N1 is N - 1,
	sequence_with_condition(N1, I1, Condition, R1).
sequence_with_condition(N, I, Condition, R) :-
	I1 is I + 1,
	sequence_with_condition(N, I1, Condition, R).

filter([], _, []).
filter([X|Xs], Condition, [X|R1]) :-
	Goal =.. [Condition, X],
	call(Goal),
	filter(Xs, Condition, R1).
filter([_|Xs], Condition, R1) :-
	filter(Xs, Condition, R1).

negative_numbers_condition(N) :-
	N > 0.
	
filter_negative_numbers(List, R) :-
	filter(List, negative_numbers_condition, R).

add_element_after_every_third(El, List, R) :-
	add_element_after_every_nth(El, 1, List, 3, R).

add_element_after_every_nth(_, _, [], _, []).
add_element_after_every_nth(El, I, [X|Xs], Every, [X|[El|R1]]) :-
	I_next is I + 1,
	is_divisible(I, 3),
	add_element_after_every_nth(El, I_next, Xs, Every, R1).
add_element_after_every_nth(El, I, [X|Xs], Every, [X|R1]) :-
	I_next is I + 1,
	add_element_after_every_nth(El, I_next, Xs, Every, R1).

list_size([], 0).
list_size([_|Xs], R) :-
	list_size(Xs, R1),
	R is R1 + 1.

reverse_list([], []).
reverse_list([X|Xs], R) :-
	reverse_list(Xs, R1),
	append(R1, [X], R).

remove_all_elements([], _, []).
remove_all_elements([X|Xs], X, R) :-
	remove_all_elements(Xs, X, R).
remove_all_elements([X|Xs], El, [X|R1]) :-
	remove_all_elements(Xs, El, R1).

remove_first([], _, []).
remove_first([X|Xs], X, Xs).
remove_first([X|Xs], El, [X|R]) :-		
	remove_first(Xs, El, R).

remove_every_second([], []).
remove_every_second([X], [X]).
remove_every_second([X1,X2|Xs], [X1|R1]) :-
	remove_every_second(Xs, R1).

sum_of_elements([], 0).
sum_of_elements([X|Xs], R) :-	
	sum_of_elements(Xs, R1),
	R is X + R1.

	


% ------------------------------------------------------------------------------

:- begin_tests(topic_three).
:- use_module(library(lists)).
	test(sequence_of_natural_numbers_1) :-
		sequence_of_natural_numbers(3, [1,2,3]).
	test(sequence_of_natural_numbers_2) :-
		sequence_of_natural_numbers(0, []).
	test(sequence_of_natural_numbers_3) :-
		sequence_of_natural_numbers(1, [1]).

	test(sequence_of_natural_numbers_2_1) :-
		sequence_of_natural_numbers_two(3, [1,2,3]).
	test(sequence_of_natural_numbers_2_2) :-
		sequence_of_natural_numbers_two(0, []).
	test(sequence_of_natural_numbers_3_2) :-
		sequence_of_natural_numbers_two(1, [1]).
	
	test(sequense_of_even_natural_numbers_1) :-
		sequense_of_even_natural_numbers(1, [2]).
	test(sequense_of_even_natural_numbers_2) :-
		sequense_of_even_natural_numbers(0, []).
	test(sequense_of_even_natural_numbers_3) :-
		sequense_of_even_natural_numbers(2, [2,4]).

	test(sequense_of_odd_natural_numbers_1) :-
		sequense_of_odd_natural_numbers(1, [1]).
	test(sequense_of_odd_natural_numbers_2) :-
		sequense_of_odd_natural_numbers(0, []).
	test(sequense_of_odd_natural_numbers_3) :-
		sequense_of_odd_natural_numbers(2, [1,3]).

	test(filter_negative_numbers_1) :-
		filter_negative_numbers([1,2,3], [1,2,3]).
	test(filter_negative_numbers_2) :-
		filter_negative_numbers([1,-2,3,-4], [1,3]).
	test(filter_negative_numbers_3) :-
		filter_negative_numbers([], []).

	test(add_elemnt_after_specific_element_1) :-
		add_element_after_every_third('a', [a,b,c,d,e,f], [a,b,c,a,d,e,f,a]).
	test(add_elemnt_after_specific_element_2) :-
		add_element_after_every_third('a', [], []).
	test(add_elemnt_after_specific_element_3) :-
		add_element_after_every_third('a', [a,b], [a,b]).

	test(list_size_1) :-
		list_size([1,2,3], 3).
	test(list_size_2) :-
		list_size([], 0).
	test(list_size_3) :-
		list_size([1], 1).

	test(reverse_list_1) :-
		reverse_list([1,2,3], [3,2,1]).
	test(reverse_list_2) :-
		reverse_list([], []).
	test(reverse_list_3) :-
		reverse_list([0], [0]).

	test(remove_all_elements_1) :-
		remove_all_elements([1,2,3], 2, [1,3]).
	test(remove_all_elements_2) :-
		remove_all_elements([], 2, []).
	test(remove_all_elements_3) :-
		remove_all_elements([1,2,3], 4, [1,2,3]).
	test(remove_all_elements_4) :-
		remove_all_elements([1,2,3,2], 2, [1,3]).

	test(remove_first_1) :-	
		remove_first([1,2,3], 2, [1,3]).
	test(remove_first_2) :-	
		remove_first([1,2,3,2], 2, [1,3,2]).
	test(remove_first_3) :-	
		remove_first([], 2, []).

	test(remove_every_second_1) :-
		remove_every_second([1,2,3,4], [1,3]).
	test(remove_every_second_2) :-
		remove_every_second([1], [1]).
	test(remove_every_second_3) :-
		remove_every_second([], []).

	test(sum_of_elements_1) :-
		sum_of_elements([1,2,3,4,5], 15).
	test(sum_of_elements_2) :-
		sum_of_elements([], 0).
	test(sum_of_elements_3) :-
		sum_of_elements([1], 1).




:- end_tests(topic_three).