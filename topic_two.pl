factorial(0, 1).
factorial(0, 1).	
factorial(N, R) :-
	N1 is N - 1,
	factorial(N1, R1),
	R is N * R1.

sum(0, 0).
sum(N, R) :- 
	N1 is N - 1,
	sum(N1, R1),
	R is N + R1. 

avg(0, 0).
avg(N, R) :-
	sum(N, Sum),
	R is Sum / N.

is_divisible(N, By) :-
	Divisible is mod(N, By),
	Divisible = 0.

sum_of_even(N, R) :-
	sum_of_elements_devided_by(N, 2, R).

sum_of_elements_divided_by_four(N, R) :-
	sum_of_elements_devided_by(N, 4, R).

sum_of_elements_devided_by(0, _, 0).
sum_of_elements_devided_by(N, Divided_by, R) :-
	is_divisible(N, Divided_by),
	N1 is N - 1,
	sum_of_elements_devided_by(N1, Divided_by, R1),
	R is R1 + N.
sum_of_elements_devided_by(N, Divided_by, R) :-
	N1 is N - 1,
	sum_of_elements_devided_by(N1, Divided_by, R1),
	R is R1.

sum_of_digits(N, R) :-
	N >= 10,
	Last_digit is mod(N, 10),
	Except_last_digit is truncate(N / 10),
	sum_of_digits(Except_last_digit, R1),
	R is Last_digit + R1.
sum_of_digits(N, R) :-
	N > 0,
	R is N.

map(1, _, 1).
map(N, Procedure, R) :-
	N1 is N - 1,
	map(N1, Procedure, R1),
	Goal =.. [Procedure, N, R1, R], 
	call(Goal).

factorial_sequence_function(N, Acc, Result) :-
	factorial(N, N_factorial),
	Sequence_element is 1 / N_factorial,
	Result is Sequence_element + Acc.


sum_of_factorial_sequence(N, R) :-
	map(N, factorial_sequence_function, R).

harmonic_sequence_function(N, Acc, Result) :-
	Sequence_element is 1 / N,
	Result is Sequence_element + Acc.

sum_of_harmonic_sequence(N, R) :-
	map(N, harmonic_sequence_function, R).

is_power_of_two(2).
is_power_of_two(N) :-
	is_divisible(N, 2),	
	N1 is N / 2,
	is_power_of_two(N1).

multiply_with_sum(0, _, 0).
multiply_with_sum(_, 0, 0).
multiply_with_sum(N, 1, N).
multiply_with_sum(1, M, M).
multiply_with_sum(N, M, R) :-
	N1 is N - 1,
	multiply_with_sum(N1, M, R1),
	R is R1 + M.

sum_of_two_digit_numbers(N, R) :-
	N < 99,
	sum(N, R).
sum_of_two_digit_numbers(_, R) :-
	sum(99, R).

pow_with_multiply(0, _, 0).
pow_with_multiply(_, 0, 1).
pow_with_multiply(N, Power, R) :-
	Less_power is Power - 1,
	pow_with_multiply(N, Less_power, R1),
	R is R1 * N.

first_variant_task_three(1, 40).
first_variant_task_three(2, 60).
first_variant_task_three(N, R) :-
	Last_element is N - 1,
	sum_of_first_variant_task_three(Last_element, Sum),
	R is Sum / Last_element.

sum_of_first_variant_task_three(0, 0).
sum_of_first_variant_task_three(N, R) :-	
	N1 is N - 1,
	first_variant_task_three(N, R1),
	sum_of_first_variant_task_three(N1, R2),
	R is R1 + R2. 

second_variant_task_three(1, 40).
second_variant_task_three(2, 60).
second_variant_task_three(N, R) :-
	Last_element is N - 1,
	mul_of_second_variant_task_three(Last_element, Mul),
	Sqrt_power is 1 / Last_element,
	pow(Mul, Sqrt_power, R).
	
mul_of_second_variant_task_three(0, 1).
mul_of_second_variant_task_three(N, R) :-	
	N1 is N - 1,
	second_variant_task_three(N, R1),
	mul_of_second_variant_task_three(N1, R2),
	R is R1 * R2. 

% ------------------------------------------------------------------------
:- begin_tests(topic_two).
:- use_module(library(lists)).

test(factorial) :-
	factorial(5, 120).

test(sum) :-
	sum(5, 15).
test(sum_of_even) :-
	sum_of_even(10, 30).

test(sum_of_elements_divided_by_four) :-
	sum_of_elements_divided_by_four(10, 12).

test(sum_of_digits) :-
	sum_of_digits(12345, 15).
test(sum_of_digits_2) :-
	sum_of_digits(1, 1).	

test(sum_of_sequence) :-
	sum_of_factorial_sequence(4, R),
	R_vicinity is R - 1.708,
	R_vicinity < 0.1.

test(sum_of_harmonic_sequence) :-
	sum_of_harmonic_sequence(4, R),
	R_vicinity is R - 2.08,
	R_vicinity < 0.1.

test(is_power_of_two) :-
	is_power_of_two(2).
test(is_power_of_two) :-
	is_power_of_two(64).
test(not_power_of_two, [fail]) :-
	is_power_of_two(1).
test(not_power_of_two_2, [fail]) :-
	is_power_of_two(13).

test(multiply_with_sum) :-
	multiply_with_sum(4, 5, 20).	
test(multiply_with_sum_2) :-
	multiply_with_sum(4, 0, 0).
test(multiply_with_sum_3) :-
	multiply_with_sum(0, 4, 0).
test(multiply_with_sum_4) :-
	multiply_with_sum(1, 4, 4).
test(multiply_with_sum_5) :-
	multiply_with_sum(4, 1, 4).
test(multiply_with_sum_6) :-
	multiply_with_sum(4, -4, -16).

test(avg) :-
	avg(5, 3).	
test(avg_from_zero) :-
	avg(0, 0).

test(sum_of_two_digit_numbers) :-
	sum_of_two_digit_numbers(5, 15).
test(pow_with_multiply_1) :-
	pow_with_multiply(2, 4, 16).
test(pow_with_multiply_2) :-
	pow_with_multiply(2, 0, 1).
test(pow_with_multiply_3) :-
	pow_with_multiply(0, 5, 0).

test(first_variant_task_three_1) :-
	first_variant_task_three(3, 50).
test(second_variant_task_three) :-
	second_variant_task_three(3, R),
	R_vicinity is R - 48.98,
	R_vicinity < 0.1.

:- end_tests(topic_two).