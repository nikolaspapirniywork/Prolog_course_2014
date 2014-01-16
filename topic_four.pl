:- op(100, xfx, name_is).
:- op(100, xf, is_cool).
:- op(100, fx, favorite_color_is).
:- op(100, xfx, was_born_in).
:- op(100, yfx, associates_with).


color_associations(black, bad).
color_associations(white, good).

my name_is nick.
nick is_cool.
favorite_color_is white.
favorite_color_is black associates_with bad.
i was_born_in lugansk.



% ------------------------------------------------------------------------------

%% :- begin_tests(topic_four).
%% :- use_module(library(lists)).

%% :- end_tests(topic_four).

stag_raboti(abc, abc1).


stag_raboti(X, R) :- 
	stag_raboti(X, R), X > 10.