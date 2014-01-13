male('Nickolay Jr').
male('Nickolay Andreevich').
male('Nickolay Iakovlevich').
male('Vadim').
male('Nickolay Sr').
male('Dmitriy').
male('Dever Example').
male('Viktoria_future_son').

female('Svetlana').
female('Valentina').
female('Nina').
female('Natalia').
female('Viktoria').
female('Viktoria_future_daughter').

marriage('Nickolay Andreevich', 'Valentina').
marriage('Nickolay Iakovlevich', 'Nina').
marriage('Nickolay Sr', 'Svetlana').
marriage('Vadim', 'Natalia').

is_married(X, Y) :-
	marriage(X, Y).
is_married(X, Y) :-
	marriage(Y, X).

parent('Nickolay Sr', 'Nickolay Jr').
parent('Nickolay Sr', 'Dmitriy').
parent('Svetlana', 'Dmitriy').
parent('Svetlana', 'Nickolay Jr').
parent('Viktoria', 'Viktoria_future_daughter').
parent('Viktoria', 'Viktoria_future_son').

parent('Valentina', 'Svetlana').
parent('Nickolay Andreevich', 'Svetlana').

parent('Nickolay Iakovlevich', 'Nickolay Sr').
parent('Nina', 'Nickolay Sr').
parent('Nickolay Iakovlevich', 'Dever Example').
parent('Nina', 'Dever Example').

parent('Nickolay Iakovlevich', 'Natalia').
parent('Nina', 'Natalia').

parent('Natalia', 'Viktoria').
parent('Vadim', 'Viktoria').

child(X, Y) :- parent(Y, X).

% >>> Topic 1%
husband(X, Y) :-
	is_married(X, Y),
	male(X).

wife(X, Y) :-
	is_married(X, Y),
	female(X).

% HW %
grand_son(X, Y) :-
	grandchild(X, Y),
	male(X).

grand_daughter(X, Y) :-
	grandchild(X, Y),
	female(X).

grandchild(X, Y) :-
	parent(Z, X),
	parent(Y, Z).

grand_parent(X, Y) :-
	parent(Z, Y),
	parent(X, Z).

grand_mother(Grand_mother, Grand_child) :-
	female(Grand_mother),
	grand_parent(Grand_mother, Grand_child).

grand_father(Grand_father, Grand_child) :-
	male(Grand_father),
	grand_parent(Grand_father, Grand_child).

nephew(Nephew, Nephews_Brother) :-
	male(Nephew),
	parent(Nephew_Parent, Nephew),
	has_same_male_parent(Nephew_Parent, Nephews_Brother).

aunt(Aunt, Nephew) :-
	female(Aunt),
	parent(Nephew_Parent, Nephew),
	has_same_male_parent(Aunt, Nephew_Parent).

uncle(Uncle, Nephew) :-
	male(Uncle),
	parent(Nephew_Parent, Nephew),
	has_same_male_parent(Uncle, Nephew_Parent).	


great_grandson(Great_grandchild, Great_grandparents) :-
	great_grandchild(Great_grandchild, Great_grandparents),
	male(Great_grandchild).

great_granddaughter(Great_grandchild, Great_grandparents) :-
	great_grandchild(Great_grandchild, Great_grandparents),
	female(Great_grandchild).

great_grandchild(Gread_grandchild, Grat_grandparrent) :-
	parent(Parent, Gread_grandchild),
	parent(Granny, Parent),
	parent(Grat_grandparrent, Granny).

cousins_daughter(X, Y) :-	
	female(X),
	cousins_child(X, Y).

cousins_son(X, Y) :-	
	male(X),
	cousins_child(X, Y).

cousins_child(Cousins_child, Y) :-		
	parent(Cousins_child_Parent, Cousins_child),
	parent(Y_Parent, Y),
	has_same_male_parent(Cousins_child_Parent, Y_Parent).

dvourodniy_plemiannik(X, Y) :-
	male(X),
	cousins_child(Cousins_child, Y),
	child(X, Cousins_child).

dvourodnaya_plemiannica(X, Y) :-
	female(X),
	cousins_child(Cousins_child, Y),
	child(X, Cousins_child).

has_same_male_parent(X, Y) :-
	male(X_and_Y_Parent_Male),
	has_same_parent(X_and_Y_Parent_Male, X, Y).

has_same_parent(Same_Parent, X, Y) :-
	not_the_same(X, Y),
	parent(Same_Parent, Y),
	parent(Same_Parent, X).

not_the_same(X, Y) :-
	\+(X = Y).	

brother_in_law(Brother_in_law, Wife) :-
	male(Brother_in_law),
	female(Wife),
	is_married(Husband, Wife),
	has_same_male_parent(Brother_in_law, Husband).



% ----------------------------------------------------------------------------------------


:- begin_tests(gene_tests).
:- use_module(library(lists)).

	test(husband) :-
	    husband('Nickolay Sr', 'Svetlana').
	test(not_husband, [fail]) :-
		husband('Nickolay Jr', 'Svetlana').

	test(wife) :-
		wife('Nina', 'Nickolay Iakovlevich').

	test(not_wife, [fail]) :-
		wife('Nina', 'Svetlana').

	test(grand_mother) :-
	    grand_mother('Valentina', 'Nickolay Jr').
	test(not_grand_son, [fail]) :-
		grand_mother('Svetlana', 'Natalia').		

	test(grand_father) :-
	    grand_father('Nickolay Iakovlevich', 'Nickolay Jr').
	test(not_grand_father, [fail]) :-
		grand_father('Svetlana', 'Natalia').		

	test(grand_son) :-
	    grand_son('Nickolay Jr', 'Valentina').
	test(not_grand_son, [fail]) :-
		grand_son('Nickolay Jr', 'Svetlana').		

	test(grand_daughter) :-
	    grand_daughter('Viktoria', 'Nina').
	test(not_grand_daughter, [fail]) :-
		grand_daughter('Viktoria', 'Svetlana').			

	test(nephew_1) :-
	    nephew('Nickolay Jr', 'Natalia').
	test(nephew_2) :-
	    nephew('Dmitriy', 'Natalia').
	test(not_nephew, [fail]) :-
		nephew('Viktoria', 'Svetlana').					

	test(aunt) :-
	    aunt('Natalia', 'Nickolay Jr').
	test(aunt_2) :-
	    aunt('Natalia', 'Dmitriy').
	test(not_aunt, [fail]) :-
		aunt('Viktoria', 'Svetlana').	

	test(great_grandson) :-
	    great_grandson('Viktoria_future_son', 'Nina').
	test(not_great_grandson, [fail]) :-
		great_grandson('Viktoria_future_daughter', 'Nina').

	test(great_granddaughter) :-
	    great_granddaughter('Viktoria_future_daughter', 'Nickolay Iakovlevich').
	test(not_great_granddaughter, [fail]) :-
		great_granddaughter('Viktoria_future_son', 'Nickolay Iakovlevich').	

	test(uncle) :-
		uncle('Nickolay Sr', 'Viktoria').
	test(not_uncle, [fail]) :-
		uncle('Natalia', 'Dmitriy').

	test(cousins_daughter) :-
		cousins_daughter('Viktoria', 'Nickolay Jr').
	test(not_cousins_daughter, [fail]) :-
		cousins_daughter('Nickolay Jr', 'Viktoria').	

	test(cousins_son) :-
		cousins_son('Nickolay Jr', 'Viktoria').
	test(not_cousins_son, [fail]) :-
		cousins_son('Viktoria', 'Nickolay Jr').

	test(dvourodniy_plemiannik) :-
		dvourodniy_plemiannik('Viktoria_future_son', 'Nickolay Jr').
	test(not_dvourodniy_plemiannik, [fail]) :-
		dvourodniy_plemiannik('Nickolay Jr', 'Viktoria_future_son').

	test(dvourodnaya_plemiannica) :-
		dvourodnaya_plemiannica('Viktoria_future_daughter', 'Nickolay Jr').
	test(not_dvourodnaya_plemiannica, [fail]) :-
		dvourodnaya_plemiannica('Nickolay Jr', 'Viktoria_future_daughter').
:- end_tests(gene_tests).





