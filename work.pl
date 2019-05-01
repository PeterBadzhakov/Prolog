% TODO: prolog_notes5.txt
% Miscellaneous code while learning Prolog at university.
% Coding style: predicate(input1, ..., inputN, optional_output).

% For declarative arithmetic.
:- use_module(library(clpfd)).

and(A, B) :- A, B.
or(A, B) :- A; B.
nand(A, B) :- not((and(A,B))).
nor(A, B) :- not((or(A,B))).
xor(A, B) :- or(A,B), nand(A,B).

% push_front
list_add(X, L, [X|L]).

%member([E|_], E).
%member([_|T], E) :- member(T, E).

len([], N) :- N #= 0.
len([_|T], N) :- 
		N #= M + 1, 
		len(T, M).

min([H], H).
min([H|T], E) :-
		min(T, E),
		H #> E.
min([H|T], H) :-
		min(T, E),
		H #=< E.

append([], B, B).
append([H|T], B, [H|R]) :- append(T, B, R).

% All L where X has been added somewhere.
insert(X, L, NL) :- 
		append(A, B, L), 
		append(A, [X|B], NL).
% [.....][......]  A + B
% [.....X.......]  NL = A + X + B

% All L where X has been removed.
remove(X, L, NL) :- 
		append(A, [X|B], L), 
		append(A, B, NL).

prefix(L, P) :- append(P, _, L).
% [....] + [.....] P + _
% [..............] L

suffix(L, S) :- append(_, S, L).
% [....] + [.....] _ + S
% [..............] L

infix(L, I) :-
		suffix(L, S),
		prefix(S, I).
% [..............] L
%         [......] S
%         [...]    Sub

permute([], []).
permute([H|T], P) :- 
		permute(T, PT), 
		insert(H, PT, P).
% [] -> []
% [a] -> [a]
% [b, a] -> [b, a]; [a, b]
% [a, b, c] -> [a, b, c]....

reverse(L, RL) :-
		reverse_rec(L, [], RL).
reverse_rec([], S, S).
reverse_rec([H|T], S, RL) :- 
		reverse_rec(T, [H|S], RL).

% List, number, element.
nth_element([H|_], 0, H).
nth_element([_|T], N, E) :-
		N #= M + 1,
		nth_element(T, M, E).

all_at_even([], []).
all_at_even([_], []).
all_at_even([H1, _|T], [H1, L]) :-
		all_at_even(T, L).

is_sorted([]).
is_sorted([_]).
is_sorted([H1, H2|T]) :- 
		H1 =< H2,
		is_sorted([H2|T]).

bogo_sort(L, SL) :-
		permute(L, SL),
		is_sorted(SL).

% partition(Pivot, List, LL, RL).
quick_sort([], []).
quick_sort([H|T], SL) :- 
		qs_partition(H, T, LL, RL),
		quick_sort(LL, SLL), 
		quick_sort(RL, SRL), 
		append(SLL, [H|SRL], SL).
qs_partition(_, [], [], []).
qs_partition(Pivot, [H|T], [H|LL], RL) :- 
		H #< Pivot, 
		qs_partition(Pivot, T, LL, RL).
qs_partition(Pivot, [H|T], LL, [H|RL]) :- 
		H #>= Pivot,
		qs_partition(Pivot, T, LL, RL).

% Tree is a list defined as
% [Root, Left, Right]
% Where Root is an element; Left and Right are trees.

% [] is empty tree.
% [Root, Left, Right]: Root, Left subtree, Right subtree.
bst_insert(Element, [], [Element, [], []]).
bst_insert(Element, [Root, Left, Right], [Root, Left_choice, Right]) :-
		Element #=< Root,
		bst_insert(Element, Left, Left_choice).
bst_insert(Element, [Root, Left, Right], [Root, Left, Right_choice]) :-
		Element #> Root,
		bst_insert(Element, Right, Right_choice).

% Makes BST from list.
bst_from_list(L, BST) :-
		% *_rec adds in reverse order, mitigate.
		reverse(L, RL),
		bst_from_list_rec(RL, BST).
bst_from_list_rec([], []).
bst_from_list_rec([H|T], BST) :-
	bst_from_list_rec(T, BST_last),
	bst_insert(H, BST_last, BST).

bst_pre_order([]).
bst_pre_order([Root, Left, Right]) :-
		write(Root),
		bst_pre_order(Left),
		bst_pre_order(Right).

bst_in_order([]).
bst_in_order([Root, Left, Right]) :-
		bst_in_order(Left),
		write(Root),
		bst_in_order(Right).

bst_post_order([]).
bst_post_order([Root, Left, Right]) :-
		bst_post_order(Left),
		bst_post_order(Right),
		write(Root).

% cartesian_product([L1, L2, _], L).
% L is every tuple of the cartesian product L1xL2x...xLn.
cartesian_product([], []).
cartesian_product([List1|Remainder], [H|T]) :-
		member(List1, H),
		cartesian_product(Remainder, T).

subset([], []).
subset([H|T], [H|S]) :- subset(T, S).
subset([_|T], S) :- subset(T, S).
