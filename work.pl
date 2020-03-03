% TODO:: prolog_notes5.txt

% Miscellaneous code while learning Prolog at university.
% Coding style: predicate(input1, ..., inputN, optional_output).

% For declarative arithmetic.
:- use_module(library(clpfd)).
:- use_module(library(clpr)).

and(A, B) :- A, B.
or(A, B) :- A; B.
nand(A, B) :- not((and(A,B))).
nor(A, B) :- not((or(A,B))).
xor(A, B) :- or(A,B), nand(A,B).

% push_front
list_add(X, L, [X|L]).

my_member([E|_], E).
my_member([_|T], E) :- my_member(T, E).

my_length([], N) :- N #= 0.
my_length([_|T], N) :- 
		N #> 0,
		N #= M + 1, 
		my_length(T, M).

sum([], 0).
sum([H|T], Sum) :-
		sum(T, Res),
		Sum #= Res + H.

my_min([H], H).
my_min([H|T], E) :-
		my_min(T, E),
		H #> E.
my_min([H|T], H) :-
		my_min(T, E),
		H #=< E.

my_append([], L2, L2).
my_append([H|T], L2, [H|T1]) :-
		my_append(T, L2, T1).

% All L where X has been added somewhere.
insert(X, L, NL) :- 
		my_append(A, B, L), 
		my_append(A, [X|B], NL).
% [.....][......]  A + B
% [.....X.......]  NL = A + X + B

% All L where X has been removed.
remove(L, X, NL) :- 
		my_append(A, [X|B], L), 
		my_append(A, B, NL).

prefix(L, P) :- my_append(P, _, L).
% [....] + [.....] P + _
% [..............] L

suffix(L, S) :- my_append(_, S, L).
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
		my_append(SLL, [H|SRL], SL).
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
tree_generate(T) :- naturals(N), t(N, T).
t(N, []) :- N #= 0.
t(N, [A, B]) :-
        N #> 0,
        P #>= 0,
        Q #>= 0,
        P + Q + 1 #= N,
        t(P, A),
        t(Q, B).

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

% TODO: bst_count

% cartesian_product([L1, L2, _], L).
% L is every tuple of the cartesian product L1xL2x...xLn.
cartesian_product([], []).
cartesian_product([List1|Remainder], [H|T]) :-
		my_member(List1, H),
		cartesian_product(Remainder, T).

subset([], []).
subset([H|T], [H|S]) :- subset(T, S).
subset([_|T], S) :- subset(T, S).

inUnion(L1, L2, E) :-
		my_member(L1, E);
		my_member(L2, E).

inIntersection(L1, L2, E) :-
		my_member(L1, E),
		my_member(L2, E).

inDifference(L1, L2, E) :-
		my_member(L1, E),
		not(my_member(L2, E)).

areEqual(L1, L2) :-
		subset(L1, L2),
		subset(L2, L1).

remove_duplicates([], []).
remove_duplicates([H|T], [H|RD]) :-
		remove_duplicates(T, RD),
		not(my_member(H, RD)).
remove_duplicates([H|T], RD) :-
		remove_duplicates(T, RD),
		my_member(H, RD).

naturals(0).
naturals(X) :-
		X #= Y + 1,
		naturals(Y).

integers(0).
integers(N) :-
	naturals(M),
	M #> 0,
	(N #= M; N #= -M).

% my_between(L, R, X)
% All numbers X my_between L and R.
my_between(L, R, L) :- L #=< R.
my_between(L, R, X) :-
		L #< R,
		L1 #= L + 1,
		my_between(L1, R, X).

% Works both ways. Works with arithmetic statements.
factorial(X, Y) :- 
		X #= 0,
		Y #= 1.
factorial(X, Y) :-
		X #> 0,
		Y #= X * PrevY,
		PrevX #= X - 1, factorial(PrevX, PrevY).

even(X) :-
		X #= 2*X1,
		naturals(X1).
	
natural_pairs(X, Y) :-
		X #>= 0,
		Y #>= 0,
		X + Y #= N,
		naturals(N),
		label([X, Y]).

% Fibonacci: a3 = a2 + a1; 1 = 1 + 0
% fib(A1, A2, A3, A1 + A2).
fib(0, 1, N) :- N #= 1.
fib(A, B, N) :- N #= A + B, fib(_, A, B).

all_sums(N,[]) :- N #= 0.
all_sums(N, [H|T]) :- 
		N #> 0,
		my_between(1, N, H),
		M is N - H,
		all_sums(M, T).

% [A, B], A, B in Z, A #=< B
special_pair(A, B) :-
		naturals(B),
		my_between(0, B, A).

%new_is_sorted([]).
new_is_sorted(L) :-
		not((infix(L, [A, B]), A #> B)).

gen_ks(K, S, []) :-
		K #= 0,
		S #= 0.
gen_ks(K, S, [H|T]) :-
		K #> 0,
		S #> 0,
		my_between(1, S, H),
		gen_ks(K - 1, S - H, T).

flatten(X, [X]) :- not(is_list(X)).
flatten([], []).
flatten([H|T], R):- 
		flatten(H, FH), 
		flatten(T, FT), 
		append(FH, FT, R).

squash([], []).
squash(L, [H|T]) :-
		append(H, H1, L),
		H \= [],
		squash(H1, T).


