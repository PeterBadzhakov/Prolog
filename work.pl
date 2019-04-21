% For declarative arithmetic.
:- use_module(library(clpfd)).

and(A, B) :- A, B.
or(A, B) :- A; B.
nand(A, B) :- not((and(A,B))).
nor(A, B) :- not((or(A,B))).
xor(A, B) :- or(A,B), nand(A,B).

add(X, L, [X|L]). % push_front

member([E|_], E).
member([_|T], E) :- member(T, E).

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

is_sorted([]).
is_sorted([_]).
is_sorted([H1, H2|T]) :- 
		H1 =< H2,
		is_sorted([H2|T]).

bogo_sort(L, SL) :-
		permute(L, SL),
		is_sorted(SL).
