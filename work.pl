and(A, B) :- A, B.
or(A, B) :- A; B.
nand(A, B) :- not((and(A,B))).
nor(A, B) :- not((or(A,B))).
xor(A, B) :- or(A,B), nand(A,B).
if(A, B) :- A -> B.
iff(A, B) :- and(A -> B, B -> A).
less(A, B) :- A < B.

add(X, L, [X|L]). % push_front

member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

append([], B, B).
append([H|T], B, [H|R]) :- append(T, B, R).

% All L where X has been added somewhere.
insert(X, L, NL) :- append(A, B, L), append(A, [X|B], NL).
% [.....][......]  A + B
% [.....X.......]  NL = A + X + B

% All L where X has been removed.
remove(X, L, NL) :- append(A, [X|B], L), append(A, B, NL).

%prefix([], _).
%prefix([H|T], [H|T1]) :- prefix(T, T1).
prefix(P, L) :- append(P, _, L).
% [....] + [.....] P + _
% [..............] L

%suffix(L, L).
%suffix(L, [_|L1]) :- suffix(L, L1).
suffix(S, L) :- append(_, S, L).
% [....] + [.....] _ + S
% [..............] L

infix(Sub, L) :- suffix(S, L), prefix(Sub, S).
% [..............] L
%         [......] S
%         [...]    Sub

len([], 0).
len([_|T], N) :- len(T, M), N is M + 1.

permutation([], []).
permutation([H|T], P) :- permutation(T, PT), insert(H, PT, P).
% [] -> []
% [a] -> [a]
% [b, a] -> [b, a]; [a, b]
% [a, b, c] -> [a, b, c]....

is_sorted(L) :- not(( append(_, [A, B | _], L), A > B )).

reverse_rec([], S, S).
reverse_rec([H|T], S, RL) :- 
	reverse_rec(T, [H|S], RL).
reverse(L, RL) :-
	reverse_rec(L, [], RL).

min(M, [M]).
min(M, L) :- 
	member(M, L),
	not((member(X, L), less(X, M))).

nthElement(X, 0, [X|_]).
nthElement(X, N, [_|T]) :-
	nthElement(X, M, T),
	N is M + 1.


