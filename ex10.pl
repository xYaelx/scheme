/* ^^ Functional and Logical Programming 2017 - Ex.10 ^^ */


/* -- Part 1 - Tree Representation -- */
/* tree_insert is same as shown in class */
tree_insert(V, [], tree([], V, [])) :- !.
tree_insert(V, tree(L, Root, R), Res) :- 
	V > Root, 
	tree_insert(V, R, NR),
	Res = tree(L, Root, NR), !.
tree_insert(V, tree(L, Root, R), Res) :-
	V < Root,
	tree_insert(V, L, NL),
	Res = tree(NL, Root, R).

/* A - The predicate succeeds if there exists a node in T such that it value is V */
tree_contains(tree(_, V, _), V).
tree_contains(tree(L, _, _), V) :- tree_contains(L, V). /* go to the left subtree */
tree_contains(tree(_, _, R), V) :- tree_contains(R, V). /* go to the right subtree */

/* B - The predicate succeeds if T is a sorted binary tree that contains all elements of L.*/
list_to_tree([], []).
/* start with the root ->left node -> right node */
list_to_tree([A | L], R) :-
	list_to_tree(L, R1),
	tree_insert(A, R1, R).

/* C - The predicate succeeds if TF is the flipped tree of T.*/
/*
tree_flip([], []).
tree_flip(tree(L, V, R), tree(RF, V, LF)) :-
	tree_flip(L, LF),
	tree_flip(R, RF).
	*/
tree_flip(tree([], X, []), tree([], X, [])) :- !.
tree_flip(tree(L, V, R), tree(R, V, L1)) :- tree_flip(L, L1).
tree_flip(tree(L, V, R), tree(R1, V, L)) :- tree_flip(R, R1).


/* D - The predicate succeeds if L is a inorder representation of the elements in T*/
tree_inorder([], []).
tree_inorder(tree(L, V, R), RES) :-
/* go LTR and check the order */
	tree_inorder(L, PL),
	tree_inorder(R, PR),
	append(PL, [V], PLV),
	append(PLV, PR, RES).

/* E - The predicate succeeds if R is the result of sorting L */
tree_sort(L, R) :-
	list_to_tree(L, T),
	tree_inorder(T, R).

/* -- Part 2 - More Prolog -- */
/* A - The predicate succeeds if R is the maximal element in L */
find_max([X], X).
find_max([X|Xs], R):- find_max(Xs, T), (X > T -> R = X ; R = T).

/* B - The predicate succeeds there exists a list R, which is a subset of L, and its total sum is X. */
/* sum_helper(L, N) :- sum of elements in L is equal to N */
sum_helper([], 0).
sum_helper([H|T], N) :- sum_helper(T, N1), N is N1 + H.
subset_sum(X, L, N) :- subset(X, L), sum_helper(X, N).

/* C */
triangular_helper(1, _, _, []).
triangular_helper(N, A, B, [C | R]) :- 
	N1 is N - 1,
	A1 is A + 1,
	C is A + B,
	fibo_helper(N1, A1, C, R), !.
trinagular(N, [1 | R]) :- triangular_helper(N, 2, 1, R).
