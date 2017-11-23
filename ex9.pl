/* ^^ Functional and Logical Programming 2017 - Ex.9 ^^ */


/* -- Part 1 - Simple Prolog -- */
/* odd(X) succeeds only if X is odd. */
odd(1) :- !.
odd(2) :- fail.
/* if X-2 is odd then X is odd */
odd(X) :- X > 2, 
		  X1 is X-2, 
		  odd(X1).

/* triangularNth(N,Value) succeeds if Y is the Xth element in a triangular number series. */
triangularNth(0,0) :- !.
/* triangularNth(N, V1+N) = triangularNth(N-1, V1) */
triangularNth(N, Value) :-	N > 0,
							N1 is N-1,
							triangularNth(N1, T1),
							Value is T1 + N.

/* fiboNth(N, Value) succeeds if Y is the Xth element in a Fibonacci series. */
fiboNth(0,0) :- !.
fiboNth(1, 1) :- !.
/* fiboNth(N, V1+V2) = fiboNth(N-1, V1) + fiboNth(N-2, V2) */
fiboNth(N, Value) :- A is N - 1, fiboNth(A, A1),
  					 B is N - 2, fiboNth(B, B1),
  					 Value is A1 + B1.

