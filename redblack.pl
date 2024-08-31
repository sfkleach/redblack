/*
type Color
  = BB   -- +2  "double black" ("double black")
  | B    -- +1
  | R    --  0
  | RR   -- -1  "double red"   ("negative black")

type Tree a
  = E    --  0  "empty"        ("black leaf"        +1)
  | BE   -- +1  "black empty"  ("double black leaf" +2)
  | T Color (Tree a) a (Tree a)
*/

;;; Format of a node is Color(Left, Value, Right) where
;;; Colour is one of bb, b, r, rr.
;;; A tree is e, be, bb/3, b/3, r/3 or rr/3.

;;; --- balance_b_r_r ---
;;; Eliminate red-red violations.

balance_b_r_r(
    b(r(r(A, X, B), Y, C), Z, D),
    r(b(A, X, B), Y, b(C, Z, D))
).
balance_b_r_r(
    b(r(A, X, r(B, Y, C)), Z, D),
    r(b(A, X, B), Y, b(C, Z, D))
).
balance_b_r_r(
    b(r(A, X, r(B, Y, C)), Z, D),
    r(b(A, X, B), Y, b(C, Z, D))
).
balance_b_r_r(
    b(A, X, r(B, Y, r(C, Z, D))),
    r(b(A, X, B), Y, b(C, Z, D))
).

;;; --- balance_bb_r_r ---

balance_bb_r_r(
    bb(r(r(A, X, B), Y, C), Z, D),
    b(b(A, X, B), Y, b(C, Z, D))
).
balance_bb_r_r(
    bb(r(A, X, r(B, Y, C)), Z, D),
    b(b(A, X, B), Y, b(C, Z, D))
).
balance_bb_r_r(
    bb( A, X, r(r(B, Y, C), Z, D) ),
    b( b(A, X, B), Y, b(C, Z, D) )
).
balance_bb_r_r(
    bb( A, X, r(B, Y, r(C, Z, D)) ),
    b( b(A, X, B), Y, b(C, Z, D) )
).

;;; --- balance_bb_rr ---

balance_bb_rr(
    bb(rr(b(A, W, B), X, b(C, Y, Z)), Z, E),
    b(Q, Y, b(D, Z, E))
) :-
    balance( b(r(A, W, B), X, C), Q ).

balance_bb_rr(
    bb(A, W, rr(b(B, X, C), Y, b(D, Z, E))),
    b(b(A, W, B), X, R)
) :-
    balance( b(C, Y, r(D, Z, E)), R ).

;;; --- balance ---

balance( P, Q ) :- balance_b_r_r( P, Q ), !.
balance( P, Q ) :- balance_bb_r_r( P, Q ), !.
balance( P, Q ) :- balance_bb_rr( P, Q ), !.
balance( P, P ).

;;; --- unused ---

incr( b( A, X, B ), bb( A, X, B ) ).
incr( r( A, X, B ), b( A, X, B) ).
incr( rr( A, X, B ), r( A, X, B ) ).

decr( bb(A, X, B), b(A, X, B) ).
decr( b( A, X, B ), r( A, X, B ) ).
decr( r(A, X, B), rr(A, X, B) ).

;;; --- bubble BE and BB ---

bubble( r(b(A, X, B), Y, be), R ) :- balance( b( r(A, X, B), Y, E ), R ).
bubble( b(b(A, X, B), Y, be), R ) :- balance( bb( r(A, X, B), Y, E ), R ).
bubble( b(r(A, X, B), Y, be), R ) :- balance( bb( rr(A, X, B), Y, E ), R ).

bubble( r(be, Y, b(C, Z, D)), R ) :- balance( b( r(A, X, B), Y, E ), R ).
bubble( b(be, Y, b(C, Z, D)), R ) :- balance( bb( r(A, X, B), Y, E ), R ).
bubble( b(be, Y, r(C, Z, D)), R ) :- balance( bb( rr(A, X, B), Y, E ), R ).

bubble( r( b(A, X, B), Y, bb(C, Z, D) ), R ) :- balance( b( r(A, X, B), Y, b(C, D, E) ), R).
bubble( r( bb(A, X, B), Y, b(C, Z, D) ), R ) :- balance( b( bb(A, X, B), Y, b(C, D, E) ), R).
bubble( b( b(A, X, B), Y, bb(C, Z, D) ), R ) :- balance( bb( r(A, X, B), Y, b(C, D, E) ), R).
bubble( b( bb(A, X, B), Y, b(C, Z, D) ), R ) :- balance( bb( b(A, X, B), Y, r(C, D, E) ), R).
bubble( b( r(A, X, B), Y, bb(C, Z, D) ), R ) :- balance( bb( rr(A, X, B), Y, b(C, D, E) ), R).
bubble( b( bb(A, X, B), Y, r(C, Z, D) ), R ) :- balance( bb( b(A, X, B), Y, rr(C, D, E) ), R).

;;; --- Removal ---

rem( N, e, e ) :- !.

;;; 0-children.
rem( N, r(e, N, e), be ) :- !.
rem( N, r(e, X, e), r(e, X, e) ) :- X \= N, !.
rem( N, b(e, N, e), bb(e, N, e) ) :- !.
rem( N, b(e, X, e), b(e, X, e) ) :- X \= N, !.

;;; 1-child.
rem( N, b(r(e, X, e), N, e), b(e, X, e) ) :- !.
rem( N, b(r(e, N, e), Y, e), b(e, Y, e) ) :- !.     ;;; FIX
rem( N, b(r(e, X, e), Y, e), b(r(e, X, e), Y, e) ) :- Y \= N, !.

rem( N, b(e, N, r(e, Z, e)), b(e, Z, e) ) :- !.
rem( N, b(e, Y, r(e, N, e)), b(e, Y, e) ) :- !.     ;;; FIX
rem( N, b(e, Y, r(e, Z, e)), b(e, Y, r(e, Z, e)) ) :- Y \= N, !.

;;; 2-children.
rem( N, LHS, RHS ) :-
    LHS =.. [C, L, Y, R],
    N < Y,
    !,
    rem( N, L, Lrem ),
    bubble( Lrem, Bubbled ),
    New =.. [C, Bubbled, Y, R],
    balance( New, RHS ).

rem( N, LHS, RHS ) :-
    LHS =.. [C, L, Y, R],
    N > Y,
    !,
    rem( N, R, Rrem ),
    bubble( Rrem, Bubbled ),
    New =.. [C, L, Y, Bubbled],
    balance( New, RHS ).

rem( N, LHS, RHS ) :-
    LHS =.. [C, L, N, R],
    rem_2_children( LHS, RHS ).


rem_2_children(LHS, RHS) :-
    LHS =.. [C, Left, Y, Right],
    remove_max(C, Left, X, NewLeft),
    RHS =.. [C, NewLeft, X, Right].


remove_max( C, r(e, X, e), X, e ) :- !.
remove_max( C, b(e, X, e), X, e ) :- !.
remove_max( C, b(r(e, W, e), X, e), X, b(e, W, e) ) :- !.
remove_max( C, LVR, X, NewLeft ) :-
    LVR =.. [Colour, L, V, R],
    remove_max( C, R, X, R2 ),
    NewLeft =.. [C, L, V, R2].

remove( X, Tree, Result ) :-
    rem( X, Tree, Tree2 ),
    Tree2 =.. [_, L, Y, R],
    Result =.. [b, L, Y, R].

ex1( b(r(e, 20, e), 30, e) ).
