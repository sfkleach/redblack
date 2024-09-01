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

;;; Consider - e0/0, e1/0, b2/3, b1/3, r0/3, r_1/3

;;; --- Invariants ---

valid( T ) :-
    only_r_and_b( T ),
    tree_invariant( T ).

tree_invariant( T ) :-
    ordered( T ),
    not( r_r_violation( T ) ),
    weight( T, _ ).



only_r_and_b( e ).
only_r_and_b( r( L, _, R ) ) :-
    only_r_and_b( L ),
    only_r_and_b( R ).
only_r_and_b( b( L, _, R ) ) :-
    only_r_and_b( L ),
    only_r_and_b( R ).

;;; ordered( in T )

ordered( e ).
ordered( be ).
ordered( T ) :-
    T =.. [_, _, N, _],
    ordered( T, N, Lmin, Lmax ).

;;; ordered( in Tree, in DefaultValue, out TreeMin, out TreeMax )
ordered( e, N, N, N ).
ordered( be, N, N, N ).
ordered( T, _, Lmin, Lmax ) :-
    T =.. [C, L, N, R],
    not( same_value( N, L ) ),
    not( same_value( N, R ) ),
    ordered( L, N, Lmin, Lmax ),
    Lmax =< N,
    ordered(R, N, Rmin, Rmax ),
    N =< Rmin.

same_value( N, T ) :-
    T =.. [_, _, N, _].

r_r_violation( T ) :-
    T =.. [_, L, _, R],
    base_color( T, r ),
    ( base_color( R, r ); base_color( L, r ) ),
    !.
r_r_violation( T ) :-
    T =.. [_, L, _, R],
    ( r_r_violation( L ); r_r_violation( R ) ).


base_colour( e, b ).
base_colour( be, b ).
base_colour( b(_, _, _), b ).
base_colour( bb(_, _, _), b ).
base_colour( r(_, _, _), r ).
base_colour( rr(_, _, _), r ).

weight( e, 0 ).
weight( be, 1 ).
weight( T, W ) :-
    T =.. [C, L, _, R],
    weight( L, Lw ),
    weight( R, Rw ),
    !,
    Lw = Rw,
    colour_weight( C, Cw ),
    W is Lw + Cw.

colour_weight( b, 1 ).
colour_weight( bb, 2 ).
colour_weight( r, 0 ).
colour_weight( rr, -1 ).



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

/*
balance_BB_RR : BalanceMaybe comparable
balance_BB_RR color l val r =
  case (color, (l, val, r)) of
    (BB, (T RR (T B a w b) x (T B c y d), z, e)) -> Just <| T B (balance B (T R a w b) x c) y (T B d z e)
    (BB, (a, w, T RR (T B b x c) y (T B d z e))) -> Just <| T B (T B a w b) x (balance B c y (T R d z e))
    _                                            -> Nothing
*/

redden(b(A, W, B), r(A, W, B)).
redden(e, e).                       ;;; FIX

balance_bb_rr(
    bb(rr(BAWB, X, b(C, Y, Z)), Z, E),
    b(Q, Y, b(D, Z, E))
) :-
    !,
    redden(BAWB, RAWB),
    balance( b(RAWB, X, C), Q ).

balance_bb_rr(
    bb(A, W, rr(b(B, X, C), Y, BDZE)),
    b(b(A, W, B), X, R)
) :-
    !,
    redden(BDZE, RDZE),
    balance( RDZE, R ).

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

;;; BE

bubble( r(b(A, X, B), Y, be), R ) :- !, balance( b( r(A, X, B), Y, E ), R ).
bubble( b(b(A, X, B), Y, be), R ) :- !, balance( bb( r(A, X, B), Y, E ), R ).
bubble( b(r(A, X, B), Y, be), R ) :- !, balance( bb( rr(A, X, B), Y, E ), R ).

bubble( r(be, Y, b(C, Z, D)), R ) :- !, balance( b(e, Y, rr(C, Z, D)), R ).
bubble( b(be, Y, b(C, Z, D)), R ) :- !, balance( bb(e, Y, r(C, Z, D)), R).
bubble( b(be, Y, r(C, Z, D)), R ) :- !, balance( bb(e, Y, rr(C, Z, D)), R ).

;;; BB

bubble( r( b(A, X, B), Y, bb(C, Z, D) ), R ) :- !, balance( b( r(A, X, B), Y, b(C, D, E) ), R).
bubble( r( bb(A, X, B), Y, b(C, Z, D) ), R ) :- !, balance( b( bb(A, X, B), Y, b(C, D, E) ), R).
bubble( b( b(A, X, B), Y, bb(C, Z, D) ), R ) :- !, balance( bb( r(A, X, B), Y, b(C, D, E) ), R).
bubble( b( bb(A, X, B), Y, b(C, Z, D) ), R ) :- !, balance( bb( b(A, X, B), Y, r(C, D, E) ), R).
bubble( b( r(A, X, B), Y, bb(C, Z, D) ), R ) :- !, balance( bb( rr(A, X, B), Y, b(C, D, E) ), R).
bubble( b( bb(A, X, B), Y, r(C, Z, D) ), R ) :- !, balance( bb( b(A, X, B), Y, rr(C, D, E) ), R).

;;; Otherwise
bubble( T, T ).

;;; --- Removal ---

rem( N, e, e ) :- !.

;;; 0-children.
rem( N, r(e, N, e), b ) :- !.           ;;; FIX
rem( N, T, T ) :- T = r(e, _, e), !.
rem( N, b(e, N, e), be ) :- !.          ;;; FIX

rem( N, T, T ) :- T = b(e, _, e), !.

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

bad( b(r(e, 20, e), 20, e) ).
ex1( b(r(e, 20, e), 30, e) ).
ex2( b(r(e, 20, e), 30, r(e, 40, e)) ).

?- ex2(T20), remove(20, T20, Result).
?- ex2(T30), remove(30, T30, Result).
?- ex2(T40), remove(40, T40, Result).
