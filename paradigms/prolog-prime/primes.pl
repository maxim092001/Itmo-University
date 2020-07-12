divisible_(X, Y) :-
    0 is mod(X, Y), !.

composite(1).

prime(2).
prime(N) :-
    N > 2,
    not(composite(N)).

init(MAXN) :-
    sieve_(2, MAXN).

inside_sieve_(Index, N, Increment) :-
    Index =< N,
    assert(composite(Index)),
    Index1 is Index + Increment,
    inside_sieve_(Index1, N, Increment).

sieve_(Index, Limit) :-
    Index =< Limit,
    not(composite(Index)),
    SquareIndex is Index * Index,
    SquareIndex =< Limit,
    inside_sieve_(SquareIndex, Limit, Index).

sieve_(Index, N) :-
    Index =< N,
    Index1 is Index + 1,
    sieve_(Index1, N).

number_for_divisors_(1, _, []).
number_for_divisors_(Number, Prev, [Head | Tail]) :-
    prime(Head),
    Prev =< Head,
    number_for_divisors_(Number1, Head, Tail),
    Number is Number1 * Head.

divisors_for_number_(Number, Divisor, [Number]) :-
  Divisor * Divisor > Number, !.
divisors_for_number_(Number, Divisor, [Head | Tail]) :-
    divisible_(Number, Divisor),
    Head is Divisor,
    Number1 is div(Number, Divisor),
    divisors_for_number_(Number1, Divisor, Tail).
divisors_for_number_(Number, Divisor, [Head | Tail]) :-
    not(divisible_(Number, Divisor)),
    Divisor1 is Divisor + 1,
    divisors_for_number_(Number, Divisor1, [Head | Tail]).

dec_to_kth_(0, _, []) :- !.
dec_to_kth_(N, K, [Head | Tail]) :-
    N1 is div(N, K),
    Head is mod(N, K),
    dec_to_kth_(N1, K, Tail).

is_palindrome_(N, K) :-
    dec_to_kth_(N, K, List),
    reverse(List, List).

prime_palindrome(N, K) :-
    prime(N),
    is_palindrome_(N, K).

prime_divisors(1, []) :- !.
prime_divisors(Number, Divisors) :-
    integer(Number),
    divisors_for_number_(Number, 2, Divisors).
prime_divisors(Number, Divisors) :-
    not(integer(Number)),
    number_for_divisors_(Number, 2, Divisors).
