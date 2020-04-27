primes(3).
primes(5).
prime_check(X):-primes(X),!.
prime_check(X):-pr(X),asserta(primes(X)).
pr(2):-!.
pr(X):-pr1(2,X).
pr1(X,X):-!.
pr1(I,X):-Y is X mod I, not(Y=0), I1 is I+1,pr1(I1,X).
next_prime_multiplier(Number,Multiplier,Next_multiplier):-Number>=Multiplier,prime_check(Multiplier),
Remainder is Number mod Multiplier,Remainder=0, Next_multiplier is
Multiplier, !.
next_prime_multiplier(Number,Multiplier,Next_multiplier):-
Number>=Multiplier,Next_Number is Multiplier+1,
next_prime_multiplier(Number,Next_Number,Next_multiplier).
prime_multiplier_counter(Number,1,Dividor,Iter):-Iter<4,!,fail.
prime_multiplier_counter(Number,Dynamic_Number,Dividor,Iter):-Iter =
4,Number1 is
Dividor+1,next_prime_multiplier(Dynamic_Number,Number1,Next_multiplier),!,fail.
prime_multiplier_counter(Number,_,Dividor,Iter):-Iter = 4,Dividor>1,!.
prime_multiplier_counter(Number,Dynamic_Number,Dividor,Iter):-next_prime_multiplier(Dynamic_Number,Dividor,Next_multiplier),Dynamic_Number1
is Dynamic_Number div Next_multiplier, Iter1 is Iter+1,Number1 is
Next_multiplier+1,
prime_multiplier_counter(Number,Dynamic_Number1,Number1,Iter1).
result(Number,Tally1,Tally2,Tally3,Tally4):-prime_multiplier_counter(Number,Number,1,0),Number1
is Number+1,prime_multiplier_counter(Number1,Number1,1,0), Number2 is
Number+2,prime_multiplier_counter(Number2,Number2,1,0), Number3 is
Number+3,prime_multiplier_counter(Number3,Number3,1,0),Tally1 is Number,
Tally2 is Number1,Tally3 is Number2, Tally4 is Number3.
result(Number,Tally1,Tally2,Tally3,Tally4):-Number1 is
Number+1,result(Number1,Tally1,Tally2,Tally3,Tally4).
