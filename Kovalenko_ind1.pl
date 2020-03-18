%я сделал другую проверку числа на простоту, т.к. она работает быстрее
/*аргументы предиката result(..):
Number - отправное число, с которого начинаем поиск нужной
последовательности;
Tally1,..,Tally4 - переменные для искомой последовательности*/

prime_check(1):-!,fail.
prime_check(2):-!.
prime_check(Number):-Number < 5,Number>2,Number mod 2 =\= 0,!.
prime_check(Number):-!,Number > 0,Square is Number*Number, Remainder is
Square mod 24, Remainder = 1.
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
