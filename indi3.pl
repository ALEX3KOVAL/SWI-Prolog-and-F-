pr5_4:-see('file1.txt'),tell('result.txt'),read_str_str.

write_str([]):-!.
write_str([H|T]):-put(H),write_str(T),!.

read_str_str:-read_str_file(Flag),read_str_st(Flag).

read_str_st(-1):-!.
read_str_st(10):-read_str_file(Flag),read_str_st(Flag).

read_str_file(Flag):-get0(X),r_str(X,[],Flag).

r_str(10,A,10):-write_str(A),put(32),nl,!.
r_str(47,A,Flag):-get0(X1),left(X1,A,Flag),!.
r_str(-1,A,-1):-write_str(A),seen,told,!.
r_str(X,B,Flag):-append(B,[X],B1), get0(X1),r_str(X1,B1,Flag).
left(42,A,Flag):-get0(X1),skipc(X1,A,Flag).
left(10,A,Flag):-get0(X1),left(X1,A,Flag).
left(-1,A,-1):-r_str(-1,A,-1),!.
left(B,A,Flag):-append(A,[47],A1),append(A1,[B],B1),get0(X1),r_str(X1,B1,Flag).
right(47,A,Flag):-get0(X1),append(A,[32],A1),r_str(X1,A1,Flag),!.
right(B,A,_):-skipc(B,A,_).
right(10,A,_):-get0(X1),right(X1,A,_).
right(-1,A,-1):-r_str(-1,A,-1),!.

skipc(42,A,Flag):-get0(X1),right(X1,A,Flag).
skipc(_,A,_):-get0(X1),skipc(X1,A,_).
