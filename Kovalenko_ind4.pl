ind4:-Index is 0, N is 3,owner(Index,N,[������,�����,�������]).
owner(N,N,List):-!.
owner(Index,N,List):-ind_access(Index,List,Pet),������(Owner,Pet),write(Owner),write(' - ������ '),write(Pet),nl,Index1 is Index+1,owner(Index1,N,List).
ind_access(0,[Elem|_Tail],Elem):-!.
ind_access(Index,_List,_Elem):-Index<0,!,fail.
ind_access(Index,[_Head|Tail],Elem):-NextIndex is Index-1,ind_access(NextIndex,Tail,Elem).
������(X,Y):-������(X,Y).
������(����,Y):-������(Y,�����).
���(����,�����).
���������������(����,Y):-������(Y,C).
���������������(X,Y):-���(Y,�����).
������(����,�����).
������(X,������):-���(X,�����),���������������(X,C),!.
������(X,�������):-���������������(X,����),������(X,�),!.

