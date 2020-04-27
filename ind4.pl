ind4:-Index is 0, N is 3,pet(Index,N,[таня,лена,петя]).
pet(N,N,List):-!.
pet(Index,N,List):-ind_access(Index,List,Owner),хозяин(Owner,Pet),write(Owner),write(' - хозяин '),write(Pet),nl,Index1 is Index+1,pet(Index1,N,List).
ind_access(0,[Elem|_Tail],Elem):-!.
ind_access(Index,_List,_Elem):-Index<0,!,fail.
ind_access(Index,[_Head|Tail],Elem):-NextIndex is Index-1,ind_access(NextIndex,Tail,Elem).
гуляет(X,Y):-хозяин(X,Y).
дружит(лена,Y):-гуляет(Y,кошка).
нет(петя,кошка).
одноподъездники(петя,Y):-дружит(Y,C).
одноподъездники(X,Y):-нет(Y,кошка).
хозяин(таня,кошка).
хозяин(X,собака):-нет(X,кошка),одноподъездники(X,C),!.
хозяин(X,хомячок):-одноподъездники(X,петя),дружит(X,C),!.
