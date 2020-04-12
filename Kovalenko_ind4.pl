ind4:-Index is 0, N is 3,owner(Index,N,[собака,кошка,хом€чок]).
owner(N,N,List):-!.
owner(Index,N,List):-ind_access(Index,List,Pet),хоз€ин(Owner,Pet),write(Owner),write(' - хоз€ин '),write(Pet),nl,Index1 is Index+1,owner(Index1,N,List).
ind_access(0,[Elem|_Tail],Elem):-!.
ind_access(Index,_List,_Elem):-Index<0,!,fail.
ind_access(Index,[_Head|Tail],Elem):-NextIndex is Index-1,ind_access(NextIndex,Tail,Elem).
гул€ет(X,Y):-хоз€ин(X,Y).
дружит(лена,Y):-гул€ет(Y,кошка).
нет(пет€,кошка).
одноподъездники(пет€,Y):-дружит(Y,C).
одноподъездники(X,Y):-нет(Y,кошка).
хоз€ин(тан€,кошка).
хоз€ин(X,собака):-нет(X,кошка),одноподъездники(X,C),!.
хоз€ин(X,хом€чок):-одноподъездники(X,пет€),дружит(X,—),!.

