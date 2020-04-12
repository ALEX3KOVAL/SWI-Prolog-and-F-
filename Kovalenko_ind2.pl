ind2:-write('Задайте число'),nl,read(Number),write('Введите размерность'),nl,read(N),write('Введите список'),nl,read_list(A,N),check_summ(0,N,A,[0],Number).
check_summ(N,N,_,L_list,Number):-Number>0,d_elet_e(L_list,Number,L1_list),last(L1_list,Last_elem),Last_elem=Number,nl,write('YES!!!'),nl,!;
Number>0,nl,write('NO!!!'),nl,!;
Number=0,d_elet_e(L_list,Number,L1_list),freq(L1_list,0,Count),degree(2,N,Degree),Count>Degree,nl,write('YES!!!'),nl,!;
Number=0,nl,write('NO!!!'),nl,!;
Number<0,down0_delete(L_list,Number,L1_list),first(L1_list,First_elem),First_elem=Number,nl,write('YES!!!'),nl,!;
nl,write('NO!!!'),nl,!.
check_summ(Index,N,Main_list,L_list,Number):-Number>0,ind_access(Index,Main_list,Cur_elem),add_increase(Cur_elem,L_list,T_list),sort(L_list,[],[],L_sort,0),sort(T_list,[],[],T_sort,0),ppend(L_sort,T_sort,U_list),sort(U_list,[],[],U_sort,0),ppend(L_list,U_sort,Lnew_list),sort(Lnew_list,[],[],Lnew1_list,0),first(Lnew1_list,First),list_compression(1,Lnew1_list,[First],Comp_L_list),del_duplicates(Comp_L_list,Comp1_L_list),d_elet_e(Comp1_L_list,Number,Comp2_L_list),Index1 is Index+1,check_summ(Index1,N,Main_list,Comp2_L_list,Number);
Number=0,ind_access(Index,Main_list,Cur_elem),add_increase(Cur_elem,L_list,T_list),sort(L_list,[],[],L_sort,0),sort(T_list,[],[],T_sort,0),ppend(L_sort,T_sort,U_list),sort(U_list,[],[],U_sort,0),ppend(L_list,U_sort,Lnew_list),sort(Lnew_list,[],[],Lnew1_list,0),Index1 is Index+1,check_summ(Index1,N,Main_list,Lnew1_list,Number);
Number<0,ind_access(Index,Main_list,Cur_elem),add_increase(Cur_elem,L_list,T_list),sort(L_list,[],[],L_sort,0),sort(T_list,[],[],T_sort,0),ppend(L_sort,T_sort,U_list),sort(U_list,[],[],U_sort,0),ppend(L_list,U_sort,Lnew_list),sort(Lnew_list,[],[],Lnew1_list,0),first(Lnew1_list,First),list_compression(1,Lnew1_list,[First],Comp_L_list),del_duplicates(Comp_L_list,Comp1_L_list),Index1 is Index+1,check_summ(Index1,N,Main_list,Comp_L_list,Number).
sort([],[],B,B,0):-!.
sort([],[X],L,B,1):-ppend(L,[X],L1),sort(L1,[],[],B,0),!.
sort([],[X],L,B,Flag):-ppend(L,[X],L1),sort([],[],L1,B,Flag),!.
sort([X|Y],[],L,B,Flag):-sort(Y,[X],L,B,Flag),!.
sort([X|Y],[I],L,B,_):-X<I,Flag1 is 1, ppend(L,[X],L1),sort(Y,[I],L1,B,Flag1).
sort([X|Y],[I],L,B,Flag):-X>=I,ppend(L,[I],L1),sort(Y,[X],L1,B,Flag).
read_list(A,N):-read1([],A,0,N).
read1(A,A,N,N):-!.
read1(B,A,I,N):-I1 is I+1,read(X),ppend(B,[X],B1),read1(B1,A,I1,N).
ppend([],L,L).
ppend([X|R1],L2,[X|R3]):-ppend(R1,L2,R3).
d_elet_e([],_,[]).
d_elet_e([H|T],Elem,[H|Tail]):-H=<Elem,d_elet_e(T,Elem,Tail).
d_elet_e([H|T],Elem,Tail):-H>Elem,d_elet_e(T,Elem,Tail).
ind_access(0,[Elem|_Tail],Elem):-!.
ind_access(Index,_List,_Elem):-Index<0,!,fail.
ind_access(Index,[_Head|Tail],Elem):-NextIndex is Index-1,ind_access(NextIndex,Tail,Elem).
add(E,[],[E]).
add(E,[H|T],[H|T1]):-add(E,T,T1).
list_compression(Index,Main_list,Tmp_list,Result_list):-length(Main_list,Length),Index=Length,ppend([],Tmp_list,Result_list),!;
ind_access(Index,Main_list,Next_elem),last(Tmp_list,Last_elem),Tmp_Elem is Last_elem*1.0001,Next_elem>Tmp_Elem,add(Next_elem,Tmp_list,Tmp1_list),
Index1 is Index+1,list_compression(Index1,Main_list,Tmp1_list,Result_list);
Index1 is Index+1,list_compression(Index1,Main_list,Tmp_list,Result_list).
first([S|H],D):-D is S,!.
add_increase(Cur_elem,H,[T1]):-length(H,Length),Length=1,T1 is H+Cur_elem.
add_increase(Cur_elem,[H|T],[H1|T1]):-add_increase(Cur_elem,T,T1),H1 is H+Cur_elem.
down0_delete([],_,[]).
down0_delete([H|T],Elem,[H|Tail]):-H>=Elem,down0_delete(T,Elem,Tail).
down0_delete([H|T],Elem,Tail):-H<Elem,down0_delete(T,Elem,Tail).
dellast([_],[]):-!.
dellast([X|T],[X|Y]):-dellast(T,Y).
del_duplicates([],[]):-!.
del_duplicates([X|Xs],Ys):-member(X,Xs),!,del_duplicates(Xs,Ys).
del_duplicates([X|Xs],[X|Ys]):-!,del_duplicates(Xs,Ys).
freq([A|B],A,X):-!,freq(B,A,Y),X is Y+1.
freq([_|B],C,X):-freq(B,C,X).
freq([],_,0).
deg(X,Y,Res):-deg1(X,Y,1,Res).
deg1(_,0,Res,Res):-!.
deg1(X,Y,C,Res):-C1 is C*X,Y1 is Y-1,deg1(X,Y1,C1,Res).
degree(_,0,1):-!.
degree(0,_,0):-!.
degree(X,Y,Res):-Y>0,!,deg(X,Y,Res).
degree(X,Y,Res):-Y1 is Y*(-1),deg(X,Y1,Res1),Res is 1/Res1.






