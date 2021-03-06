/*area(descri��o,Nome).*/
area(a,'Aldoar,Foz do Douro,Nevogilde').
area(b,'Ramalde').
area(c,'Lordelo do Ouro e Massarelos').
area(d,'Paranhos').
area(e,'Cedofeita').
area(f,'Bonfim').
area(g,'Campanha').

/*subarea(Nome,Area,PosX,PosY).*/

subarea(1,a,20,80).
subarea(2,a,50,30).
subarea(3,b,100,100).
subarea(4,b,90,70).
subarea(5,c,70,50).
subarea(6,c,110,50).
subarea(7,d,150,90).
subarea(8,d,200,90).
subarea(9,e,140,60).
subarea(10,e,160,20).
subarea(11,f,180,60).
subarea(12,f,190,20).
subarea(13,g,230,70).
subarea(14,g,240,30).

/*estrada(CidadeA,CidadeB,Distancia).*/
estrada(1,2,10).
estrada(1,3,100).
estrada(1,4,75).
estrada(1,5,65).
estrada(2,5,30).
estrada(3,4,10).
estrada(3,7,50).
estrada(4,5,25).
estrada(4,6,30).
estrada(4,9,65).
estrada(4,7,70).
estrada(5,6,10).
estrada(6,9,35).
estrada(6,10,60).
estrada(7,8,10).
estrada(7,9,30).
estrada(8,9,70).
estrada(8,11,40).
estrada(8,13,50).
estrada(9,10,10).
estrada(9,11,30).
estrada(10,11,50).
estrada(10,12,40).
estrada(11,12,10).
estrada(11,13,50).
estrada(11,14,60).
estrada(12,14,50).
estrada(13,14,10).

%cliente(NomeCliente,AreaResidencia)
cliente(1,ines,1).
cliente(2,dumitri,2).
cliente(3,nuno,3).
cliente(4,carvalho,4).
cliente(5,joao,5).
cliente(6,andre,6).
cliente(7,amorim,7).
cliente(8,rafa,8).
cliente(9,gon�alo,9).
cliente(10,catarina,10).
cliente(11,freitas,11).
cliente(12,ana,12).
cliente(13,cristiano,13).
cliente(14,joel,14).
cliente(15,tom�s,1).
cliente(16,diogo,2).
cliente(17,dinis,3).
cliente(18,martim,4).
cliente(19,afonso,5).
cliente(20,henrique,6).
cliente(21,jos�,7).
cliente(22,maria,8).
cliente(23,mariana,9).
cliente(24,marta,10).
cliente(25,francisco,11).
cliente(26,margarida,12).
cliente(27,bruno,13).
cliente(28,pedro,14).

% disponibilidade(Cliente,[Hora1,Hora2])
disponibilidade(1,[7,13]).
disponibilidade(2,[7,11]).
disponibilidade(3,[12,13]).
disponibilidade(4,[15,23]).
disponibilidade(5,[6,8]).
disponibilidade(6,[6,16]).
disponibilidade(7,[8,10]).
disponibilidade(8,[12,15]).
disponibilidade(9,[15,16]).
disponibilidade(10,[14,17]).
disponibilidade(11,[16,17]).
disponibilidade(12,[10,11]).
disponibilidade(13,[11,13]).
disponibilidade(14,[15,20]).
disponibilidade(15,[12,20]).
disponibilidade(16,[11,13]).
disponibilidade(17,[7,13]).
disponibilidade(18,[7,12]).
disponibilidade(19,[9,10]).
disponibilidade(20,[13,17]).
disponibilidade(21,[17,18]).
disponibilidade(22,[7,9]).
disponibilidade(23,[15,16]).
disponibilidade(24,[17,18]).
disponibilidade(25,[7,9]).
disponibilidade(26,[8,9]).
disponibilidade(27,[9,14]).
disponibilidade(28,[14,16]).


estimativa(C1,C2,Est):-
	subarea(C1,_,X1,Y1), subarea(C2,_,X2,Y2),
	DX is X1-X2,DY is Y1-Y2,
	Est is sqrt(DX*DX+DY*DY).

caminho(O,D,L):-
	caminho(O,D,[O],L).
caminho(O,O,_,[O]).
caminho(O,D,V,[O|L]):-
	liga(O,X),not(member(X,V)),
	caminho(X,D,[X|V],L).


/*1*/
caminho(O,L):-
	caminho(O,D,L),
	liga(D,O),
	todos_nos(Ln),
	length(Ln,Tn),
	length(L,Tn).

todos_nos(L1):-findall(X,liga(X,_),L),sort(L,L1).




% Principal
entrega_encomendas(ListaClientes,

astar(O,D,C,Tp):-
	estimativa(O,D,E),
	astar1([(E,E,0,[O])],D,L,Tp),
	reverse(L,C).

astar1([(_,_,Tp,[D|R])|_],D,[D|R],Tp).
astar1([(_,_,P,[X|R1])|R2],D,C,Tp):-
	findall((NovaSoma,E1,NP,[Z,X|R1]),(estrada(X,Z,V),
				   not(member(Z,R1)),
				   NP is P+V,
				   estimativa(Z,D,E1),
				   NovaSoma is E1+NP),L),
	append(R2,L,R3),
	sort(R3,R4),
	astar1(R4,D,C,Tp).
