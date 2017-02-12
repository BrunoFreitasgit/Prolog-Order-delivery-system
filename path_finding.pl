%
% PESQUISA DE CAMINHOS PARA ENVIO DE ENCOMENDAS
%

:- use_module(library(date)).

% distancia(Cidade1, Cidade2, Transporte,Distancia)
% CARRO
distancia(amesterdam,paris,aviao,430).
distancia(berlin,paris,aviao,878).
distancia(copenhagen,helsinki,aviao,884).
distancia(copenhagen,oslo,aviao,484).
distancia(copenhagen,stockholm,aviao,523).
distancia(dublin,paris,aviao,836).
distancia(helsinki,copenhagen,aviao,884).
distancia(lisbon,madrid,aviao,503).
distancia(london,paris,aviao,344).
distancia(madrid,lisbon,aviao,503).
distancia(madrid,paris,aviao,1054).
distancia(oslo,copenhagen,aviao,484).
distancia(paris,amesterdam,aviao,430).
distancia(paris,berlin,aviao,878).
distancia(paris,dublin,aviao,836).
distancia(paris,london,aviao,344).
distancia(paris,madrid,aviao,1054).
distancia(paris,rome,aviao,1107).
distancia(paris,vienna,aviao,1035).
distancia(rome,paris,aviao,1107).
distancia(stockholm,copenhagen,aviao,523).
distancia(vienna,paris,aviao,1035).
% CARRO
distancia(amesterdam,brussels,carro,210).
distancia(amesterdam,paris,carro,501).
distancia(andorra,ljubljana,carro,1430).
distancia(andorra,madrid,carro,609).
distancia(andorra,paris,carro,861).
distancia(berlin,copenhagen,carro,356).
distancia(berlin,paris,carro,1048).
distancia(berlin,prague,carro,351).
distancia(berlin,tallin,carro,1042).
distancia(berlin,warsaw,carro,572).
distancia(bratislava,budapest,carro,200).
distancia(bratislava,vienna,carro,79).
distancia(brussels,amersterdam,carro,210).
distancia(budapest,berlin,carro,356).
distancia(copenhagen,berlin,carro,356).
distancia(copenhagen,oslo,carro,606).
distancia(copenhagen,stockholm,carro,657).
distancia(dublin,paris,carro,1083).
distancia(lisbon,madrid,carro,625).
distancia(lisbon,porto,carro,313).
distancia(ljubljana,andorra,carro,1430).
distancia(ljubljana,vienna,carro,384).
distancia(ljubljana,zagreb,carro,140).
distancia(london,paris,carro,464).
distancia(luxembourg,paris,carro,374).
distancia(madrid,andorra,carro,609).
distancia(madrid,lisbon,carro,625).
distancia(madrid,paris,carro,1276).
distancia(madrid,porto,carro,561).
distancia(oslo,copenhagen,carro,606).
distancia(oslo,stockholm,carro,521).
distancia(paris,amesterdam,carro,501).
distancia(paris,andorra,carro,861).
distancia(paris,berlin,carro,1048).
distancia(paris,brussels,carro,314).
distancia(paris,dublin,carro,1083).
distancia(paris,london,carro,464).
distancia(paris,luxembourg,carro,374).
distancia(paris,madrid,carro,1276).
distancia(paris,rome,carro,1424).
distancia(paris,vienna,carro,1236).
distancia(porto,lisbon,carro,313).
distancia(porto,madrid,carro,561).
distancia(prague,berlin,carro,351).
distancia(prague,vienna,carro,309).
distancia(riga,tallin,carro,309).
distancia(riga,vilnius,carro,302).
distancia(rome,paris,carro,1424).
distancia(tallin,riga,carro,309).
distancia(vienna,bratislava,carro,79).
distancia(vienna,ljubljana,carro,384).
distancia(vienna,paris,carro,1236).
distancia(vienna,prague,carro,309).
distancia(vilnius,riga,carro,302).
distancia(vilnius,warsaw,carro,459).
distancia(warsaw,vilnius,carro,459).
distancia(zabreg,ljubljana,carro,140).


% CONSTANTES
velocidade(aviao,500).
velocidade(carro,80).
preco(aviao,0.05).
preco(carro,0.01).


% disponibilidade(Transporte,DiaDaSemana,Hora)
disponibilidade(aviao,2,6).
disponibilidade(aviao,4,6).
disponibilidade(aviao,6,6).
disponibilidade(carro,1,7).
disponibilidade(carro,1,12).
disponibilidade(carro,2,7).
disponibilidade(carro,2,12).
disponibilidade(carro,3,7).
disponibilidade(carro,3,12).
disponibilidade(carro,4,7).
disponibilidade(carro,4,12).
disponibilidade(carro,5,7).
disponibilidade(carro,5,12).
disponibilidade(carro,6,7).
disponibilidade(carro,6,12).
disponibilidade(carro,7,7).
disponibilidade(carro,7,12).




% ******* PREDICADOS PRINCIPAIS *******
% *************************************
mais_barato(Origem,Destino,Peso,DiaInicial,MesInicial,AnoInicial,HoraInicial,MinutosInicial):-
	DiaInicial > 0, DiaInicial < 31, MesInicial > 0, MesInicial =< 12,
	HoraInicial < 24, MinutosInicial >= 0, MinutosInicial < 60,
	findall_caminho_mais_barato(Origem,Destino,Peso,ListaFinal),
	restricoes_tempo(ListaFinal,DiaInicial,MesInicial,AnoInicial,HoraInicial,MinutosInicial,0).


mais_rapido(Origem,Destino,Peso,DiaInicial,MesInicial,AnoInicial,HoraInicial,MinutosInicial):-
	DiaInicial > 0, DiaInicial < 31, MesInicial > 0, MesInicial =< 12,
	HoraInicial < 24, MinutosInicial >= 0, MinutosInicial < 60,
	findall_caminho_mais_rapido(Origem,Destino,Peso,ListaFinal),
	restricoes_tempo(ListaFinal,DiaInicial,MesInicial,AnoInicial,HoraInicial,MinutosInicial,1).


% ******* PERCURSOS MAIS BARATOS *******
% **************************************
findall_caminho_mais_barato(Origem,Destino,Peso,ListaFinal):-
	 findnsols(15,
	    (CustoTotal,TempoViagem,Caminho,Transportes),
	    (caminho_mais_barato(Origem,Destino,Peso,Caminho,Transportes,CustoTotal,TempoViagem)),
	    ListaFinal),!.


caminho_mais_barato(Origem,Destino,Peso,Caminho,Transportes,CustoTotal,TempoViagem):-
	inicializa_barato(Origem,Peso,ListaInicial),
	caminho_mais_barato2(ListaInicial,Destino,Peso,Caminho,Transportes,CustoTotal,TempoViagem).


caminho_mais_barato2([(CustoTotal,TempoViagem,[D|R],ListaTransportes)|_],D,_,Caminho,Transportes,CustoTotal,TempoViagem):-
	nl,
	reverse(ListaTransportes,Transportes),
	reverse([D|R],Caminho).


caminho_mais_barato2([(_,_,[D|_],_)|R2],D,Peso,Caminho,Transportes,CustoTotal,TempoViagem):- !,
	caminho_mais_barato2(R2,D,Peso,Caminho,Transportes,CustoTotal,TempoViagem).


caminho_mais_barato2([(Custo1,TempoAcumulado,[Cidade1|R1],[T|ListaTransportes])|R2],D,Peso,Caminho,Transportes,CustoTotal,TempoViagemTotal):-
	findall(
	    (Custo,TempoViagem,[Z,Cidade1|R1],[T2,T|ListaTransportes]),
	    (distancia(Cidade1,Z,T2,_),
	     not(member(Z,R1)),
	     custo(Cidade1,Z,T2,Peso,C),Custo is Custo1+C, calc_tempo_horas(Cidade1,Z,T2,Horas),TempoViagem is Horas + TempoAcumulado),
	    L),
	append(R2,L,R3),
	sort(R3,R4),
	caminho_mais_barato2(R4,D,Peso,Caminho,Transportes,CustoTotal,TempoViagemTotal).


inicializa_barato(Cidade1,Peso,L2):-
	findall(
	    (Custo,TempoViagem,[Z,Cidade1],[Transporte]),
	    (distancia(Cidade1,Z,Transporte,_), custo(Cidade1,Z,Transporte,Peso,Custo),calc_tempo_horas(Cidade1,Z,Transporte,TempoViagem)),
	    L),
	sort(L,L2).


% ******* PERCURSOS MAIS RÁPIDOS *******
% **************************************
findall_caminho_mais_rapido(Origem,Destino,Peso,ListaFinal):-
	 findnsols(15,
	    (TempoDeViagem,CustoTotal,Caminho,Transportes),
	    (caminho_mais_rapido(Origem,Destino,Peso,Caminho,Transportes,CustoTotal,TempoDeViagem)),
	    ListaFinal),!.


caminho_mais_rapido(Origem,Destino,Peso,Caminho,Transportes,CustoTotal,TempoDeViagem):-
	inicializa_rapido(Origem,Peso,ListaInicial),
	caminho_mais_rapido2(ListaInicial,Destino,Peso,Caminho,Transportes,CustoTotal,TempoDeViagem).


caminho_mais_rapido2([(TempoDeViagem,CustoTotal,[D|R],ListaTransportes)|_],D,_,Caminho,Transportes,CustoTotal,TempoDeViagem):-
	nl,
	reverse(ListaTransportes,Transportes),
	reverse([D|R],Caminho).


caminho_mais_rapido2([(_,_,[D|_],_)|R2],D,Peso,Caminho,Transportes,CustoTotal,TempoDeViagem):- !,
	caminho_mais_rapido2(R2,D,Peso,Caminho,Transportes,CustoTotal,TempoDeViagem).


caminho_mais_rapido2([(Tempo,Custo1,[Cidade1|R1],[T|ListaTransportes])|R2],D,Peso,Caminho,Transportes,CustoTotal,TempoDeViagem):-
	findall(
	    (TempoAcumulado,Custo,[Z,Cidade1|R1],[T2,T|ListaTransportes]),
	    (distancia(Cidade1,Z,T2,_),
	     not(member(Z,R1)),
	     custo(Cidade1,Z,T2,Peso,C),Custo is Custo1 + C,calc_tempo_horas(Cidade1,Z,T2,Tempo2), TempoAcumulado is Tempo2 + Tempo),
	    L),
	append(R2,L,R3),
	sort(R3,R4),
	caminho_mais_rapido2(R4,D,Peso,Caminho,Transportes,CustoTotal,TempoDeViagem).


inicializa_rapido(Cidade1,Peso,L2):-
	findall(
	    (TempoAcumulado,Custo,[Z,Cidade1],[Transporte]),
	    (distancia(Cidade1,Z,Transporte,_),
	     custo(Cidade1,Z,Transporte,Peso,Custo),
	     calc_tempo_horas(Cidade1,Z,Transporte,TempoAcumulado)),
	    L),
	sort(L,L2).


% ******* CALCULO HORA MAIS PRÓXIMA DISPONIVEL DE TRANSPORTE *******
% ******************************************************************
ate_proximo_aviao(HoraChegada,HorasAcumuladasTotal,Dia,Mes,Ano):-
	HoraChegada2 is round(HoraChegada),
	ate_proximo_aviao2(HoraChegada2,0,HorasAcumuladasTotal,Dia,Mes,Ano).


ate_proximo_aviao2(6,HorasAcumuladas,HorasAcumuladasTotal,Dia,Mes,Ano):-
	day_of_the_week(date(Ano,Mes,Dia),DiaDaSemana),
	disponibilidade(aviao,DiaDaSemana,6),
	HorasAcumuladasTotal is HorasAcumuladas.


ate_proximo_aviao2(6,HorasAcumuladas,HorasAcumuladasTotal,Dia,Mes,Ano):-
	HorasAcumuladasX is HorasAcumuladas + 24,
	DiaX is Dia+1,
	ate_proximo_aviao2(6,HorasAcumuladasX,HorasAcumuladasTotal,DiaX,Mes,Ano).


ate_proximo_aviao2(HoraChegada,HorasAcumuladas,HorasAcumuladasTotal,Dia,Mes,Ano):-
	HoraChegada >= 24, DiaX is Dia+1, HorasX is 1,HorasAcumuladasX is HorasAcumuladas + 1,!,
	ate_proximo_aviao2(HorasX,HorasAcumuladasX,HorasAcumuladasTotal,DiaX,Mes,Ano).


ate_proximo_aviao2(HoraChegada,HorasAcumuladas,HorasAcumuladasTotal,Dia,Mes,Ano):-
	HoraChegadaX is HoraChegada +1, HorasAcumuladasX is HorasAcumuladas + 1,!,
	ate_proximo_aviao2(HoraChegadaX,HorasAcumuladasX,HorasAcumuladasTotal,Dia,Mes,Ano).


ate_proximo_carro(HoraChegada,HorasAcumuladasTotal,Dia,Mes,Ano):-
	HoraChegada2 is round(HoraChegada),
	ate_proximo_carro2(HoraChegada2,0,HorasAcumuladasTotal,Dia,Mes,Ano).


ate_proximo_carro2(HoraChegada,HorasAcumuladas,HorasAcumuladasTotal,Dia,Mes,Ano):-
	day_of_the_week(date(Ano,Mes,Dia),DiaDaSemana),
	disponibilidade(carro,DiaDaSemana,HoraChegada),
	HorasAcumuladasTotal is HorasAcumuladas.


ate_proximo_carro2(HoraChegada,HorasAcumuladas,HorasAcumuladasTotal,Dia,Mes,Ano):-
	HoraChegada >= 24, DiaX is Dia+1, HorasX is 1,HorasAcumuladasX is HorasAcumuladas + 1,!,
	ate_proximo_carro2(HorasX,HorasAcumuladasX,HorasAcumuladasTotal,DiaX,Mes,Ano).


ate_proximo_carro2(HoraChegada,HorasAcumuladas,HorasAcumuladasTotal,Dia,Mes,Ano):-
	HoraChegadaX is HoraChegada+1, HorasAcumuladasX is HorasAcumuladas + 1,!,
	ate_proximo_carro2(HoraChegadaX,HorasAcumuladasX,HorasAcumuladasTotal,Dia,Mes,Ano).


% ******* PLANEAMENTO/RESTRIÇÕES DE TEMPO *******
% ***********************************************

restricoes_tempo([],_,_,_,_,_,_).
restricoes_tempo([(CustoFinal,TempoViagem,Caminho,Transportes)|RestoLista],DiaInicial,MesInicial,AnoInicial,HoraInicial,MinutosInicial,0):-
	restricoes_tempo1((CustoFinal,TempoViagem,Caminho,Transportes),DiaInicial,MesInicial,AnoInicial,HoraInicial,MinutosInicial,DiaEntrega,MesEntrega,AnoEntrega,HoraEntrega,MinutosEntrega),!,
	write('Custo Total(€) ='), write(CustoFinal),nl,
	write('Tempo de Viagem Acumulado(horas)='), write(TempoViagem),nl,
	write('Caminho = '), write(Caminho),nl,
	write('Transportes = '), write(Transportes),nl,
	write('Dia Inicial = '), write(DiaInicial), write('\t| Dia Entrega = ') ,write(DiaEntrega),nl,
	write('Mes Inicial = '), write(MesInicial), write('\t| Mes Entrega = ') ,write(MesEntrega),nl,
	write('Ano Inicial = '), write(AnoInicial), write('\t| Ano Entrega = ') ,write(AnoEntrega),nl,
	write('Hora Inicial = '), write(HoraInicial), write('\t| Hora Entrega = ') ,write(HoraEntrega),nl,
	write('Minutos Iniciais = '), write(MinutosInicial), write('\t| Minutos Entrega = ') ,write(MinutosEntrega),nl,
	write('###################################'),nl,
	restricoes_tempo(RestoLista,DiaInicial,MesInicial,AnoInicial,HoraInicial,MinutosInicial,0).


restricoes_tempo([(TempoViagem,CustoFinal,Caminho,Transportes)|RestoLista],DiaInicial,MesInicial,AnoInicial,HoraInicial,MinutosInicial,1):-
	restricoes_tempo1((CustoFinal,TempoViagem,Caminho,Transportes),DiaInicial,MesInicial,AnoInicial,HoraInicial,MinutosInicial,DiaEntrega,MesEntrega,AnoEntrega,HoraEntrega,MinutosEntrega),!,
	write('Custo Total(€) ='), write(CustoFinal),nl,
	write('Tempo de Viagem Acumulado(horas)='), write(TempoViagem),nl,
	write('Caminho = '), write(Caminho),nl,
	write('Transportes = '), write(Transportes),nl,
	write('Dia Inicial = '), write(DiaInicial), write('\t| Dia Entrega = ') ,write(DiaEntrega),nl,
	write('Mes Inicial = '), write(MesInicial), write('\t| Mes Entrega = ') ,write(MesEntrega),nl,
	write('Ano Inicial = '), write(AnoInicial), write('\t| Ano Entrega = ') ,write(AnoEntrega),nl,
	write('Hora Inicial = '), write(HoraInicial), write('\t| Hora Entrega = ') ,write(HoraEntrega),nl,
	write('Minutos Iniciais = '), write(MinutosInicial), write('\t| Minutos Entrega = ') ,write(MinutosEntrega),nl,
	write('###################################'),nl,
	restricoes_tempo(RestoLista,DiaInicial,MesInicial,AnoInicial,HoraInicial,MinutosInicial,1).


restricoes_tempo1((_,_,_,[]),DiaEntrega,MesEntrega,AnoEntrega,HoraEntrega,MinutosEntrega,DiaEntrega,MesEntrega,AnoEntrega,HoraEntrega,MinutosEntrega).


restricoes_tempo1((Custo1,Custo2,[Cidade1,Cidade2|R],[T1|RestoTransportes]),DiaChegada,MesChegada,AnoChegada,HoraChegada,MinutosChegada,DiaEntrega,MesEntrega,AnoEntrega,HoraEntrega,MinutosEntrega):-
	T1 == 'aviao',
	normalizar_minutos(MinutosChegada,HoraChegada,HoraFinal),
        ate_proximo_aviao(HoraFinal,HorasAcumuladasTotal,DiaChegada,MesChegada,AnoChegada),
        adicionar_horas_dia(HorasAcumuladasTotal,HoraFinal,DiaChegada,NovoDia,NovaHora),
        calc_tempo(Cidade1,Cidade2,T1,Horas,Minutos),
        adicionar_horas_dia(Horas,NovaHora,NovoDia,NovoDia2,NovaHora3),
	restricoes_tempo1((Custo1,Custo2,[Cidade2|R],RestoTransportes),NovoDia2,MesChegada,AnoChegada,NovaHora3,Minutos,DiaEntrega,MesEntrega,AnoEntrega,HoraEntrega,MinutosEntrega).


restricoes_tempo1((Custo1,Custo2,[Cidade1,Cidade2|R],[T1|RestoTransportes]),DiaChegada,MesChegada,AnoChegada,HoraChegada,MinutosChegada,DiaEntrega,MesEntrega,AnoEntrega,HoraEntrega,MinutosEntrega):-
	T1 == 'carro',
	normalizar_minutos(MinutosChegada,HoraChegada,HoraFinal),
	ate_proximo_carro(HoraFinal,HorasAcumuladasTotal,DiaChegada,MesChegada,AnoChegada),
	adicionar_horas_dia(HorasAcumuladasTotal,HoraFinal,DiaChegada,NovoDia,NovaHora),
	calc_tempo(Cidade1,Cidade2,T1,Horas,Minutos),
	adicionar_horas_dia(Horas,NovaHora,NovoDia,NovoDia2,NovaHora3),
	restricoes_tempo1((Custo1,Custo2,[Cidade2|R],RestoTransportes),NovoDia2,MesChegada,AnoChegada,NovaHora3,Minutos,DiaEntrega,MesEntrega,AnoEntrega,HoraEntrega,MinutosEntrega).


adicionar_horas_dia(HorasAdicionar,HoraInicial,DiaInicial,DiaX,NovaHora):-
	HorasAdicionarX is HoraInicial + HorasAdicionar,
	HorasAdicionarX >= 24, NovaHora is HorasAdicionarX-24, DiaX is DiaInicial+1,!.


adicionar_horas_dia(HorasAdicionar,HoraX,Dia,DiaX,NovaHora):-
	NovaHora is HoraX + HorasAdicionar,
	DiaX is Dia.


% ******* PREDICADOS AUXILIARES *********
% CALCULA TEMPO TRANSPORTE
% calc_tempo(Cidade1,Cidade2,TempoEmHoras,TempoEmMinutos)
calc_tempo(C1,C2,T,TH,TM):-
	 distancia(C1,C2,T,D),!,
	 velocidade(T,Vel),
	 Tempo is D/Vel,
	 TH is float_integer_part(Tempo),
	 TM is round(float_fractional_part(Tempo)*60).


% CALCULA TEMPO TRANSPORTE (HORAS)
% calc_tempo_horas(Cidade1,Cidade2,TempoEmHoras)
calc_tempo_horas(C1,C2,T,TH):-
	distancia(C1,C2,T,D),!,
	velocidade(T,Vel),
	TH is D/Vel.


% CALCULA CUSTO DE TRANSPORTE
% custo(Cidade1,Cidade2,Transporte,Preço)
custo(C1,C2,T,Peso,Preco):-
	distancia(C1,C2,T,D),!,
	preco(T,Custo),
	Preco is D*Peso*Custo.


normalizar_minutos(0,HoraChegada,HoraChegada):-!.

normalizar_minutos(_,HoraInicial,HoraFinal):-
	HoraFinal is HoraInicial +1.















