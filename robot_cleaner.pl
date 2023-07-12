:- use_module(library(statistics)).


:- dynamic distanciasujeirainicio/2.
:- dynamic distanciasujeirafim/2.
:- dynamic distanciasujeirasujeira/3.
:- dynamic distanciasujeiraponto/2.
:- dynamic estadorobo/1.
:- dynamic objetivo/1.

:- dynamic matriz/1.
matriz([
    [0, 2, 0, 0, 0, 0, 0, 0, 0, 0],
    [2, 0, 0, 1, 0, 2, 0, 0, 0, 0],
    [0, 2, 1, 0, 0, 1, 0, 0, 0, 0],
    [0, 0, 0, 2, 0, 2, 0, 0, 0, 0],
    [0, 0, 0, 2, 0, 0, 0, 2, 0, 0],
    [0, 2, 0, 2, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 2, 0, 2, 0, 0, 0, 0],
    [0, 2, 0, 2, 0, 2, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
]
).

posicao_vazia(X, Y) :-
    matriz(Matriz),
    nth1(X, Matriz, Linha),
    nth1(Y, Linha, 0).

posicao_sujeira(X, Y) :-
    matriz(Matriz),
    nth1(X, Matriz, Linha),
    nth1(Y, Linha, 2).

posicao_obstaculo(X, Y) :-
    matriz(Matriz),
    nth1(X, Matriz, Linha),
    nth1(Y, Linha, 1).

remover_sujeira(X, Y) :-
    retract(matriz(Matriz)),
    nth1(X, Matriz, Linha),
    replace(Y, Linha, 0, NovaLinha),
    replace(X, Matriz, NovaLinha, NovaMatriz),
    assertz(matriz(NovaMatriz)).

limpar_sujeira(X, Y) :-
    remover_sujeira(X, Y).

replace(Index, List, Element, Result) :-
    nth1(Index, List, _, Temp),
    nth1(Index, Result, Element, Temp).

distancia_manhattan(X1, Y1, X2, Y2, Distancia) :-
    Difx is abs(X2 - X1),
    Dify is abs(Y2 - Y1),
    Distancia is Difx + Dify.

distancia_manhattan_AUX(N1, N2, Distancia) :-
    matriz(M),
    length(M, L),
    X1 is ((N1-1) mod L),
    Y1 is (N1 // L) + 1,
    X2 is ((N2-1) mod L),
    Y2 is (N2 // L) + 1,
    distancia_manhattan(X1, Y1, X2, Y2, Distancia).
    

calcular_distancias_sujeira_sujeira :-
    findall(_, (posicao_sujeira(X1, Y1), posicao_sujeira(X2, Y2), (X1 =\= X2 ; Y1 =\= Y2), \+ distanciasujeirasujeira((X2, Y2), (X1, Y1), _), \+ distanciasujeirasujeira((X1, Y1), (X2, Y2), _), cria_distancias_sujeira_sujeira(X1, Y1, X2, Y2)), _).

cria_distancias_sujeira_sujeira(X1, Y1, X2, Y2) :-
    distancia_manhattan(X1, Y1, X2, Y2, Distancia),
    assertz(distanciasujeirasujeira((X1, Y1), (X2, Y2), Distancia)).

calcular_distancias_sujeira_inicio(XInicial, YInicial) :-
    findall(_, (posicao_sujeira(X, Y), cria_distancias_sujeira_inicio(XInicial, YInicial, X, Y)), _).

cria_distancias_sujeira_inicio(XInicial, YInicial, X, Y) :-
    distancia_manhattan(XInicial, YInicial, X, Y, Distancia),
    assertz(distanciasujeirainicio((X, Y), Distancia)).

calcular_distancias_sujeira_fim(Xfinal, Yfinal) :-
    findall(_, (posicao_sujeira(X, Y), cria_distancias_sujeira_fim(Xfinal, Yfinal, X, Y)), _).

cria_distancias_sujeira_fim(Xfinal, Yfinal, X, Y) :-
    distancia_manhattan(Xfinal, Yfinal, X, Y, Distancia),
    assertz(distanciasujeirafim((X, Y), Distancia)).

calcular_distancias_sujeira_tudo((Xinicio, Yinicio), (Xfim, Yfim)) :-
    calcular_distancias_sujeira_sujeira,
    calcular_distancias_sujeira_inicio(Xinicio, Yinicio),
    calcular_distancias_sujeira_fim(Xfim, Yfim).

imprimir_base_fatos :-
    listing(matriz),
    listing(estadorobo),
    true.

descobre_sujeira_mais_proximo_do_ponto_atual((XAtual, YAtual), XDaSujeira, YDaSujeira) :-
    findall(Distancia, (posicao_sujeira(X, Y), distancia_manhattan(XAtual, YAtual, X, Y, Distancia), assertz(distanciasujeiraponto((X, Y), Distancia))), Distancias),
    min_list(Distancias, MenorDistancia),
    distanciasujeiraponto((XDaSujeira, YDaSujeira), MenorDistancia),
    retractall(distanciasujeiraponto(_,_)).

guloso_best_first(_, (XFinal, YFinal)) :-
    \+ posicao_sujeira(_, _),
    estadorobo(EstadoAtual),
    gerar_identificador(XFinal, YFinal, IdentificadorPontoFinal),
    assertz(objetivo(IdentificadorPontoFinal)),
    bestFirst([[EstadoAtual]], Solucao),
    movimenta_robo(Solucao),
    !.

guloso_best_first((XInicial, YInicial), (XFinal, YFinal)) :-
    descobre_sujeira_mais_proximo_do_ponto_atual((XInicial, YInicial), XDaSujeira, YDaSujeira),
    gerar_identificador(XDaSujeira, YDaSujeira, IdentificadorObjetivo),
    assertz(objetivo(IdentificadorObjetivo)),
    gerar_identificador(XInicial, YInicial, IdentificadorPontoAtual),
    bestFirst([[IdentificadorPontoAtual]], Solucao),
    movimenta_robo(Solucao),
    limpar_sujeira(XDaSujeira, YDaSujeira),
    retractall(objetivo(_)),
    guloso_best_first((XDaSujeira, YDaSujeira), (XFinal, YFinal)).

mede_guloso((XInicial, YInicial), (XFinal, YFinal)) :-
    statistics(runtime, [TempoInicial|_]),
    statistics(memory, [MemoriaInicial|_]),
    guloso_a_estrela((XInicial, YInicial), (XFinal, YFinal)),
    statistics(memory, [MemoriaFinal|_]),
    MemoriaUtilizada is MemoriaFinal - MemoriaInicial,
    statistics(runtime, [TempoFinal|_]),
    TempoTotal is TempoFinal - TempoInicial,
    format('Tempo de CPU: ~3f segundos~n', [TempoTotal]),
    format('Memória utilizada: ~w bytes~n', [MemoriaUtilizada]).
    

guloso_hill_climb(_, (XFinal, YFinal)) :-
    \+ posicao_sujeira(_, _),
    estadorobo(EstadoAtual),
    gerar_identificador(XFinal, YFinal, IdentificadorPontoFinal),
    assertz(objetivo(IdentificadorPontoFinal)),
    hillClimb([[EstadoAtual]], Solucao),
    movimenta_robo(Solucao),
    !.

guloso_hill_climb((XInicial, YInicial), (XFinal, YFinal)) :-
    descobre_sujeira_mais_proximo_do_ponto_atual((XInicial, YInicial), XDaSujeira, YDaSujeira),
    gerar_identificador(XDaSujeira, YDaSujeira, IdentificadorObjetivo),
    assertz(objetivo(IdentificadorObjetivo)),
    gerar_identificador(XInicial, YInicial, IdentificadorPontoAtual),
    hillClimb([[IdentificadorPontoAtual]], Solucao),
    movimenta_robo(Solucao),
    limpar_sujeira(XDaSujeira, YDaSujeira),
    retractall(objetivo(_)),
    guloso_hill_climb((XDaSujeira, YDaSujeira), (XFinal, YFinal)).

guloso_branch_and_bound(_, (XFinal, YFinal)) :-
    \+ posicao_sujeira(_, _),
    estadorobo(EstadoAtual),
    gerar_identificador(XFinal, YFinal, IdentificadorPontoFinal),
    assertz(objetivo(IdentificadorPontoFinal)),
    branchAndBound([[EstadoAtual]], Solucao),
    movimenta_robo(Solucao),
    !.

guloso_branch_and_bound((XInicial, YInicial), (XFinal, YFinal)) :-
    descobre_sujeira_mais_proximo_do_ponto_atual((XInicial, YInicial), XDaSujeira, YDaSujeira),
    gerar_identificador(XDaSujeira, YDaSujeira, IdentificadorObjetivo),
    assertz(objetivo(IdentificadorObjetivo)),
    gerar_identificador(XInicial, YInicial, IdentificadorPontoAtual),
    branchAndBound([[IdentificadorPontoAtual]], Solucao),
    movimenta_robo(Solucao),
    limpar_sujeira(XDaSujeira, YDaSujeira),
    retractall(objetivo(_)),
    guloso_hill_climb((XDaSujeira, YDaSujeira), (XFinal, YFinal)).

guloso_a_estrela(_, (XFinal, YFinal)) :-
    \+ posicao_sujeira(_, _),
    estadorobo(EstadoAtual),
    gerar_identificador(XFinal, YFinal, IdentificadorPontoFinal),
    assertz(objetivo(IdentificadorPontoFinal)),
    aEstrela([[EstadoAtual]], Solucao),
    movimenta_robo(Solucao),
    !.

guloso_a_estrela((XInicial, YInicial), (XFinal, YFinal)) :-
    descobre_sujeira_mais_proximo_do_ponto_atual((XInicial, YInicial), XDaSujeira, YDaSujeira),
    gerar_identificador(XDaSujeira, YDaSujeira, IdentificadorObjetivo),
    assertz(objetivo(IdentificadorObjetivo)),
    gerar_identificador(XInicial, YInicial, IdentificadorPontoAtual),
    aEstrela([[IdentificadorPontoAtual]], Solucao),
    movimenta_robo(Solucao),
    limpar_sujeira(XDaSujeira, YDaSujeira),
    retractall(objetivo(_)),
    guloso_hill_climb((XDaSujeira, YDaSujeira), (XFinal, YFinal)).

movimenta_robo([]) :-
    !.

movimenta_robo([No | ProximosNos]) :-
    retractall(estadorobo(_)),
    assertz(estadorobo(No)),
    movimenta_robo(ProximosNos).


gerar_identificador(X, Y, Identificador) :-
    matriz(Matriz),
    nth1(X, Matriz, Linha),
    length(Linha, TamanhoLinha),
    Identificador is (X - 1) * TamanhoLinha + Y.

coordenadas_ponto(Identificador, X, Y) :-
    matriz(Matriz),
    length(Matriz, TamanhoLinha),
    X is (Identificador - 1) mod TamanhoLinha + 1,
    Y is (Identificador - 1) // TamanhoLinha + 1.


%Montando grafo de vizinhanças a partir da matriz
% X1 e X2, indices do elemento na linha
%N1 e N2, identificadores do espaço da matriz
horizontal(X1, X2, N1, N2) :- 
    matriz(M), 
    nth1(IndiceLinha, M, Linha),
    nth1(X1, Linha, E1), 
    X2 is X1 - 1, 
    nth1(X2, Linha, E2),
    E1 =\= 1, %Não pode ser obstaculo
    E2 =\= 1, %Não pode ser obstaculo
    length(Linha, L),
    N1 is X1 + L * (IndiceLinha - 1),
    N2 is X2 + L * (IndiceLinha - 1).


horizontal(X1, X2, N1, N2) :- 
    matriz(M), 
    nth1(IndiceLinha, M, Linha), 
    nth1(X1, Linha, E1), 
    X2 is X1 + 1, 
    nth1(X2, Linha, E2),
    E1 =\= 1, %Não pode ser obstaculo
    E2 =\= 1, %Não pode ser obstaculo
    length(Linha, L),
    N1 is X1 + L * (IndiceLinha - 1),
    N2 is X2 + L * (IndiceLinha - 1).

% X1 e X2, indices do elemento na linha
%N1 e N2, identificadores do espaço da matriz
vertical(X1, X2, N1, N2) :- 
    matriz(M), 
    nth1(IndiceLinha, M, Linha),
    IndiceLinha2 is IndiceLinha - 1,
    nth1(IndiceLinha2, M, Linha2), 
    nth1(X1, Linha, E1),
    X2 is X1,
    nth1(X2, Linha2, E2),
    E1 =\= 1, %Não pode ser obstaculo
    E2 =\= 1, %Não pode ser obstaculo
    length(Linha, L),
    N1 is X1 + L * (IndiceLinha - 1),
    N2 is X2 + L * (IndiceLinha2 - 1).

vertical(X1, X2, N1, N2) :- 
    matriz(M), 
    nth1(IndiceLinha, M, Linha),
    IndiceLinha2 is IndiceLinha + 1,
    nth1(IndiceLinha2, M, Linha2), 
    nth1(X1, Linha, E1),
    X2 is X1,
    nth1(X2, Linha2, E2),
    E1 =\= 1, %Não pode ser obstaculo
    E2 =\= 1, %Não pode ser obstaculo
    length(Linha, L),
    N1 is X1 + L * (IndiceLinha - 1),
    N2 is X2 + L * (IndiceLinha2 - 1).

%Juntar os dois
vizinho_grade(N1, N2) :- horizontal(_, _, N1, N2).
vizinho_grade(N1, N2) :- vertical(_, _, N1, N2).

%Funções auxiliares
%modificado para conseguir se adaptar as mudanças de objetivo
maiorF([X |_], [Y |_]):-
    objetivo(O), %objetivo guardará próximo ponto que o robo quer ir (ex: próx sujeira para limpar)
    distancia_manhattan_AUX(X, O, F1),
    distancia_manhattan_AUX(Y, O, F2),
    F1 > F2.

concatena([],L,L).
concatena([X|L1],L,[X|L2]):-
          concatena(L1,L,L2).

ordenaF(Caminhos,CaminhosOrd):-
	quicksortF(Caminhos,CaminhosOrd).

particionarF(_,[],[],[]).
particionarF(X,[Y|Cauda],[Y|Menor],Maior):-
	maiorF(X,Y), !,
	particionarF(X,Cauda,Menor,Maior).
particionarF(X,[Y|Cauda],Menor,[Y|Maior]):-
	particionarF(X,Cauda,Menor,Maior).

quicksortF([],[]).
quicksortF([X|Cauda],ListaOrd):-
	particionarF(X,Cauda,Menor,Maior),
	quicksortF(Menor,MenorOrd),
	quicksortF(Maior,MaiorOrd),
	concatena(MenorOrd,[X|MaiorOrd],ListaOrd).



%Ordenar baseado no custo 
%Custo é apenas o tamanho do caminho, já que todas as operações valem o mesmo
maiorG(X, Y):-
    length(X, F1),
    length(Y, F2),
    F1 > F2.

ordenaG(Caminhos,CaminhosOrd):-
	quicksortG(Caminhos,CaminhosOrd).

particionarG(_,[],[],[]).
particionarG(X,[Y|Cauda],[Y|Menor],Maior):-
	maiorG(X,Y), !,
	particionarG(X,Cauda,Menor,Maior).
particionarG(X,[Y|Cauda],Menor,[Y|Maior]):-
	particionarG(X,Cauda,Menor,Maior).

quicksortG([],[]).
quicksortG([X|Cauda],ListaOrd):-
	particionarG(X,Cauda,Menor,Maior),
	quicksortG(Menor,MenorOrd),
	quicksortG(Maior,MaiorOrd),
	concatena(MenorOrd,[X|MaiorOrd],ListaOrd).

%Ordenar baseado no custo + Heuristica

maiorA([X|T], [Y|T]):-
    length([X|T], C1),
    length([Y|T], C2),
    objetivo(O), %objetivo guardará próximo ponto que o robo quer ir (ex: próx sujeira para limpar)
    distancia_manhattan_AUX(X, O, A1),
    distancia_manhattan_AUX(Y, O, A2),
    F1 is C1 + A1,
    F2 is C2 + A2,
    F1 > F2.

ordenaA(Caminhos,CaminhosOrd):-
	quicksortA(Caminhos,CaminhosOrd).

particionarA(_,[],[],[]).
particionarA(X,[Y|Cauda],[Y|Menor],Maior):-
	maiorA(X,Y), !,
	particionarA(X,Cauda,Menor,Maior).
particionarA(X,[Y|Cauda],Menor,[Y|Maior]):-
	particionarA(X,Cauda,Menor,Maior).

quicksortA([],[]).
quicksortA([X|Cauda],ListaOrd):-
	particionarA(X,Cauda,Menor,Maior),
	quicksortA(Menor,MenorOrd),
	quicksortA(Maior,MaiorOrd),
	concatena(MenorOrd,[X|MaiorOrd],ListaOrd).

estende([No|Caminho], NovosCaminhos) :-
	findall(
            [NovoNo,No|Caminho],
            (
                vizinho_grade(No, NovoNo),
                not(member(NovoNo,[No|Caminho]))
            ),
                NovosCaminhos
            ).


%%% Apenas para testar
%profundidade(Caminho, NoCorrente, Solucao):-
%	objetivo(NoCorrente),
%	reverse(Caminho,Solucao).

%profundidade(Caminho, NoCorrente, Solucao) :-
%	vizinho_grade(NoCorrente, NoNovo),				
%	not(member(NoNovo, Caminho)),                
%	profundidade([NoNovo|Caminho], NoNovo, Solucao).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Algoritmos de busca

bestFirst([[No|Caminho]|_],Solucao):-
	objetivo(No),
	reverse([No|Caminho],Solucao),
    !. % Pegar apenas o primeiro
bestFirst([Caminho|Caminhos], Solucao):-
	estende(Caminho, NovosCaminhos),
	ordenaF(NovosCaminhos, CaminhosOrd), %Ordenação pela função de avaliação
    concatena(CaminhosOrd, Caminhos, CaminhosTotal),
	bestFirst(CaminhosTotal, Solucao).

hillClimb([[No|Caminho]|_],Solucao):-	  				
	objetivo(No),                                   	
    reverse([No|Caminho],Solucao),
    !.

hillClimb([Caminho|Caminhos], Solucao) :-
	estende(Caminho, NovosCaminhos),
	concatena(Caminhos,NovosCaminhos,CaminhosTotal),
    ordenaF(CaminhosTotal,CaminhosTotOrd), %Ordenação pela função de avaliação
	hillClimb(CaminhosTotOrd, Solucao). 

branchAndBound([[No|Caminho]|_],Solucao):-	  				
	objetivo(No),                                   	
    reverse([No|Caminho],Solucao),
    !.

branchAndBound([Caminho|Caminhos], Solucao) :-
	estende(Caminho, NovosCaminhos),
	concatena(Caminhos,NovosCaminhos,CaminhosTotal),
    ordenaG(CaminhosTotal,CaminhosTotOrd), %Ordenação feita pelo custo agora
	branchAndBound(CaminhosTotOrd, Solucao).

aEstrela([[No|Caminho]|_],Solucao):-	 
	objetivo(No),                              
    reverse([No|Caminho],Solucao),
    !.

aEstrela([Caminho|Caminhos], Solucao) :-
	estende(Caminho, NovosCaminhos),
	concatena(Caminhos,NovosCaminhos, CaminhosTotal),
	ordenaA(CaminhosTotal,CaminhosTotOrd), %ordenação com custo + heuristica
	aEstrela(CaminhosTotOrd, Solucao). 	






   















