KŌNANE GAME
---

MENUS DE CONFIGURAÇÃO - GUI

---

Ao iniciar o jogo aparecem vários menus de configuração:

Escolha do tamanho do tabuleiro:

  
4x4
6x6
8x8
10x10

Escolha da dificuldade:

  
Fácil
Médio
Difícil

Escolha do tempo limite por jogada.

---

REGRAS DO JOGO

---

O jogador humano controla as peças pretas.
O computador controla as peças brancas.
Uma jogada consiste em saltar uma peça adversária.
Os movimentos são apenas horizontais ou verticais.
A peça adversária saltada é removida.
Se existirem mais capturas possíveis, o jogador pode continuar a jogar com a mesma peça.
O jogo termina quando um jogador deixa de ter jogadas válidas.

---

BOTÕES

---

Parar Capturas

Termina a sequência atual de capturas.

Desfazer Jogada (Undo)

Reverte a jogada anterior.

Reiniciar Jogo

Reinicia completamente o jogo.

---

GUI (INTERFACE GRÁFICA)

---

A interface gráfica foi desenvolvida em JavaFX.

A janela contém:

Indicador do jogador atual;
Cronómetro;
Tabuleiro do jogo;
Botões de controlo.

As peças são representadas por imagens:

Pretas → jogador humano;
Brancas → computador.

A peça selecionada é destacada visualmente.

---

ORGANIZAÇÃO DO CÓDIGO

---

O projeto está dividido em várias partes:

Game.scala

Contém as regras do jogo.
Validação de movimentos.
Capturas.
IA.

GameAPI.scala

Faz a ligação entre a lógica e a interface.
Controla turnos, seleção de peças e estado do jogo.

Controller.scala

Controla a interface gráfica.
Gere eventos, botões, cronómetro e renderização do tabuleiro.

Controller.fxml

Define a estrutura visual da interface.

FxApp.scala / Main

Inicia a aplicação JavaFX.