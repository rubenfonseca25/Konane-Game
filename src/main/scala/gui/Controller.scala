package gui

import logic.*
import logic.MyRandom
import logic.Game
import logic.Stone
import logic.Difficulty
import logic.Stone.*
import logic.Difficulty.*
import javafx.animation.{Animation, KeyFrame, Timeline}
import javafx.application.Platform
import javafx.event.ActionEvent
import javafx.fxml.FXML
import javafx.scene.control._
import javafx.scene.image.{Image, ImageView}
import javafx.scene.layout.GridPane
import javafx.util.Duration

import scala.jdk.CollectionConverters._

class Controller {

  @FXML var tabuleiro: GridPane = _

  @FXML var btnParar: Button = _
  @FXML var btnUndo: Button = _
  @FXML var btnReiniciar: Button = _

  @FXML var lblCronometro: Label = _

  @FXML var lblJogador: Label = _



  private var game: GameAPI = _

  private var tempoLimitePorJogada = 30
  private var tempoRestante = 30

  private var cronometro: Timeline = _


  @FXML
  def initialize(): Unit = {

    val opcoesTamanho = List("4x4", "6x6", "8x8", "10x10").asJava

    val dialogTamanho =
      new ChoiceDialog[String]("6x6", opcoesTamanho)

    dialogTamanho.setTitle("Configuração do Tabuleiro")
    dialogTamanho.setHeaderText("Escolha as proporções do tabuleiro de Kōnane:")
    dialogTamanho.setContentText("Dimensões:")

    val tamanhoEscolhido =
      dialogTamanho.showAndWait().orElse("6x6")

    val partes = tamanhoEscolhido.split("x")

    val linhas = partes(0).toInt
    val colunas = partes(1).toInt

    val opcoesDificuldade = List("1 - Fácil", "2 - Médio", "3 - Difícil").asJava

    val dialogDificuldade = new ChoiceDialog[String]("1 - Fácil", opcoesDificuldade)

    dialogDificuldade.setTitle("Configuração do nível de dificuldade")
    dialogDificuldade.setHeaderText("Escolha o nível de dificuldade do Computador:")
    dialogDificuldade.setContentText("Dificuldade:")

    val diffEscolhida = dialogDificuldade.showAndWait().orElse("1")

    val dificuldade = if (diffEscolhida.startsWith("3")) 3 else if (diffEscolhida.startsWith("2")) 2 else 1

    val dialogTempo = new TextInputDialog("30")

    dialogTempo.setTitle("Configuração do Cronómetro")
    dialogTempo.setHeaderText("Defina o tempo limite por jogada (em segundos):")

    dialogTempo.setContentText("Segundos:")

    val tempoIntroduzido = dialogTempo.showAndWait().orElse("30")

    try {
      tempoLimitePorJogada = tempoIntroduzido.toInt
    } catch {
      case _: NumberFormatException =>
        tempoLimitePorJogada = 30
    }

    game = new GameAPI(linhas, colunas, dificuldade)

    inicializarCronometro()

    renderBoard()

  }

  @FXML
  def handleParar(event: ActionEvent): Unit = {

    game.stopCapturing()

    renderBoard()

    if (game.currentPlayer == "WHITE") {
      ejecutarTurnoComputador()
    }
  }

  @FXML
  def handleUndo(event: ActionEvent): Unit = {

    if(game.undo()) {

      reiniciarCronometro()

      renderBoard()

    }

  }

  @FXML
  def handleReiniciar(event: ActionEvent): Unit = {

    game.reset()

    reiniciarCronometro()

    renderBoard()
  }

  private def inicializarCronometro(): Unit = {

    tempoRestante = tempoLimitePorJogada

    cronometro = new Timeline(
      new KeyFrame(
        Duration.seconds(1),
        _ => {
          tempoRestante -= 1

          atualizarTextoVisual()

          if (tempoRestante <= 0) {
            cronometro.stop()
            executarJogadaForcadaPorTempo()
          }
        }
      )
    )

    cronometro.setCycleCount(Animation.INDEFINITE)

    cronometro.play()
  }

  private def reiniciarCronometro(): Unit = {

    if (cronometro != null) {
      cronometro.stop()
    }

    tempoRestante = tempoLimitePorJogada

    atualizarTextoVisual()

    if (!game.isGameOver &&
      game.currentPlayer == "BLACK") {

      cronometro.play()
    }
  }

  private def atualizarTextoVisual(): Unit = {

    if (game.isGameOver) {

      lblCronometro.setText("Fim de Jogo!")

      return
    }

    if (game.currentPlayer == "BLACK") {

      lblCronometro.setText(
        s"Tempo Restante: ${tempoRestante}s"
      )
    }
  }

  private def executarJogadaForcadaPorTempo(): Unit = {

    if (game.currentPlayer == "BLACK") {

      game.jogarAleatorioHumano()

      renderBoard()

      if (!game.isGameOver && game.currentPlayer == "WHITE") {
        ejecutarTurnoComputador()
      }
    }
  }

  private def verificarFimDeJogo(): Unit = {

    if (game.isGameOver) {

      if (cronometro != null) {
        cronometro.stop()
      }

      Platform.runLater(() => {

        val alert =
          new Alert(Alert.AlertType.INFORMATION)

        alert.setTitle("Fim de Jogo")

        alert.setHeaderText(
          "O jogo Kōnane terminou!"
        )

        if (game.currentPlayer == "BLACK") {

          alert.setContentText(
            "Não tens mais jogadas disponíveis. O Computador ganhou!"
          )

        } else {

          alert.setContentText(
            "O computador ficou sem jogadas disponíveis. Parabéns, ganhaste!"
          )
        }

        alert.showAndWait()
      })
    }
  }

  private def renderBoard(): Unit = {

    tabuleiro.getChildren.clear()

    if (game.isMultiJumping) {

      btnParar.setVisible(true)

      btnParar.setText(
        "Parar Capturas (Passar Vez)"
      )

    } else {

      btnParar.setVisible(false)
    }

    atualizarTextoVisual()
    atualizarJogador()

    for (r <- 0 until game.getRows;
         c <- 0 until game.getCols) {

      val botao = new Button()

      botao.setPrefSize(70, 70)

      val stone = game.getStone(r, c)

      var imagePath = ""

      stone match {
        case "BLACK" => imagePath = "/black.png"
        case "WHITE" => imagePath = "/white.png"
        case _ =>
      }

      if (imagePath.nonEmpty) {

        val img =
          new Image(getClass.getResourceAsStream(imagePath))

        val view = new ImageView(img)

        view.setFitWidth(45)
        view.setFitHeight(45)

        botao.setGraphic(view)
      }

      if (game.isSelected(r, c)) {

        botao.setStyle(
          "-fx-background-color: #f1c40f;"
        )
      }

      val row = r
      val col = c



      botao.setOnAction(_ => {



        if (game.currentPlayer == "BLACK") {


          val jogadorAntes = game.currentPlayer

          val jogadaValida =
            game.select(row, col, game.getDifficulty)

          val jogadorDepois = game.currentPlayer


          if (jogadaValida) {



            renderBoard()

            if (jogadorAntes!=jogadorDepois) {
              reiniciarCronometro()
              if (jogadorDepois=="WHITE"){
                ejecutarTurnoComputador()
              }
            }
          }
        }
      })

      tabuleiro.add(botao, c, r)
    }

    verificarFimDeJogo()
  }

  private def ejecutarTurnoComputador(): Unit = {

    if (cronometro != null) {
      cronometro.stop()
    }

    val delay = new javafx.animation.PauseTransition(Duration.millis(1000))
    delay.setOnFinished(_ => {
      game.jogarComputador()
      reiniciarCronometro()
      renderBoard()
    })
    delay.play()

    renderBoard()
  }


  private def atualizarJogador(): Unit = {

    if (game.currentPlayer == "BLACK") {

      lblJogador.setText("Pretas a jogar")

      lblJogador.setStyle(
        """
          -fx-font-size: 18px;
          -fx-font-weight: bold;
          -fx-text-fill: white;
          -fx-background-color: black;
          -fx-padding: 5 15 5 15;
          -fx-background-radius: 5;
        """
      )

    } else {

      lblJogador.setText("Brancas a jogar")

      lblJogador.setStyle(
        """
          -fx-font-size: 18px;
          -fx-font-weight: bold;
          -fx-text-fill: black;
          -fx-background-color: white;
          -fx-padding: 5 15 5 15;
          -fx-background-radius: 5;
        """
      )
    }
  }

}