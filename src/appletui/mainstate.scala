package appletui
import java.awt._, java.awt.event._, javax.swing._
import scala.collection.immutable.List


class MainGameState(game: GameContainer) extends GameState(game) {
  setLayout(new BorderLayout)

  val responseLbl = new GameLabel("")
  val inputPanel = new GamePanel(new BorderLayout)
  val outputPanel = new GamePanel(new BorderLayout)
  val promptLbl = new GameLabel("> ")
  promptLbl.setForeground(Color.GREEN)
  val inputBox = new GameField()
  inputBox.setText("--")
  inputBox.requestFocus()
  outputPanel.add(responseLbl, BorderLayout.SOUTH)
  inputPanel.add(promptLbl, BorderLayout.WEST)
  inputPanel.add(inputBox, BorderLayout.CENTER)
  add(inputPanel, BorderLayout.SOUTH)
  add(outputPanel, BorderLayout.CENTER)

  responseLbl.setBorder(BorderFactory.createEmptyBorder(0, 50, 0, 50))
  inputPanel.setBorder(BorderFactory.createEmptyBorder(50,50,50,50))

  inputBox.setAction(new AbstractAction {
    override def actionPerformed(e: ActionEvent) {
      submit(inputBox.getText())
    }
  })

  val faceImage = new ImagePanel((300, 264), GameConstants.dreamingImage)
  faceImage.setBorder(BorderFactory.createEmptyBorder(50,50,50,50))
  val facePanel = new GamePanel(new BorderLayout)
  facePanel.add(faceImage, BorderLayout.CENTER)
  outputPanel.add(facePanel, BorderLayout.CENTER)


  import dreamer.concept._, dreamer.context._, dreamer.conceptnet._
  import dreamer.conversation._, dreamer.lang.html_en._
  import GameUtil._

  var context = Context(MentalMap(upstream=Some(new ConceptNet)))
  val conversation = Conversation(new HTMLEnglish)

  var firstSubmit = true
  def submit(text: String) {
    inputBox.setEditable(false)
    inputBox.setForeground(Color.GRAY)
    responseLbl.colored(Color.CYAN, "Thinking...")

    spawn("Thinking") {
      val (newCtx, response) = conversation.query(text)(context)
      context = newCtx
      
      swingThread {
        inputBox.setForeground(Color.WHITE)
        inputBox.setEditable(true)
        inputBox.requestFocus()
        updateDisplay(
          response=Some(response),
          awake=Some(Context.isAwake(context)))

        if (firstSubmit) {
          inputBox.setText("help")
          firstSubmit = false
        } else {
          inputBox.setText("")
        }
      }
    }
  }

  def updateDisplay(
      response: Option[String]=None,
      awake: Option[Boolean]=None) {

    response.map{x =>
      responseLbl.html(x)
    }

    awake.map{x =>
      faceImage.image = if (x) GameConstants.awakeImage
          else GameConstants.dreamingImage
    }
  }

  // start the game:
  submit("go to sleep")
}
