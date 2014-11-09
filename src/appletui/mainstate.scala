package appletui
import java.awt._, java.awt.event._, javax.swing._
import scala.collection.immutable.List


class MainGameState(game: GameContainer) extends GameState(game) {
  setLayout(new BorderLayout)

  val responseLbl = new GameLabel("")
  val inputPanel = new GamePanel(new BorderLayout)
  val promptLbl = new GameLabel("> ")
  val inputBox = new GameField()
  inputBox.setText("--")
  inputBox.requestFocus()
  for (x <- List(responseLbl, inputPanel)) {
    x.setBorder(BorderFactory.createEmptyBorder(50,50,50,50))
  }
  add(responseLbl, BorderLayout.CENTER)
  inputPanel.add(promptLbl, BorderLayout.WEST)
  inputPanel.add(inputBox, BorderLayout.CENTER)
  add(inputPanel, BorderLayout.SOUTH)

  inputBox.setAction(new AbstractAction {
    override def actionPerformed(e: ActionEvent) {
      submit(inputBox.getText())
    }
  })

  import dreamer.concept._, dreamer.context._, dreamer.conceptnet._
  import dreamer.conversation._, dreamer.lang.en._
  import GameUtil._

  var context = Context(MentalMap(upstream=Some(new ConceptNet)))
  val conversation = Conversation(new English)

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
      val x0 = x.replace(". ", ".&nbsp;&nbsp; ")
      val x1 = if (!x.startsWith("<html>")) "<html>"+x0+"</html>" else x0
      responseLbl.setText(x1)
    }
  }

  // start the game:
  submit("go to sleep")
}
