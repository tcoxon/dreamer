package appletui
import java.io._
import java.awt._, java.awt.event._, javax.swing._


class GameState(val game: GameContainer) extends JPanel {

  setPreferredSize(new Dimension(1024, 768))
  setMinimumSize(getPreferredSize)
  setBackground(Color.BLACK)

}

class GameContainer extends GameState(null) {

  override val game = this
  private var _state: GameState = null

  setPreferredSize(new Dimension(1024, 768))
  setMinimumSize(getPreferredSize)
  state = new TitleState(this)

  def state = _state
  def state_=(newState: GameState) = {
    if (_state != null) removeAll()
    _state = newState
    add(newState)
    revalidate()
    repaint()
    newState
  }

}


object GameConstants {
  lazy val defaultFont = {
    val res = this.getClass.getResourceAsStream("/Beeb.ttf")
    val font = Font.createFont(Font.TRUETYPE_FONT, res).deriveFont(32.0f)

    val genv = GraphicsEnvironment.getLocalGraphicsEnvironment()
    genv.registerFont(font)

    font
  }

  val TRANSPARENT = new Color(0, 0, 0, 0)
}

class GameLabel(text: String) extends JLabel(text) {
  setForeground(Color.WHITE)
  setFont(GameConstants.defaultFont)
  putClientProperty(JEditorPane.HONOR_DISPLAY_PROPERTIES, true)
}

object GameLabel {
  def withHtml(html: String): GameLabel = {
    val fullHtml = """<html>
      <head><style type="text/css">
        body {
          font-family: Beeb;
          text-align: center;
        }
        p {
          padding: 3px;
        }
      </style></head>
      <body>
        """+html+"""
      </body></html>"""
    new GameLabel(fullHtml)
  }
}

class GamePanel(layout: LayoutManager=null) extends JPanel(layout) {
  setOpaque(false)
  setBackground(GameConstants.TRANSPARENT)
}

class TitleState(game: GameContainer) extends GameState(game) {

  setLayout(new BorderLayout)

  val titlePanel = new GamePanel(new BorderLayout)
  add(titlePanel, BorderLayout.NORTH)
  val titleLbl = new GameLabel("DREAMER OF ELECTRIC SHEEP")
  titleLbl.setHorizontalAlignment(SwingConstants.CENTER)
  titleLbl.setBorder(BorderFactory.createEmptyBorder(50,50,20,50))
  titlePanel.add(titleLbl, BorderLayout.CENTER)
  val subtitleLbl = GameLabel.withHtml("<p>by Tom Coxon</p><p>#procjam 2014</p>")
  subtitleLbl.setHorizontalAlignment(SwingConstants.CENTER)
  subtitleLbl.setBorder(BorderFactory.createEmptyBorder(10,10,10,10))
  titlePanel.add(subtitleLbl, BorderLayout.SOUTH)
  
  val clickLbl = new GameLabel("CLICK TO GIVE FOCUS")
  clickLbl.setHorizontalAlignment(SwingConstants.CENTER)
  clickLbl.setBorder(BorderFactory.createEmptyBorder(50,50,50,50))
  add(clickLbl, BorderLayout.SOUTH)

  addMouseListener(new MouseAdapter {
    override def mouseClicked(e: MouseEvent) {
      println("Switch state")
      game.state = new MainGameState(game)
    }
  })
}

class MainGameState(game: GameContainer) extends GameState(game) {
  setLayout(new BorderLayout)
  add(new GameLabel("Foobar"), BorderLayout.NORTH)
}
