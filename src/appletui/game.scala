package appletui
import java.io._
import java.awt._, java.awt.event._, javax.swing._, javax.swing.border._
import java.awt.image._, javax.imageio._


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
    val font = Font.createFont(Font.TRUETYPE_FONT, res).deriveFont(24.0f)

    val genv = GraphicsEnvironment.getLocalGraphicsEnvironment()
    genv.registerFont(font)

    font
  }
  lazy val titleFont = defaultFont.deriveFont(32.0f)

  val TRANSPARENT = new Color(0, 0, 0, 0)

  lazy val awakeImage =
      ImageIO.read(this.getClass.getResourceAsStream("/awake.png"))
  lazy val dreamingImage =
      ImageIO.read(this.getClass.getResourceAsStream("/dreaming.png"))
}

object GameUtil {

  def spawn(name: String)(code: =>Unit) {
    new Thread(name) {
      override def run() {
        try { code }
        catch { case e@_ =>
          e.printStackTrace()
        }
      }
    }.start()
  }

  def swingThread(code: =>Unit) {
    SwingUtilities.invokeLater(new Runnable() {
      override def run() {
        try { code }
        catch { case e@_ =>
          e.printStackTrace()
        }
      }
    })
  }

}

class GameLabel(
    text: String,
    font: Font=GameConstants.defaultFont
    ) extends JLabel(text) {
  setForeground(Color.WHITE)
  setFont(font)
  putClientProperty(JEditorPane.HONOR_DISPLAY_PROPERTIES, true)

  def htmlColorCode(color: Color) =
    "#%02x%02x%02x" format (color.getRed, color.getGreen, color.getBlue)

  def html(text: String) {
    setText(GameLabel.surroundWithHtml(text))
  }
  def colored(color: Color, text: String) {
    setText("<html><font color=\""+htmlColorCode(color)+"\">"+text+
        "</font></html>")
  }
}

object GameLabel {
  def surroundWithHtml(html: String): String = """<html>
      <head><style type="text/css">
        body {
          font-family: Beeb;
        }
        p {
          padding-top: 16pt;
        }
      </style></head>
      <body>
        """+html+"""
      </body></html>"""

  def withHtml(html: String): GameLabel = {
    new GameLabel(surroundWithHtml(html))
  }
}

class ImagePanel(
    private var _imageSize: (Int,Int),
    private var _image: BufferedImage
  ) extends GamePanel {

  def imageSize = _imageSize
  def image = _image

  def imageSize_=(sz: (Int,Int)) {
    _imageSize = sz
    repaint()
  }

  def image_=(img: BufferedImage) {
    _image = img
    repaint()
  }
  
  override def paint(g: Graphics) {
    super.paint(g)
    if (image != null) {
      val x = (getWidth-imageSize._1)/2
      val y = (getHeight-imageSize._2)/2
      g.drawImage(image, x, y, imageSize._1, imageSize._2, null)
    }
  }

  override def getMinimumSize() = new Dimension(imageSize._1, imageSize._2)
  override def getPreferredSize() = getSize()
}

class GameField(font: Font=GameConstants.defaultFont) extends JTextField {
  setOpaque(false)
  setBackground(GameConstants.TRANSPARENT)
  setForeground(Color.WHITE)
  setFont(font)

  import javax.swing.text._

  setCaret(new DefaultCaret {
    // The following is lifted from:
    //  http://www.java2s.com/Code/Java/Swing-JFC/Fanciercustomcaretclass.htm

    override protected def damage(r: Rectangle) { this.synchronized {
      if (r == null)
        return

      // give values to x,y,width,height (inherited from java.awt.Rectangle)
      x = r.x
      y = r.y
      height = r.height
      // A value for width was probably set by paint(), which we leave alone.
      // But the first call to damage() precedes the first call to paint(), so
      // in this case we must be prepared to set a valid width, or else
      // paint()
      // will receive a bogus clip area and caret will not get drawn properly.
      if (width <= 0)
        width = getComponent().getWidth()

      repaint() // calls getComponent().repaint(x, y, width, height)
    }}
    override def paint(g0: Graphics) {
      val comp = getComponent
      if (comp == null || !comp.hasFocus || !comp.isEditable) return

      val g = g0.create()

      try {

        val dot = getDot
        var r: Rectangle = null
        var dotChar = '\0'
        try {
          r = comp.modelToView(dot)
          if (r == null) return
          dotChar = comp.getText(dot, 1).charAt(0)
        } catch {
          case e: BadLocationException =>
            return
        }
        if ("\r\n\t" contains dotChar) dotChar = 'l'

        if ((x != r.x) || (y != r.y)) {
          // paint() has been called directly, without a previous call to
          // damage(), so do some cleanup. (This happens, for example, when
          // the
          // text component is resized.)
          repaint() // erase previous location of caret
          x = r.x // Update dimensions (width gets set later in this method)
          y = r.y
          height = r.height
        }

        g.setColor(Color.WHITE)
        g.setXORMode(comp.getBackground()) // do this to draw in XOR mode

        width = g.getFontMetrics().charWidth(dotChar)
        if (isVisible())
          g.fillRect(r.x, r.y, width, r.height)
      } finally {
        g.dispose()
      }
    }
  })

  override def setBorder(border: Border) {}
}

class GamePanel(layout: LayoutManager=null) extends JPanel(layout) {
  setOpaque(false)
  setBackground(GameConstants.TRANSPARENT)
}

class TitleState(game: GameContainer) extends GameState(game) {

  setLayout(new BorderLayout)

  val titlePanel = new GamePanel(new BorderLayout)
  add(titlePanel, BorderLayout.NORTH)
  val titleLbl = new GameLabel("DREAMER OF ELECTRIC SHEEP",
      GameConstants.titleFont)
  titleLbl.setForeground(Color.CYAN)
  titleLbl.setHorizontalAlignment(SwingConstants.CENTER)
  titleLbl.setBorder(BorderFactory.createEmptyBorder(50,50,20,50))
  titlePanel.add(titleLbl, BorderLayout.CENTER)
  val subtitleLbl = GameLabel.withHtml(
      "<center><p>by Tom Coxon</p><p>#procjam 2014</p></center>")
  subtitleLbl.setHorizontalAlignment(SwingConstants.CENTER)
  subtitleLbl.setBorder(BorderFactory.createEmptyBorder(10,10,10,10))
  titlePanel.add(subtitleLbl, BorderLayout.SOUTH)

  val faceImage = new ImagePanel((300, 264), GameConstants.dreamingImage)
  add(faceImage, BorderLayout.CENTER)
  
  val clickLbl = new GameLabel("CLICK TO GIVE FOCUS", GameConstants.titleFont)
  clickLbl.setForeground(Color.GREEN)
  clickLbl.setHorizontalAlignment(SwingConstants.CENTER)
  clickLbl.setBorder(BorderFactory.createEmptyBorder(50,50,50,50))
  add(clickLbl, BorderLayout.SOUTH)

  addMouseListener(new MouseAdapter {
    override def mouseClicked(e: MouseEvent) {
      game.state = new MainGameState(game)
    }
  })

  setFocusable(true)
  requestFocus()
  addKeyListener(new KeyAdapter {
    override def keyTyped(e: KeyEvent) {
      game.state = new MainGameState(game)
    }
  })
}

