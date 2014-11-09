import java.awt._
import javax.swing._
import appletui._


class GameApplet extends java.applet.Applet {

  override def init() {
    setLayout(new BorderLayout)
    add(new GameContainer, BorderLayout.CENTER)
  }

}

object AppletMain {

  def main(args: Array[String]) {
    val frame = new JFrame()
    val applet = new GameApplet()
    applet.init()
    frame.setTitle("Dreamer of Electric Sheep")
    frame.setLayout(new BorderLayout)
    frame.getContentPane().add(applet, BorderLayout.CENTER)
    frame.pack()
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setVisible(true)
    applet.start()
  }

}
