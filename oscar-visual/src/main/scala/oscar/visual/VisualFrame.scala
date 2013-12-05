/**
 * *****************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 * ****************************************************************************
 */
package oscar.visual;

import java.awt.Dimension
import java.awt.event._
import javax.swing._
import scala.swing.BoxPanel
import scala.swing.Orientation
import java.awt.Color
import java.awt.Toolkit
import java.awt.BorderLayout

class VisualFrame(title: String, nbLines: Int = 1, nbCols: Int = 1) extends JFrame(title) {

  val desktop = new JDesktopPane()

  desktop.setBackground(Color.white)
  val sz = Toolkit.getDefaultToolkit().getScreenSize()
  val screenSize = new Dimension(sz.getWidth().toInt, (sz.getHeight() * 85 / 100).toInt)
  var n = 0
  var w = (screenSize.width / nbCols) * 90 / 100
  var h = (screenSize.height / nbLines) * 90 / 100

  val content = getContentPane()
  content.setBackground(Color.white)
  content.add(new JScrollPane(desktop))
  setSize(screenSize)
  setMinimumSize(new Dimension(400, 300))
  setVisible(true)

  addWindowListener(new WindowAdapter() {
    override def windowClosing(event: WindowEvent) {
      System.exit(0);
    }
  })

  def createMenuBar(): JMenuBar = {
    val menuBar = new JMenuBar()
    val menu = new JMenu("Frame")
    menu.setMnemonic(KeyEvent.VK_N)
    val menuItem = new JMenuItem("New IFrame")
    menuItem.setMnemonic(KeyEvent.VK_N)
    menuItem.addActionListener(new ActionListener() {

      override def actionPerformed(e: ActionEvent) {
        createFrame("sub frame")
      }
    })
    menu.add(menuItem)
    menuBar.add(menu)
    menuBar
  }

  def createFrame(title: String): JInternalFrame = {
    val c = n % nbCols
    val l = n / nbCols

    val frame = new JInternalFrame(title, true, false, true, true)

    frame.setLocation(c * w, l * h)
    frame.setPreferredSize(new Dimension(w, h))
    frame.setSize(w, h)
    frame.setBackground(Color.white)
    frame.setVisible(true)
    desktop.add(frame)
    frame.moveToFront()
    n += 1
    frame
  }

  def createToolBar(withVisuController: Boolean = false): VisualToolBar = {
    val toolbar = new VisualToolBar(withVisuController)
    content.add(toolbar, BorderLayout.NORTH)
    toolbar
  }

}

object VisualFrame {
  
  def apply(title: String, nbLines: Int = 2, nbCols: Int = 2): VisualFrame = {
    new VisualFrame(title, nbLines, nbCols)
  }
}

object VisualFrameExample extends App {
  val frame = new VisualFrame("My Frame", 3, 2)
  for (i <- 0 until 6) frame.createFrame("My Sub-frame " + i)
  val tools = frame.createToolBar()
  tools.addButton("toto", println("hello"))
}
