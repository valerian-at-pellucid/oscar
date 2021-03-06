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
package oscar.visual

import javax.swing.JPanel
import java.awt.Color
import java.awt.Shape
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.event.MouseMotionListener
import java.awt.event.MouseEvent
import oscar.visual.shapes.VisualLine
import oscar.visual.shapes.VisualRectangle
import oscar.visual.shapes.VisualShape
import scala.collection.mutable.Queue
import javax.swing.SwingUtilities
import java.awt.event.MouseListener
import java.awt.geom.AffineTransform
import java.awt.geom.Point2D

/**
 * VisualDrawing
 *
 *  Contains and draws VisualShapes.
 */
class VisualDrawing(flipped: Boolean, scalable: Boolean) extends JPanel {

  setBackground(Color.white)

  // Shapes contained in the panel
  protected val shapes: Queue[VisualShape] = Queue()

  protected var marginT: Double = 0
  protected var marginR: Double = 0
  protected var marginB: Double = 0
  protected var marginL: Double = 0

  /** Returns the margins of the panel. */
  def margin = (marginT, marginR, marginB, marginL)

  /** Sets the margins of the panel. */
  def margin(m: Double): Unit = margin(m, m, m, m)

  /** Sets the margins of the panel. */
  def margin(top: Double, right: Double, bottom: Double, left: Double): Unit = {
    marginT = top
    marginR = right
    marginB = bottom
    marginL = left
  }

  // Returns the bounds of the bounding box containing all the shapes.
  protected def findBounds(shapes: Iterable[VisualShape]): (Double, Double, Double, Double) = {
    var minX = Double.MaxValue
    var maxX = Double.MinValue
    var minY = Double.MaxValue
    var maxY = Double.MinValue
    for (shape <- shapes) {
      val bounds = shape.getBounds
      if (bounds._1 < minX) minX = bounds._1
      if (bounds._2 > maxX) maxX = bounds._2
      if (bounds._3 < minY) minY = bounds._3
      if (bounds._4 > maxY) maxY = bounds._4
    }
    (minX, maxX, minY, maxY)
  }
  var transform = new AffineTransform()
  var scale = 1.0
  
  override def paint(g: Graphics): Unit = {
	
    super.paintComponent(g)
    val g2d = g.asInstanceOf[Graphics2D]
    transform = new AffineTransform() // start with identity transform   
    
    if (!shapes.isEmpty) {
      
      // Shapes size
      val (minX, maxX, minY, maxY) = findBounds(shapes)
      val sWidth = maxX - minX
      val sHeight = maxY - minY

      // Drawing size
      val dWidth = getWidth()
      val dHeight = getHeight()
      
      // Flip
      if (flipped) {
        transform.translate(0, dHeight)
        transform.scale(1*scale, -1*scale)
      } else {
        transform.scale(1*scale, 1*scale)
      }

      // Scale
      if (scalable) {
        // Compute the scaling ratio
        val ratioX = dWidth / (marginR + marginL + sWidth)
        val ratioY = dHeight / (marginT + marginB + sHeight)
        val ratio = math.min(ratioX, ratioY) // Maintain proportions
        transform.scale(ratio, ratio)
        
        // Translate
        val translateX: Int = (marginL - minX).toInt
        val translateY: Int = ((if (flipped) marginB else marginT) - minY).toInt 
        transform.translate(translateX,translateY)
      }
	  g2d.transform(transform)
      for (s <- shapes) {
        s.draw(g2d);
      }
    }
  }
  
  def invertTransform(p: Point2D): Point2D = {
    val clone = transform.clone().asInstanceOf[AffineTransform]
    clone.invert()
    clone.transform(new Point2D.Double(p.getX(), p.getY()), null);
  } 

  /** Adds a new non null colored shape in the panel. */
  def addShape(shape: VisualShape, repaintAfter: Boolean = true): Unit = {
    if (shape == null) throw new IllegalArgumentException("The added shape is null.")
    else {
      shapes.enqueue(shape)
      if (repaintAfter) repaint()
    }
  }

  /** Removes all the shapes contained in the panel. */
  def clear(repaintAfter: Boolean = true): Unit = {
    shapes.clear()
    revalidate()
    if (repaintAfter) repaint()
  }

  addMouseMotionListener {
    val drawingPanel = this
    new MouseMotionListener() {
      override def mouseMoved(e: MouseEvent) {
        drawingPanel.setToolTipText("");
        for (s <- shapes) {
          s.showToolTip(e.getPoint());
        }         
      }
      override def mouseDragged(e: MouseEvent) {}
    }
  }
  
  private def scale(factor: Double) {
    scale = scale * factor
    repaint()
  }
  
  addMouseListener {
    val drawingPanel = this
    new MouseListener() {
      override def mouseClicked(e: MouseEvent) {
        if (SwingUtilities.isRightMouseButton(e)) {
          scale(0.9)
        }
        if (SwingUtilities.isLeftMouseButton(e)) {
          if (e.getClickCount() == 2) {
            scale(1.1)
          }
          else {
            shapes.foreach(_.clicked(e.getPoint()))
          }
        }
      }
      override def mouseEntered(e: MouseEvent) {}
      override def mousePressed(e: MouseEvent) {}
      override def mouseExited(e: MouseEvent) {}
      override def mouseReleased(e: MouseEvent) {}
    }    
  }

  def showToolTip(text: String): Unit = {
    setToolTipText(text)
  }
}

object VisualDrawing {
  
  def apply(flipped: Boolean = true, scalable: Boolean = false): VisualDrawing = {
    new VisualDrawing(flipped, scalable)
  }
}

object VisualDrawingTest extends App {

  val frame = VisualFrame("Example");
  val drawing = VisualDrawing();
  val inFrame = frame.createFrame("Drawing");
  inFrame.add(drawing);
  frame.pack();

  val rect = new VisualRectangle(drawing, 50, 50, 100, 100)
  val line = VisualLine(drawing, 50, 50, 150, 150)

  try {
    Thread.sleep(1000);
  } catch {
    case e: InterruptedException => e.printStackTrace();
  }

  rect.innerCol = Color.red;
}

