package oscar.visual.shapes

import java.awt.Shape
import java.awt.Color
import java.awt.Graphics2D
import java.awt.geom.Point2D
import oscar.visual.VisualDrawing

abstract class VisualShape[+S <: Shape](protected val drawing: VisualDrawing, protected val shape: S) {

  private var _fillColor = Color.white
  private var _borderColor = Color.black

  private var _visible = true
  private var _fill = true
  private var _border = true

  private var _toolTipText: String = null
  
  // If true, the drawing repaints after each modification of the shape
  private var _autoRepaint: Boolean = true
  
  def autoRepaint: Boolean = _autoRepaint
  def autoRepaint_= (b: Boolean): Unit = { _autoRepaint = b }
  
  // Adds the visual shape in the drawing
  drawing.addShape(this);

  def innerCol: Color = _fillColor
  def outerCol: Color = _borderColor
  def visible: Boolean = _visible
  def fill: Boolean = _fill
  def border: Boolean = _border
  def toolTip: String = _toolTipText
  
  def visible_=(visible: Boolean): Unit = {
    _visible = visible
    if (autoRepaint) repaint()
  }

  def border_=(border: Boolean): Unit = {
    _border = border
    if (autoRepaint) repaint()
  }

  def fill_=(fill: Boolean): Unit = {
    _fill = fill
    if (autoRepaint) repaint()
  }

  def innerCol_=(innerCol: Color): Unit = {
    _fillColor = innerCol
    if (autoRepaint) repaint()
  }

  def outerCol_=(outerCol: Color): Unit = {
    _borderColor = outerCol
    if (autoRepaint) repaint()
  }

  def toolTip_=(s: String): Unit = { _toolTipText = s }

  def draw(g: Graphics2D) {
    if (visible) {
      if (fill) {
        g.setColor(_fillColor)
        g.fill(shape)
      }
      if (border) {
        g.setColor(_borderColor)
        g.draw(shape)
      }
    }
  }

  def showToolTip(mousePoint: Point2D) = {
    if (toolTip != null && shape.contains(mousePoint)) {
      drawing.showToolTip(toolTip);
    }
  }
  
  def repaint(): Unit = drawing.repaint()
  
  def getBounds: (Int, Int, Int, Int) = {
    val bounds = shape.getBounds()
    val xMin = bounds.x
    val xMax = xMin + bounds.width
    val yMin = bounds.y
    val yMax = yMin + bounds.height
    (xMin, xMax, yMin, yMax)
  }
  
  def move(x: Double, y: Double): Unit
}


