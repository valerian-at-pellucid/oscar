/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *   
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *  
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *  
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/
package oscar.visual;

import java.awt.Color;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;

import javax.swing.JInternalFrame;

/**
 * 
 * @author Pierre Schaus
 *
 */
class VisualRectangle(d:VisualDrawing, shape:Rectangle2D.Double) extends ColoredShape[Rectangle2D.Double](d,shape){

	def rect:Rectangle2D.Double = shape

	def this( d:VisualDrawing, x:Double, y:Double, w:Double, h:Double) {
	  
		this(d, new Rectangle2D.Double(x,y,w,h))
	}
	
	/**
	 * X coordinates of bottom left corner
	 * @return
	 */
	def x = rect.getX()

	

	/**
	 * Y coordinates of bottom left corner
	 * @return
	 */
	def y = rect.getY()

	
	/**
	 * width of the rectangle
	 * @return
	 */
	def width = rect.getWidth()
	
	
	/**
	 * height of the rectangle
	 * @return
	 */
	def height = rect.getHeight()
	
	
	/**
	 * Move the specified left corner
	 * @param x
	 * @param y
	 */
	def move(x:Double, y:Double) {
		rect.setRect(x, y, width, height)
		drawing.repaint()
	}

	/**
	 * Set width
	 * @param w
	 */
	def width_=(w:Double) {
		rect.setRect(x, y, w, height)
		drawing.repaint()
	}


	/**
	 * Set height
	 * @param w
	 */
	def height_= (h:Double) {
	  rect.setRect(x, y, width, h)
	  drawing.repaint()
	}



}

object VisualRectangle{
  	
  def main(args : Array[String]) {
		val f = new VisualFrame("toto");
		val d = new VisualDrawing(false);
		val inf = f.createFrame("Drawing");
		inf.add(d);
		f.pack();

		val rect = new VisualRectangle(d, 50, 50, 100, 50);
		rect.toolTip_$eq("Hello");
		
		
		Thread.sleep(1000);
		rect.innerCol_$eq(Color.red);
		Thread.sleep(1000);
		rect.width = 200;
		Thread.sleep(1000);
		rect.height = 100;
		Thread.sleep(1000);
		rect.move(100, 20);
		for (i <- 0 until 20) {
			Thread.sleep(50);
			rect.move(rect.x+5, rect.y);
		}
		
	}
}