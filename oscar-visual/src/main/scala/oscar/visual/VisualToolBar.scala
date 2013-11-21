/*******************************************************************************
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
 ******************************************************************************/
package oscar.visual

import javax.swing.JToolBar
import javax.swing.JButton
import java.awt.event.ActionListener
import java.awt.event.ActionEvent

import oscar.visual.shapes.VisualRectangle

class VisualToolBar(withVisuController: Boolean = false) extends JToolBar {
  
  
  if (withVisuController) {
    import VisualController._
    addButton("Pause", inPause = true)
    addButton("Next", next())
    addButton("Play", play())
    inPause = true
  }

  def addButton(label:String,actionOnClick : => Unit): Unit ={
    val button = new JButton(label)
    button.addActionListener( new ActionListener() {
		override def actionPerformed(e:ActionEvent) { actionOnClick }
	})
	add(button)
  }
}

object VisualToolBar{
  	
  def main(args : Array[String]) {
		val f = VisualFrame("toto");
		val tb = f.createToolBar(false)
		val d = VisualDrawing(false);
		val inf = f.createFrame("Drawing");
		inf.add(d);
		f.pack();
		
		tb.addButton("rectangle",{
		  val rect = new VisualRectangle(d, 50, 50, 100, 50);
		  rect.toolTip_$eq("Hello");
		  
		  tb.addButton("make it move",{
			  rect.move(100, 20);
		  })
		})		
	}
}
