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
/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by Ghilain Florent.
  ******************************************************************************/

package oscar.cbls.routing.visual

import javax.swing.{BorderFactory, JFrame}
import oscar.visual.VisualDrawing
import oscar.visual.shapes.VisualLine
import oscar.visual.shapes.VisualCircle
import oscar.visual.shapes.VisualArrow

import java.awt.{Color, Dimension}

class SmartSolution(myPanelVRP:PanelVRP) extends JFrame{

  val mapPanel : VisualDrawing = VisualDrawing(false);
  mapPanel.setPreferredSize(new Dimension(700,700))
  mapPanel.setMinimumSize(new Dimension(500,500))
  mapPanel.setBorder(BorderFactory.createTitledBorder("Map"))
  mapPanel.setBackground(Color.white)

  setContentPane(mapPanel)
  setVisible(true)
  setTitle("Best Solution")
  pack()
  var arrows:Array[VisualArrow] = null

  cleanMapPanel()
  displayNodes()
  displayArrows()

  def cleanMapPanel(){
    mapPanel.clear()
  }

  def setColorToRoute(l:VisualLine ,i:Int){
    l.outerCol = myPanelVRP.colorsManagement(i+1)
  }

  def updateVisualisation() {
    val vrp = myPanelVRP.vrpModel.vrp
    val nodes = myPanelVRP.vrpModel.towns

    def update(i: Int) {
      if(vrp.isRouted(i)){
        arrows(i).visible = true
        setColorToRoute( arrows(i),vrp.RouteNr(i).value)
        arrows(i).dest = (nodes(vrp.Next(i).value).long,
          nodes(vrp.Next(i).value).lat)
      }
      else
        arrows(i).visible = false
    }
    for (i <- 0 until vrp.N) update(i)
  }



  def displayNodes(){
    val nodes = myPanelVRP.vrpModel.towns
    myPanelVRP.colorsManagement.setDifferentColors(myPanelVRP.vrpModel.V)
    for(i <- 0 until nodes.length){
      val t = nodes(i)
      if (i<myPanelVRP.vrpModel.V) new VisualCircle(mapPanel, t.long,t.lat,10).innerCol = myPanelVRP.colorsManagement(i+1)
      else new VisualCircle(mapPanel, t.long,t.lat,6).innerCol = Color.white
    }
  }

  def displayArrows() {
    val vrp = myPanelVRP.vrpModel.vrp
    val nodes = myPanelVRP.vrpModel.towns

    arrows = Array.tabulate(myPanelVRP.vrpModel.N)(i => {
      val arrow =
        if (vrp.isRouted(i)) VisualArrow(mapPanel,nodes(i).long,nodes(i).lat,
          nodes(vrp.Next(i).value).long,nodes(vrp.Next(i).value).lat,4)
        else VisualArrow(mapPanel,nodes(i).long,nodes(i).lat,nodes(i).long,nodes(i).lat,4)
      if(vrp.isRouted(i)) setColorToRoute(arrow,vrp.RouteNr(i).value)
      arrow
    })
  }

  def updateUI(){
    updateVisualisation()
  }
}