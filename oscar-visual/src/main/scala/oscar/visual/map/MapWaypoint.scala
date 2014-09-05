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
package oscar.visual.map

import java.awt.Color

/**
 * @author Corentin Mulders
 */
class MapWaypoint(map: VisualMap, xlat: Double, xlong: Double, col: Color = Color.BLUE, lbl: String = null) {
  var lat = xlat
  var long = xlong

  var color = col

  var label = lbl

  def this(map: VisualMap, o: (Double, Double), col: Color) = this(map, o._1, o._2, col)
  def remove = map.removeWaypoint(this)

}
