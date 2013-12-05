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
/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.visual.map
import java.awt.BorderLayout
import java.awt.Color
import java.io.IOException
import javax.swing.JPanel
import org.jdesktop.swingx.JXMapViewer
import org.jdesktop.swingx.mapviewer.DefaultTileFactory
import org.jdesktop.swingx.mapviewer.GeoPosition
import org.jdesktop.swingx.mapviewer.TileFactoryInfo
import java.lang.InterruptedException
import java.awt.Toolkit
import oscar.visual.VisualFrame

/**
 * @author Pierre Schaus
 */
class VisualMap extends JPanel(new BorderLayout()) {

  val viewer = new JXMapViewer()
  var lines = List[MapLine]()
  var waypoints = List[MapWaypoint]()
  var paths = List[MapPath]()

  val max = 17
  val info = new TileFactoryInfo(1, max - 2, max,
    256, true, true, // tile size is 256 and x/y orientation is normal
    "http://tile.openstreetmap.org",
    "x", "y", "z") {
    override def getTileUrl(x: Int, y: Int, zoo: Int) = {
      val zoom = max - zoo
      this.baseURL + "/" + zoom + "/" + x + "/" + y + ".png"
    }

  }

  val tf = new DefaultTileFactory(info)
  viewer.setTileFactory(tf)
  viewer.setZoom(11)
  viewer.setAddressLocation(new GeoPosition(51.5, 0))

  viewer.setOverlayPainter(new MapPainter(this))

  viewer.setName("mapKit")
  val screensize = Toolkit.getDefaultToolkit().getScreenSize()
  viewer.setPreferredSize(new java.awt.Dimension(screensize.width / 2, screensize.height / 2))
  //viewer.setDefaultProvider(DefaultProviders.OpenStreetMaps);
  add(viewer)

  def createWaypoint(lt: Double, lg: Double, col : Color = Color.BLUE, lbl : String = null): MapWaypoint = {
    val res = new MapWaypoint(this, lt, lg, col, lbl)
    waypoints = waypoints :+ res
    refresh()
    res
  }

  def removeWaypoint(wp : MapWaypoint) = {
    waypoints = waypoints.filterNot(_ == wp)
    refresh()
  }

  def createLine(o: (Double, Double), d: (Double, Double), col : Color = Color.RED ): MapLine = {
    val l = new MapLine(this, o._1, o._2, d._1, d._2, col);
    lines = lines :+ l

    refresh()

    l
  }


  def removeLine(line: MapLine) = {
    lines = lines.filterNot(_ == line)
    refresh()
  }

  def clear() = {
    lines = List[MapLine]()
    waypoints = List[MapWaypoint]()
    paths = List[MapPath]()
    refresh()
  }

  def createPath(o: (Double, Double), d: (Double, Double), col : Color = Color.BLACK): MapPath = {
    val p = new MapPath(this, o._1, o._2, d._1, d._2, col);
    paths = paths :+ p

    refresh()

    p
  }


  def removePath(p: MapPath) = {
    paths = paths.filterNot(_ == p)
    refresh()
  }

  def refresh() = {
    viewer.repaint()
  }

}

object VisualMap {
  def main(args: Array[String]) = {

    val f = VisualFrame("toto")
    var inf = f.createFrame("tmap")

    val map = new VisualMap()

    inf.add(map)
    f.pack();

    try {
      val countries = List("Albania", "Andorra", "Austria", "Belgium", "Bulgaria", "Croatia", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Ireland", "Latvia", "Luxembourg", "Portugal", "Spain", "United Kingdom")
      for (c <- countries) {

        val loc = Geocoder.getLocation(c)
        println(c + " " + loc.lat + "," + loc.lon)
        map.createWaypoint(loc.lat, loc.lon);
        try {
          Thread.sleep(200)
        } catch {
          case e: InterruptedException => e.printStackTrace()
        }

      }

      val ny = Geocoder.getLocation("New York")
      val be = Geocoder.getLocation("Brussels")

      //MapLine l = map.createLine(ny.lat,ny.lon,be.lat,be.lon);

      val pa = map.createPath((50.294603, 4.800819), (50.844972, 4.357246))

    } catch {
      case e1: IOException => e1.printStackTrace()
    }

    map.refresh()

    System.out.println(map.hashCode());
    /*
		try {
			Thread.sleep(2000);
		} catch {
		case e: InterruptedException => e.printStackTrace()
		}
		*/

  }
}

