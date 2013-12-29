package oscar.examples.cp

import oscar.cp.modeling._
import oscar.cp.core._

/**
 * Traveling Salesman Problem with Visualization
 *
 * Given a distance matrix between 20 cities,
 * find the shortest tour visiting each city exactly once.
 *
 * @author Pierre Schaus  pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
object TSPVisu extends App {

  // Data
  val nCities = 20
  val Cities = 0 until nCities
  val (distMatrix, coordinates) = TSPGenerator.randomInstance(nCities)

  // Solver
  implicit val cp = new CPSolver()

  // Variables
  val succ = Array.fill(nCities)(CPVarInt(Cities)) 
  val totDist = CPVarInt(0 to distMatrix.flatten.sum)

  // Constraints
  add(circuit(succ), Strong)
  add(minAssignment(succ, distMatrix, totDist))
  add(sum(Cities)(i => distMatrix(i)(succ(i))) == totDist)

  // Search heuristic
  minimize(totDist) search binaryFirstFail(succ)

  // Visual Component
  val visual = new VisualTSP(coordinates, succ)

  var nSols = 0
  onSolution {
    nSols += 1
    visual.updateTour(nSols, totDist.value)
  }

  println(start())
}

/** Generates a random TSP instance */
object TSPGenerator {
  def randomInstance(nCities: Int, seed: Int = 0): (Array[Array[Int]], Array[(Int, Int)]) = {
    val rand = new scala.util.Random(seed)
    val coord = Array.tabulate(nCities)(i => (100 + rand.nextInt(400), rand.nextInt(400)))
    val distMatrix = Array.tabulate(nCities, nCities)((i, j) => getDist(coord(i), coord(j)))
    (distMatrix, coord)
  }

  def getDist(p1: (Int, Int), p2: (Int, Int)): Int = {
    val dx = p2._1 - p1._1
    val dy = p2._2 - p1._2
    math.sqrt(dx * dx + dy * dy).toInt
  }
}

/** Visualization for TSP */
class VisualTSP(coordinates: Array[(Int, Int)], succ: Array[CPVarInt]) {

  import oscar.visual._
  import oscar.visual.plot.PlotLine

  val Cities = 0 until coordinates.size
  val frame = VisualFrame("TSP")

  // Creates the plot and place it into the frame
  val plot = new PlotLine("", "Solution number", "Distance")
  frame.createFrame("TSP Objective Function").add(plot)

  // Creates the visualization of the tour and place it into the frame
  val tour = VisualTour(coordinates)
  frame.createFrame("TSP Tour").add(tour)
  frame.pack()

  // Updates the visualization  
  def updateTour(nSol: Int, dist: Int): Unit = {
    Cities.foreach(i => tour.edgeDest(i, succ(i).value))
    tour.repaint()
    plot.addPoint(nSol, dist)
  }
}
