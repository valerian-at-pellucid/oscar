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
package oscar.examples.linprog


import scala.collection.JavaConversions._
import oscar.linprog.modeling._
import oscar.algebra._

/**
 * Chemical Tanker Problem:
 * The Objective is to place products (called cargos) into tanks on a chemical tanker (vessel).
 * - At most one cargo per tank but several tanks can be used to all the volume of one cargo.
 * - Some cargo cannot be placed into adjacent tanks (different temperature requirement and security constraints)
 * - Some cargo cannot be placed into some tanks (all the tanks does not have the required property to accept the cargo)
 * The objective it to place  all the volumes while satisfying the security constraints and maximizing the total free space (total volume of unused space).
 * The idea of the objective function is to let more freedom for future cargos and also to decrease the cleaning costs
 *
 * @author Pierre Schaus pschaus@gmail.com
 */
object ChemicalTanker extends MIPModel(LPSolverLib.gurobi) with App {


  /**
   * Class representing a cargo object and its related data.
   * The constructor parses the xml cargo node
   */
  class Cargo(node: scala.xml.Node) {
    val id = (node \ "@id").text.toInt
    val name = (node \ "@name").text
    val volume = (node \ "@volume").text.toInt
    override def toString = id + ""
  }

  /**
   * Class representing a tank object and its related data.
   * The constructor parses the xml tank node
   */
  class Tank(node: scala.xml.Node, cargos: Array[Cargo]) {
    val id = (node \ "@id").text.toInt
    val capa = (node \ "@capa").text.toInt
    val x = (node \ "@x").text.toInt
    val y = (node \ "@y").text.toInt
    val w = (node \ "@w").text.toInt
    val h = (node \ "@h").text.toInt
    val impossibleCargos =
      for (n <- (node \ "impossiblecargos" \ "cargo").toArray)
        yield (n \ "@id").text.toInt
    val neighbours =
      for (n <- (node \ "neighbours" \ "tank").toArray)
        yield (n \ "@id").text.toInt
    val possibleCargos = (0 until cargos.size).filter(!impossibleCargos.contains(_)).toSet

  }

  // ------------- parses the data of the problem  ---------------

  val problemNode = xml.XML.loadFile("data/chemical4.xml")
  val dummyCargo = new Cargo(<cargo id="0" name="empty" volume="0"/>)
  val cargos = Array(dummyCargo) ++ // dummy cargo
    (for (node <- (problemNode \ "cargos" \ "cargo").toArray)
      yield new Cargo(node))
  val tanks =
    for (node <- (problemNode \ "tanks" \ "tank").toArray)
      yield new Tank(node, cargos)

  val totCapa = (0 /: tanks)((s, t) => s + t.capa) // fold left to compute tot capa

  // extract cargo that cannot be be adjacent to each others
  val incompatibles: Set[(Int, Int)] =
    (for (n <- (problemNode \ "incompatibles" \ "incompatible"))
      yield ((n \ "@cargo1").text.toInt, (n \ "@cargo2").text.toInt)).toSet
  // transform this information to get the possible adjacent pairs
  // transform this information to get the possible adjacent pairs
  val compatibles =
    (for (
      i <- 0 until cargos.size;
      j <- 0 until cargos.size;
      if (!incompatibles.contains((i, j)) &&
        !incompatibles.contains((j, i)))
    ) yield (i, j)).toSet

  // ------------- declare the lp model ---------------



  // for each tank, the cargo type placed into it (dummy cargo if emmty)
  val cargo = Array.fill(tanks.size, cargos.size)(MIPFloatVar(0 to 1))
  // for each cargo, the number of tanks allocated to it
  val card = Array.fill(cargos.size)(MIPFloatVar(0 to tanks.size))

  // objective = maximize the total empty space
  val freeSpace = sum(0 until tanks.size)(t => cargo(t)(0) * tanks(t).capa)
  val nbFreeTanks = sum(0 until tanks.size)(t => cargo(t)(0))

  // make the link between cargo and load vars with binPacking constraint
  for (c <- 0 until cargos.size) {
    add(sum(0 until tanks.size)(t => cargo(t)(c) * tanks(t).capa) >= cargos(c).volume)
  }
  for (t <- 0 until tanks.size) {
    add(sum(0 until cargos.size)(c => cargo(t)(c)) == 1) // every tank has exactly one cargo
  }
  for (t <- 0 until tanks.size; c <- 0 until cargos.size; if !tanks(t).possibleCargos.contains(c)) {
    add(cargo(t)(c) == 0)
  }

  // enforce that for any two neighbor tanks, they must contain compatible cargo types
  for (t <- tanks; t2 <- t.neighbours; if (t2 > t.id)) {
    for ((c1, c2) <- incompatibles) {
      add(cargo(t.id - 1)(c1) + cargo(t2 - 1)(c2) <= 1)
      add(cargo(t.id - 1)(c2) + cargo(t2 - 1)(c1) <= 1)
    }
  }

  //maximize(nbFreeTanks)
  maximize(freeSpace) 
  
  start()

  println("objective:" + objectiveValue)

  val res = (0 until tanks.size).map(t => (0 until cargos.size).filter(c => cargo(t)(c).value.get >= 0.9).head)
  println(res.mkString(","))

}
