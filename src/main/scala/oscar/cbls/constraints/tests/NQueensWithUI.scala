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

/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer and Christophe Ponsard
 ******************************************************************************/

package oscar.cbls.constraints.tests

import scala.swing.SimpleSwingApplication
import scala.swing.MainFrame
import scala.swing.Button
import java.awt.GridLayout
import scala.swing.GridPanel
import scala.swing.Label
import oscar.cbls.search.SearchEngine
import oscar.cbls.invariants.lib.minmax.ArgMaxArray
import oscar.cbls.invariants.lib.logic.SelectLESetQueue
import oscar.cbls.constraints.core.ConstraintSystem
import oscar.cbls.constraints.lib.global.AllDiff
import oscar.cbls.invariants.lib.logic._
import oscar.cbls.invariants.lib.minmax._
import oscar.cbls.modeling.Algebra._
import scala.swing.Component
import scala.swing.Swing
import javax.swing.ImageIcon
import java.awt.Color
import oscar.cbls.search.SearchEngineTrait
import sun.swing.ImageIconUIResource
import scala.swing.BoxPanel
import scala.swing.Orientation
import scala.swing.event.ButtonClicked
import javax.swing.border.LineBorder
import oscar.cbls.invariants.core.computation._

object NQueensWithUI extends SimpleSwingApplication with SearchEngineTrait {

  var N: Int = 15;
  var Dim = Range(0, N)
  var PAUSE: Int = 50+1000/N;
      
  // UI stuff
  val cl=Thread.currentThread().getContextClassLoader()
  val QUEEN = new ImageIcon(cl.getResource("oscar/cbls/constraints/tests/resources/queen-ok.png"))
  val CONFLICT = new ImageIcon(cl.getResource("oscar/cbls/constraints/tests/resources/queen-ko.png"))
  val EMPTY = new ImageIcon(cl.getResource("oscar/cbls/constraints/tests/resources/queen-no.png"))
  var boxPanel:BoxPanel=null
  var tab:Array[Array[Label]]=null
  var lNQueen:Label=null
  
  // control stuff
  var stopRequested=false
  var solverRunning=false
  
  def top = new MainFrame {
    title = "NQueen"
    reInit()
    
    boxPanel = new BoxPanel(Orientation.Vertical) {      
      contents += reInit()
      contents += new BoxPanel(Orientation.Horizontal) {
        val bLess = new Button() {
          text = "Less (min 4)"
        }
        lNQueen = new Label("  "+N+"  ")
        val bMore = new Button() {
          text = "More (max 25)"
        }
        val lSpace = new Label("      ")
        val bRestart = new Button() {
          text = "Restart"
        }
        contents.append(bLess,lNQueen,bMore,lSpace,bRestart)
        listenTo(bLess, bMore, bRestart)
        reactions += {
          case ButtonClicked(`bLess`) => if (N>4) resetProblem(N-1)
          case ButtonClicked(`bMore`) => if (N<25) resetProblem(N+1)
          case ButtonClicked(`bRestart`) => resetProblem(N)
        }
      }           
    }
    contents = boxPanel

    startSolverInThread()
  }

  def startSolverInThread() {
    new Thread {
      override def run() {
        stopRequested=false
        solverRunning=true
        solve()
        solverRunning=false
      }
    }.start()    
  }
  
  def resetProblem(n:Int) {
    if (stopRequested) return // not reentering
        
    N=n
    PAUSE=50+1000/N;
    
    lNQueen.text="  "+N+"  "
    stopRequested=true
    while (solverRunning==true) {
      Thread.sleep(50)
    }
    boxPanel.contents(0)=reInit()
    boxPanel.revalidate()
    startSolverInThread()    
  }
  
  def reInit():GridPanel = {
    Dim = Range(0, N)
    tab=Array.ofDim[Label](N,N)
    
    val LineBorder=new LineBorder(Color.BLACK,1)
    
    new GridPanel(N, N) {
        for (i <- Dim; j <- Dim) {
          val lab = new Label("")
          lab.icon=EMPTY
          lab.border = LineBorder
          contents += lab
          tab(i)(j) = lab
        }
        background=Color.WHITE
    }
  }

  def solve() {
    val min = 0
    val max = N-1
    val range:Range = Range(0,N)
    val tabulength = 0
    val m: Model = new Model(false,false,true)
    val MaxIT = 10000

    println("NQueens(" + N + ")")
    val Queens:Array[IntVar] = new Array[IntVar](N)
    for (q <- range){
      Queens(q) = new IntVar(m, min, max, q, "queen" + q)
      tab(q)(q).icon=CONFLICT
    }

    val c:ConstraintSystem = new ConstraintSystem(m)
    //c.post(AllDiff(Queens)) handled trough permutations
    c.post(AllDiff(for ( q <- range) yield (q + Queens(q)).toIntVar))
    c.post(AllDiff(for ( q <- range) yield (q - Queens(q)).toIntVar))
    
    for (q <- range){c.registerForViolation(Queens(q))}
    c.close()
    
    val viol:Array[IntVar] = (for(q <- range) yield c.violation(Queens(q))).toArray

    for (q<-range){
      Event(Queens(q),viol(q),(oldqueenposition:Int) => {
        tab(q)(oldqueenposition).icon=EMPTY
        if (viol(q).value>0){
          tab(q)(Queens(q).value).icon=CONFLICT
        }else{
          tab(q)(Queens(q).value).icon=QUEEN
        }
      })
    }

    m.close()
    
    var it:Int =0
    val Tabu = (for(q <- range)yield -1).toArray

    var longueurplateau = 0;
    while((c.Violation.value > 0) && (it < MaxIT) && !stopRequested){
      val oldviolation:Int = c.Violation.value
      val allowedqueens = range.filter(q => Tabu(q) < it)
      val (q1,q2) = selectMin(allowedqueens, allowedqueens)((q1,q2) => c.getSwapVal(Queens(q1),Queens(q2)), (q1,q2) => q1 < q2)

      Queens(q1) :=: Queens(q2)
      Tabu(q1) = it + tabulength
      Tabu(q2) = it + tabulength
      m.propagate() //we need to do this because only partial propagation are performed otherwise, and events are not propagated
      it += 1
      println("it: " + it + " " + c.Violation + " (swapped "+ q1 + " and " + q2 + ")")
      if(oldviolation <= c.Violation.value) longueurplateau+=1 else longueurplateau = 0

      if (longueurplateau > 5){
        println("jump away")
        for (i <- 1 to N/5){
          Queens(selectFrom(range)) :=: Queens(selectFrom(range))
        }
        longueurplateau = 0
      }
      
      Thread.sleep(PAUSE)
    }
    println("SOLVE ENDED")
  }
}
