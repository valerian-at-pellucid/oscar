/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      Hakan Kjellerstrand (hakank@gmail.com)
 ******************************************************************************/
package oscar.examples.cp.hakank

import oscar.cp.modeling._
import oscar.cp.search._

/**
 *
 * Diet problem in Oscar.
 *
 * Standard OR example.
 *
 * Minimize the cost for the products:
 * Type of                        Calories   Chocolate    Sugar    Fat
 * Food                                      (ounces)     (ounces) (ounces)
 * Chocolate Cake (1 slice)       400           3            2      2
 * Chocolate ice cream (1 scoop)  200           2            2      4
 * Cola (1 bottle)                150           0            4      1
 * Pineapple cheesecake (1 piece) 500           0            4      5
 *
 * 
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object Diet extends CPModel {

   def main(args: Array[String]) {

     val cp = CPSolver()

     // data
     val n = 4
     val price  = Array( 50, 20, 30, 80) // in cents
     val limits = Array(500,  6, 10,  8) // requirements for each nutrition type

     // nutritions for each product
     val calories  = Array(400, 200, 150, 500)
     val chocolate = Array(3,2,0,0)
     val sugar     = Array(2,2,4,4)
     val fat       = Array(2,4,1,5)

     // variables
     val x = Array.fill(n)(CPVarInt(cp, 0 to 10))
     val cost = sum(Array.tabulate(n)(i => x(i)*price(i)).toList)

     // constraints
     cp.minimize(cost) subjectTo {

       cp.add(sum(Array.tabulate(n)(i => x(i) * calories(i)))  >= limits(0))
       cp.add(sum(Array.tabulate(n)(i => x(i) * chocolate(i))) >= limits(1))
       cp.add(sum(Array.tabulate(n)(i => x(i) * sugar(i)))     >= limits(2))
       cp.add(sum(Array.tabulate(n)(i => x(i) * fat(i)))       >= limits(3))

     } exploration {
       
       cp.binaryFirstFail(x)
       println(x.mkString(" "))
       
     }

     cp.printStats()
   }

}
