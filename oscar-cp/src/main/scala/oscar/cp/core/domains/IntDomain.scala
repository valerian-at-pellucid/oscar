package oscar.cp.core.domains

import oscar.cp.core.CPOutcome
import scala.util.Random

/** @author Renaud Hartert */

abstract class IntDomain extends Iterable[Int] {
  
  def size: Int

  def isEmpty: Boolean
  
  def isBound: Boolean 

  def max: Int

  def min: Int
  
  def randomValue(rand: Random): Int
  
  def hasValue(value: Int): Boolean
  
  def removeValue(value: Int): CPOutcome 
  
  def assign(value: Int): CPOutcome

  def updateMin(value: Int): CPOutcome

  def updateMax(value: Int): CPOutcome

  def nextValue(value: Int): Int

  def prevValue(value: Int): Int

  def delta(oldMin: Int, oldMax: Int, oldSize: Int): Iterator[Int] 
}