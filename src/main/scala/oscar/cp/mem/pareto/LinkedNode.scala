package oscar.cp.mem.pareto

class LinkedNode[T](k: Int, private var v: T) {

  private var _prev: LinkedNode[T] = null
  private var _next: LinkedNode[T] = null

  def key = k
  def value = v
  def value_= (x : T) { v = x }

  def prev = _prev
  def next = _next

  def prev_=(x: LinkedNode[T]) { _prev = x }
  def next_=(x: LinkedNode[T]) { _next = x }

  def isFirst = _prev == null
  def isLast = _next == null
}