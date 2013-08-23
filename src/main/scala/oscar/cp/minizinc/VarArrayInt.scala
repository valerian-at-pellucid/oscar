package oscar.cp.minizinc

import oscar.cp.core.CPVarInt

class VarArrayInt (
    val value: Set[Int],
    val annotations: List[Annotation],
    val cpvar: Array[CPVarInt],
    override val name: String
    ) extends FZObject (name) {

	override def toString() = name + " " + annotations + " " + cpvar.mkString(",")
	
}