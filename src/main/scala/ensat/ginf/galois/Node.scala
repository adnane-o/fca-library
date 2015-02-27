package ensat.ginf.galois

import ensat.ginf.basics.{IFeature, IObject}

import scala.collection.mutable.ListBuffer

/**
 * Created by Adnane on 25/01/2015.
 */
class Node(var objects: Set[IObject] = Set[IObject](),
           var features: Set[IFeature] = Set[IFeature](),
           var children: Set[Node] = Set[Node](),
           val parent: Node = null) extends Equals {

  override def toString: String = objects + "->" + features + "@" + super.toString()

  def canEqual(other: Any) = {
    other.isInstanceOf[ensat.ginf.galois.Node]
  }

  override def equals(other: Any) = {
    other match {
      case that: ensat.ginf.galois.Node => that.canEqual(Node.this) && objects == that.objects && features == that.features
      case _ => false
    }
  }

  override def hashCode() = {
    val prime = 41
    prime * (prime + objects.hashCode) + features.hashCode
  }

}