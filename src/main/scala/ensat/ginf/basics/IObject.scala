package ensat.ginf.basics

import scala.collection.Set
import scala.collection.immutable.List
import scala.collection.mutable.ListBuffer

class IObject(val name: String, val fs: ListBuffer[IFeature] = new ListBuffer[IFeature], val features: ListBuffer[String] = new ListBuffer[String]) extends Equals {
  var featuresSet: Set[String] = null
  var iFeaturesSet: Set[String] = null
  override def toString(): String = name

  def toFeaturesSet():Set[String] = {
    if (iFeaturesSet == null) {
      iFeaturesSet = features.toSet
    }
    this.iFeaturesSet
  }

  def canEqual(other: Any) = {
    other.isInstanceOf[ensat.ginf.basics.IObject]
  }

  override def equals(other: Any) = {
    other match {
      case that: ensat.ginf.basics.IObject => that.canEqual(IObject.this) && name == that.name
      case _                               => false
    }
  }

  override def hashCode() = {
    val prime = 41
    prime + name.hashCode
  }

}
