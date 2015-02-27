package ensat.ginf.basics

import scala.collection.mutable.ListBuffer

/**
 * Created by Adnane on 21/01/2015.
 */
class IFeature(val name: String,
                    val characs: ListBuffer[ICharacter] = new ListBuffer[ICharacter]) extends Equals {

  var objects: ListBuffer[IObject] = new ListBuffer[IObject]
  override def toString(): String = name

  def canEqual(other: Any) = {
    other.isInstanceOf[ensat.ginf.basics.IFeature]
  }

  override def equals(other: Any) = {
    other match {
      case that: ensat.ginf.basics.IFeature => that.canEqual(IFeature.this) && name == that.name
      case _                                => false
    }
  }

  override def hashCode() = {
    val prime = 41
    prime + name.hashCode
  }
}