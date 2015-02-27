package ensat.ginf.basics

import java.io.File

import scala.collection.mutable.ListBuffer
import scala.util.control.NonFatal

/**
 * Created by Adnane on 21/01/2015.
 */
object Parser {
  def parseList(fileName: String): ListBuffer[IObject] = {
    import scala.io.Source
    try {
      val file = new File(fileName)
      val fileSource = Source.fromFile(file)
      var result = ""
      //      var objects=List[IObject]

      try {
        val input = fileSource getLines() next() toString
        val array: Array[String] = input.split(" ")
        var i = 0
        var characteristic = false
        val list = new ListBuffer[IObject]()
        for (element: String <- array){
          element match {
            case "(" => i += 1
            case ")" => {
              i -= 1
              characteristic = false
            }
            case _ => {
              //println(i+"->"+element)
              if (i == 2){
                list += new IObject(element)
              }
              if (characteristic)
                list.last.fs.last.characs += new ICharacter(element)
              if (i == 4 && !characteristic) {
                list.last.fs += new IFeature(element)
                list.last.features += element
                characteristic = true;
              }
            }
          }
        }
        list foreach ( o => o.featuresSet = o.features.toSet)
        return list

      }
      catch {
        case NonFatal(_) => new ListBuffer[IObject]
      } finally
        fileSource.close()
    } catch {
      case NonFatal(_) => new ListBuffer[IObject]
    }
  }
}
