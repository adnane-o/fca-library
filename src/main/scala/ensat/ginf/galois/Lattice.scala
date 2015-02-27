package ensat.ensat.galois

import ensat.ginf.basics.{IFeature, IObject}
import ensat.ginf.galois.Node

import scala.collection.mutable.ListBuffer
import scala.swing.TextArea

/**
 * Created by Adnane on 25/01/2015.
 */
class Lattice {
  var distinctFeatures: Set[IFeature] = null
  var distinctObjects: Set[IObject] = null
  var mainRoot: Node = null

  def ask(o: IObject, f: IFeature, node: Node = mainRoot): Boolean = {
    if (!node.objects.contains(o))
      return false
    if (node.features.contains(f))
      return true
    var temp = false
    node.children.foreach(
      n => {
        temp = ask(o, f, n)
        if (temp)
          return true
      }
    )
    temp
  }

  def execute(textArea: TextArea) = {
    mainRoot = getRoot()

    //Construire le graphe
    var startTime: Long = System.currentTimeMillis()
    println("Graph construction ...")
    textArea.text += "\nGraph construction ..."
    constructB(Set(mainRoot))
    var estimatedTime = System.currentTimeMillis() - startTime
    println("Graph construction : Finish in : " + estimatedTime + " ms")
    textArea.text += "\nGraph construction : Finish in : " + estimatedTime + " ms"

//    //Trouver la reponse
//    startTime = System.currentTimeMillis()
//    println("Question -> Object: MANDARIN , Feature: A_CHINESE_LANGUAGE")
//    var answer = ask(new IObject("MANDARIN"), new IFeature("A_CHINESE_LANGUAGE"))
//    println("Answer is: " + answer)
//    estimatedTime = System.currentTimeMillis() - startTime
//    println("Finish in : " + estimatedTime + " ms")
//
//    startTime = System.currentTimeMillis()
//    println("Question -> Object: MEDAL , Feature: A_PRIZE")
//    answer = ask(new IObject("MEDAL"), new IFeature("A_PRIZE"))
//    println("Answer is: " + answer)
//    estimatedTime = System.currentTimeMillis() - startTime
//    println("Finish in : " + estimatedTime + " ms")
//
//    startTime = System.currentTimeMillis()
//    println("Question -> Object: MEDAL , Feature: HAS_LISTS_OF_FOODS")
//    answer = ask(new IObject("MEDAL"), new IFeature("HAS_LISTS_OF_FOODS"))
//    println("Answer is: " + answer)
//    estimatedTime = System.currentTimeMillis() - startTime
//    println("Finish in : " + estimatedTime + " ms")
//
//    startTime = System.currentTimeMillis()
//    println("Question -> Object: BANANA , Feature: A_FRUIT")
//    answer = ask(new IObject("BANANA"), new IFeature("A_FRUIT"))
//    println("Answer is: " + answer)
//    estimatedTime = System.currentTimeMillis() - startTime
//    println("Finish in : " + estimatedTime + " ms")
//
//    startTime = System.currentTimeMillis()
//    println("Question -> Object: BANANA , Feature: MADE_OF_METAL")
//    answer = ask(new IObject("BANANA"), new IFeature("MADE_OF_METAL"))
//    println("Answer is: " + answer)
//    estimatedTime = System.currentTimeMillis() - startTime
//    println("Finish in : " + estimatedTime + " ms")
  }

  def printGraph(root: Node): Unit = {
    println(root)
    root.children match {
      case children if (children.size == 0) =>
      case that => {
        that.foreach(n => printGraph(n))
      }
    }
  }

  def constructB(parents: Set[Node]): Unit = {
    if (parents.size == 1 && parents.head.features == distinctFeatures)
      return

    var tempValues = new ListBuffer[Node]()
    for (parent <- parents) {
      parent.features match {
        case fs if fs == distinctFeatures =>
        case _ => {
          val newFeatureSet = distinctFeatures -- parent.features
          val childCandidates = new ListBuffer[Node]
          newFeatureSet foreach (
            f => childCandidates += new Node(f.objects.toSet & parent.objects, Set(f) | parent.features))

          var concepts = keepConcepts(mergeDuplicates(childCandidates).toList, List()).toSet
          var finalConcepts = Set[Node]()

          concepts.foreach(c => {
            var temp = bfs(parent, c)
            if (temp != null)
              parent.children += temp
            else
              parent.children += c
          })

          parent.children.foreach(
            n => {
              if (n.children.size == 0)
                tempValues += n
            })
        }
      }
    }
    constructB(tempValues.toSet)
  }

  def construct(root: Node): Unit = {
    root.features match {
      case fs if fs == distinctFeatures =>
      case _ => {
        val newFeatureSet = distinctFeatures -- root.features
        val childCandidates = new ListBuffer[Node]
        newFeatureSet foreach (
          f => childCandidates += new Node(f.objects.toSet & root.objects, Set(f) | root.features))

        var concepts = keepConcepts(mergeDuplicates(childCandidates).toList, List()).toSet
        var finalConcepts = Set[Node]()

        concepts.foreach(c => {
          var temp = bfs(root, c)
          if (temp != null)
            root.children += temp
          else
            root.children += c
        })

        root.children.foreach(
          n => {
            if (n.children.size == 0)
              construct(n)
          })
      }
    }
  }

  def bfs(currentNode: Node, candidate: Node): Node = {
    if (currentNode == candidate)
      return currentNode

    if (currentNode.children.size == 0)
      return null

    var temp: Node = null
    for (node <- (currentNode.children)) {
      temp = bfs(node, candidate)
      if (temp != null)
        return temp
    }
    temp
  }

  def getRoot(): Node = {
    val root = new Node(distinctObjects)
    distinctFeatures.foreach(
      f => if (distinctObjects subsetOf f.objects.toSet)
        root.features += f)
    root
  }

  def prepare(objectList: ListBuffer[IObject]) = {

    val allFeatures = new ListBuffer[IFeature]
    objectList.foreach(o => allFeatures ++= o.fs)

    distinctFeatures = allFeatures.toSet
    distinctObjects = objectList.toSet

    distinctFeatures foreach (
      f => distinctObjects foreach (
        o => if (o.fs.toSet contains f) {
          f.objects += o
        }))
  }

  def keepConcepts(nodes: List[Node], current: List[Node]): List[Node] = {
    nodes match {
      case List() => current
      case node :: rest => {
        if (isConcept(node))
          keepConcepts(rest, node :: current)
        else
          keepConcepts(rest, current)
      }
    }
  }

  def isConcept(node: Node): Boolean = {
    var tempFeatures = distinctFeatures
    node.objects.foreach(o => {
      tempFeatures = tempFeatures & o.fs.toSet
    })
    return node.features == tempFeatures
  }

  def mapToList(map: Map[Set[IObject], Node]): ListBuffer[Node] = {
    var list = new ListBuffer[Node]
    map.foreach((e: (Set[IObject], Node)) => list += e._2)
    return list
  }

  def mergeDuplicates(childCandidates: ListBuffer[Node]): ListBuffer[Node] = {
    var tempFeatures = Set[IFeature]()
    var childCandidatesMap = childCandidates.groupBy(_.objects).transform(
      (k, v) => {
        tempFeatures = Set[IFeature]()
        v.foreach(n => tempFeatures ++= n.features)
        new Node(k, tempFeatures)
      })

    return mapToList(childCandidatesMap)
  }
}
