package Main

import scala.concurrent._
import scala.util.{Failure, Success}
import ExecutionContext.Implicits.global

import ensat.ginf.basics.{IFeature, IObject, Parser}
import ensat.ensat.galois.Lattice
import scala.swing._
import scala.swing.event.ButtonClicked


object Main extends SimpleSwingApplication {
  def top = new MainFrame {
    resizable=false;

    preferredSize = new Dimension(600, 400)

    val lattice = new Lattice
    val parser = Parser

    title = "AI Project: Formal concept analysis"

    object objectName extends TextField {
      columns = 5
    }

    object featureName extends TextField {
      columns = 5
    }

    object logs extends TextArea {
      columns = 5
      rows = 10
      editable = false
      text = "Hello !! \n"
    }

    val scroll = new ScrollPane(logs)

    val buttonConstruct = new Button() {
      text = "Construct the graph"
      enabled = false
    }

    val buttonAsk = new Button("construct") {
      text = "Ask"
      enabled = false
    }

    val buttonLoad = new Button() {
      text = "Load DATA"
    }

    val labelObject = new Label {
      text = "Object    "
    }

    val labelFeature = new Label {
      text = "Feature  "
    }

    val labelLogs = new Label {
      text = "Logs"
    }

    val labelOwners = new Label {
      text = "Adnane Ouahabi Ayaou, Tidkar Ben Ajiba, Imane Bouziane and Soukaina Legsabi."
    }

    contents = new BoxPanel(Orientation.Vertical) {
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += buttonLoad
        contents += buttonConstruct
        contents += buttonAsk
        border = Swing.EmptyBorder(10, 10, 10, 10)
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += labelObject
        contents += objectName
        border = Swing.EmptyBorder(10, 0, 10, 0)
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += labelFeature
        contents += featureName
        border = Swing.EmptyBorder(10, 0, 10, 0)
      }

      contents += new BorderPanel {
        add(labelLogs, BorderPanel.Position.West)
      }
      contents += scroll
      contents += new BorderPanel {
        add(labelOwners, BorderPanel.Position.West)
      }
      border = Swing.EmptyBorder(10, 10, 10, 10)
    }

    listenTo(buttonAsk)
    listenTo(buttonConstruct)
    listenTo(buttonLoad)
    reactions += {
      case ButtonClicked(`buttonLoad`) =>
        buttonConstruct.enabled = false
        buttonAsk.enabled = false
        buttonLoad.enabled = false

        val backgroundQuestion: Future[Unit] = Future {
          lattice prepare parser.parseList("DATA.txt")
        }

        backgroundQuestion.onComplete {
          case Success(unit) => {
            logs.text += "\nData loaded"
            buttonConstruct.enabled = true
            buttonLoad.enabled = true
          }
          case Failure(exception) => {
            logs.text += "\n Error loading the data"
          }
        }

      case ButtonClicked(`buttonConstruct`) =>
        buttonConstruct.enabled = false
        buttonLoad.enabled = false
        buttonAsk.enabled = false

        val backgroundConstruction: Future[Unit] = Future {
          lattice execute logs
        }
        backgroundConstruction.onComplete {
          case Success(unit) => {
            buttonAsk.enabled = true
            buttonLoad.enabled = true
          }
          case Failure(exception) => {
            logs.text += "\n Error constructing the graph"
          }
        }


      case ButtonClicked(`buttonAsk`) =>
        val backgroundQuestion: Future[Boolean] = Future {
          logs.text += "\n Does the object " + objectName.text + " have the feature " + featureName.text +" ?"
          lattice ask(new IObject(objectName.text), new IFeature(featureName.text))
        }
        backgroundQuestion.onComplete {
          case Success(bool: Boolean) => {
            logs.text += "\n Answer is: " + bool
          }
          case Failure(exception) => {
            logs.text += "\n Error processing your question"
          }
        }


    }
  }

  //  def main(args: Array[String]) {
  ////    val o1 = new IObject("TEST")
  ////    val lattice = new Lattice
  ////    val parser = Parser
  ////
  ////    lattice prepare parser.parseList("DATA.txt")
  ////    lattice execute
  //  }
}