package io.blep.tda

import io.blep.tda.ThreadDumpAnalyzer.ThreadDump
import io.blep.tda.View.{resultContainer, listThreads}
import org.scalajs.dom.html.{Span, Button, Div, TextArea}
import org.scalajs.dom.raw.{MouseEvent, Element}
import org.scalajs.jquery.{jQuery => $}
import upickle._

import scala.scalajs.js.{Function1, JSApp}
import scala.scalajs.js.annotation.{JSExport, JSExportNamed}
import scala.util.{Failure, Success, Try}
import scalatags.JsDom.all._
import org.scalajs.dom

@JSExportNamed
class Configuration(
                     val plainDumpId: String,
                     val analyzeBtnId: String,
                     val resetBtnId: String,
                     val alertContainerId: String,
                     val threadNumberId: String
                     )

class Controller(val configuration: Configuration) {
  println("Init")

  val analyzeBtn: Button = dom.document.getElementById(configuration.analyzeBtnId).asInstanceOf[Button]
  val resetBtn: Button = dom.document.getElementById(configuration.resetBtnId).asInstanceOf[Button]

  analyzeBtn.onclick = { (e: Any) => {
    reset(e)
    val plainDumpArea: TextArea = dom.document.getElementById(configuration.plainDumpId).asInstanceOf[TextArea]
    val dumpTxt: String = plainDumpArea.value
    println("dumpTxt = " + dumpTxt.substring(0, 50))
    val triedDump: Try[ThreadDump] = Try(ThreadDumpAnalyzer.parseDump(dumpTxt))
    triedDump match {
      case Success(threadDump: ThreadDump) =>
        View.displaySharedLocks(threadDump)
        listThreads(threadDump)
      case Failure(e) => error(configuration.alertContainerId, e)
    }
  }
  }

  //  val resultContainer = div(p("line1"),p("line2")).render
  dom.document.body.appendChild(resultContainer)

  def reset(e:Any)={
    val nodes = resultContainer.childNodes
    for (i <- 1 to nodes.length) resultContainer.removeChild(nodes(0))
  }
  resetBtn.onclick = reset _

  def error(containerId: String, t: Throwable) = {
    val id1: Element = dom.document.getElementById(containerId)

    id1.appendChild(View.buildAlert(t.getMessage))
  }
}


object Controller extends JSApp {


  val elem = p("Error").render

  @JSExport
  def init(conf: Configuration) = new Controller(conf)


  @JSExport
  override def main() = println("Starting controller")
}
