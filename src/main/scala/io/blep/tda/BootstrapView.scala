package io.blep.tda

import io.blep.tda.BootstrapView.resultContainer
import io.blep.tda.ThreadDumpAnalyzer._
import org.scalajs.dom
import org.scalajs.dom.html.{Div, Span}
import org.scalajs.dom.raw.NodeList

import scala.scalajs.js
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._

object BootstrapView {
  def buildAlert(msg: String) = div(cls := "alert alert-danger alert-dismissible fade in")(
    button(`type` := "button", `class` := "close", "data-dismiss".attr := "alert") {
      span("x")
    },
    s"Parsing error : ${msg}"
  ) render

  val resultContainer = div.render

}


class BootstrapView(val threadDump: ThreadDump) extends View{

  val runningBullet = span(`class`:="glyphicon glyphicon-repeat running-thread")
  val blockedBullet = span(`class`:="glyphicon glyphicon-remove-circle blocked-thread")
  val waitingBullet = span(`class`:="glyphicon glyphicon-time waiting-thread")
  val timedWaitingBullet = span(`class`:="glyphicon glyphicon-time timed-waiting-thread")
  val newBullet = span(`class`:="glyphicon glyphicon-collapse-up running-thread")

  val searchResult = span render
  val searchInput = input(`class` := "form-control", `type` := "text", placeholder:="Case matters...").render


  override def displayResults={
    resultContainer.appendChild(
      div(`class` := "row")(
        div(`class` := "col-md-4")(
          buildGeneralInfoPanel
        ),
        div(`class` := "col-md-8")(
          buildSharedLockPanel
        )
      ) render
    )

    resultContainer.appendChild(listThreads)

    dom.document.addEventListener("analysis_finished", (e:Any)=> searchInput.focus())

  }

  def buildGeneralInfoPanel={
    div(`class`:= "affix affix-top")(
      div(`class` := "panel panel-default")(
        div(`class` := "panel-heading")(h2(`class` := "panel-title")("General information")),
        div(`class` := "thread-listing panel-body")(
          table(`class` := "info")(
            tr(
              td("Dump date"), td(threadDump.dateStr)
            ),
            tr(
              td("Java version"), td(threadDump.javaVersion)
            ),
            tr(
              td("All threads"), td(threadDump.threads.size)
            ),
            tr(
              td(a(href := "#runningThreads")(runningBullet, " Running threads")), td(threadDump.runningThreads.size)
            ),
            tr(
              td(a(href := "#blockedThreads")(blockedBullet, " Blocked threads")), td(threadDump.blockedThreads.size)
            ),
            tr(
              td(a(href := "#waitingThreads")(waitingBullet, " Waiting threads")), td(threadDump.waitingThreads.size)
            ),
            tr(
              td(a(href := "#timedWaitingThreads")(timedWaitingBullet, " Timed waiting threads")), td(threadDump.timedWaitingThreads.size)
            ),
            tr(
              td(newBullet, " New threads"), td(threadDump.newThreads.size)
            ),
            tr(
              td("System threads"), td(threadDump.systemThreads.size)
            )
          )
        )
      ),
      form(`class`:="form-inline", onsubmit:="return false;")(
        div(`class`:="form-group")(searchInput)," ",
        button(`class`:="btn btn-primary btn-default", onclick := { (e: Any) => searchAction(searchInput.value) })("Search in stacks"),
        " ",
        button(`class`:="btn btn-primary btn-default", onclick := { (e: Any) => {
          resetHighlight
          resetSearchResult
        }})("Reset"),
        br,searchResult,
        br,br,buildInfo
      )
    ) render

  }

  val buildInfo=div(`class`:="bs-callout bs-callout-info")(
    h4(BuildInfo.name),
    em("Version: "), BuildInfo.version, br, em("Commit: "), BuildInfo.gitCommit
  )

  def searchAction(searchString:String)={
    resetHighlight
    val pres: NodeList = dom.document.getElementsByTagName("pre")
    val res = for {
      i <- 0 to pres.length -1
      node = pres(i)
      text = node.textContent
      if text contains searchString
    } yield {
        val parentNode = node.parentNode

        val (occurenceCount,newPre) = buildHighlightesPre(text,searchString)
        parentNode.removeChild(node)
        parentNode.appendChild(newPre)

        parentNode.parentNode.attributes.getNamedItem("class").value = "panel-collapse collapse in"
        occurenceCount
      }

    val totalOcurrenceCount = if (res.nonEmpty) res.reduce(_+_) else 0
    searchResult.innerHTML = s"$totalOcurrenceCount occurences found"
  }

  def resetSearchResult = searchResult.innerHTML = ""

  def resetHighlight={
    val pres: NodeList = dom.document.getElementsByTagName("pre")
    for (i <- 0 to pres.length - 1) {
      val node = pres(i)
      node.textContent = node.textContent
    }

  }

  def buildHighlightesPre(text:String, toFind:String)={
    def _inner(strs:List[String], acc:List[Modifier]):List[Modifier] = strs match {
      case Nil => acc
      case e :: Nil => span(e) :: acc
      case e :: tail => _inner(tail, em(`class`:="highlight")(toFind) :: span(e) :: acc)
    }

    val strings = text.split(toFind).toList

    (strings.size-1,pre(_inner(strings,Nil).reverse).render)
  }

  def buildSharedLockPanel={
    val (tags, collapsablePanels) = (for (l <- threadDump.sharedLocks) yield {
      val theId: String = l.monitor.id
      val (panelDiv, collapsablePanel) = buildThreadAccordion(theId)(l.ownedBy)

      val (accordionsForBlocked, blockedCollapsablePanels) = (l.blockedThreads map buildThreadAccordion(theId))unzip
      val theDiv = div(`class`:="panel-body", id := theId , role:="tablist")(
        s"${l.monitor.id}: ${l.monitor.clazz}", br,
        s"Owned by : ", br,
        panelDiv,
        s"Locks ${l.blockedThreads.size} threads : ", br,
        div(`class`:="thread-listing", id:=s"blockedBy$theId}")(
          accordionsForBlocked
        )
      ) render

      (theDiv, collapsablePanel::blockedCollapsablePanels)

    }) unzip

    val allCollapsable = collapsablePanels flatten

    val btnGrp = buildCollapseAndExpandButtons(allCollapsable)

    val panel: Div = div(`class` := "panel panel-default")(
      div(`class` := "panel-heading")(h2(`class` := "panel-title")(s"Hot monitors (${threadDump.sharedLocks.size})",btnGrp))
    ) render


    tags foreach panel.appendChild
    panel
  }


  def listThreads ={
    div(`class`:="row")(
      buildThreadListPanel("runningThreads","Running threads", threadDump.runningThreads),
      buildThreadListPanel("blockedThreads","Blocked threads", threadDump.blockedThreads),
      buildThreadListPanel("waitingThreads","Waiting threads", threadDump.waitingThreads),
      buildThreadListPanel("timedWaitingThreads","Timed waiting threads", threadDump.timedWaitingThreads)
    )render
  }

  def buildThreadListPanel(panelId:String, panelName:String,threads: List[AppThread])={

    val (panelDivs, collapsablePanels) = (threads map buildThreadAccordion("runningTreads")).unzip

    val btnGrp = buildCollapseAndExpandButtons(collapsablePanels)

    div(`class`:="col-md-4")::div(`class`:="col-md-8", id:=panelId)(
      div(`class` := "panel panel-default")(
        div(`class` := "panel-heading")(
          h2(`class` := "panel-title")(s"$panelName (${threads.size})",btnGrp)
        ),
        div(`class` := "thread-listing panel-body", role := "tablist")(panelDivs)
      )
    )::Nil
  }

  def buildCollapseAndExpandButtons(collapsablePanels: List[Div])={
    val collapseBtn = button(
      `class`:= "btn btn-default btn-xs",
      onclick := { (e: Any) => collapseAll(collapsablePanels) }
    )(
      span(`class` := "glyphicon glyphicon-collapse-up")()
    )

    val expandBtn = button(
      `class`:= "btn btn-default btn-xs",
      onclick := { (e: Any) => expandAll(collapsablePanels) }
    )(
      span(`class` := "glyphicon glyphicon-collapse-down")()
    )

    div(`class`:= "btn-group pull-right")(collapseBtn, expandBtn)

  }


  def collapseAll(divs:List[Div])=
    divs.foreach { d =>
      if(d.classList contains "in") d.classList.remove ("in")
      if(! d.classList.contains("collapse")) d.classList.add ("collapse")

    }

  def expandAll(divs:List[Div]) =
    divs.foreach{ d=>
      if(! d.classList.contains("in")) d.classList.add ("in")
    }


  def buildThreadAccordion(groupId:String)(thread: AppThread)={
    val theId: String = groupId + thread.id
    val panelCollapse = buildPanelCollapse(thread, theId)

    (
      buildTabForCollapse(groupId, thread, theId, panelCollapse),
      panelCollapse
    )
  }

  def buildTabForCollapse(groupId: String, thread: AppThread, theId: String, panelCollapse: Div): TypedTag[Div] = {
    if (thread.stackTrace isEmpty)
      div(
        div(role := "tab")(
          span(`class` := "leading-tab")(bulletForThreadState(thread.state)), " ", thread.name)
      )
    else{
      val stack = thread.stackTrace.foldLeft("")((acc,e)=> s"$acc\n$e")
      val url = "http://grepcode.com/search/st?query=" + js.URIUtils.encodeURI(stack)
      div(
        div(role := "tab")(
          a("data-toggle".attr := "collapse", href := s"#$theId", "data-parent".attr := s"#$groupId")(
            span(`class` := "leading-tab")(bulletForThreadState(thread.state)), " ", thread.name
          ),
          "  ",
          if (thread.stackTrace isEmpty)
            " "
          else
            a(href:=url, target := "_blank")(img(src:="http://grepcode.com/static/app/images/favicon.ico"))
        ),
        panelCollapse
      )
    }
  }

  def buildPanelCollapse(thread: AppThread, theId: String): Div =
    div(id := theId, `class` := "panel-collapse collapse", role := "tabpanel")(
      div(`class` := "panel-body")(
        pre(thread.stackTrace map (_ + "\n"))
      )
    ) render

  val bulletForThreadState: PartialFunction[ThreadState, TypedTag[Span]]={
    case Runnable(_) => runningBullet
    case Blocked(_) => blockedBullet
    case Waiting(_) => waitingBullet
    case TimedWaiting => timedWaitingBullet
    case New => newBullet
  }
}


