package io.blep.tda

import io.blep.tda.ThreadDumpAnalyzer._
import io.blep.tda.BootstrapView.resultContainer
import org.scalajs.dom.html.{Span, Div}

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
  }

  def buildGeneralInfoPanel={
    div(`class` := "panel panel-default affix affix-top")(
      div(`class` := "panel-heading")(h2(`class` := "panel-title")("General information")),
      div(`class` := "thread-listing panel-body")(
        table(`class`:="info")(
          tr(
            td("Dump date"),td(threadDump.dateStr)
          ),
          tr(
            td("Java version"),td(threadDump.javaVersion)
          ),
          tr(
            td("All threads"),td(threadDump.threads.size)
          ),
          tr(
            td(a(href := "#runningThreads")(runningBullet, " Running threads")),td(threadDump.runningThreads.size)
          ),
          tr(
            td(a(href:="#blockedThreads")(blockedBullet," Blocked threads")),td(threadDump.blockedThreads.size)
          ),
          tr(
            td(a(href:="#waitingThreads")(waitingBullet," Waiting threads")),td(threadDump.waitingThreads.size)
          ),
          tr(
            td(a(href:="#timedWaitingThreads")(timedWaitingBullet," Timed waiting threads")),td(threadDump.timedWaitingThreads.size)
          ),
          tr(
            td(newBullet," New threads"),td(threadDump.newThreads.size)
          ),
          tr(
            td("System threads"),td(threadDump.systemThreads.size)
          )
        )
      )
    ) render

  }

  def buildSharedLockPanel={
    val panel: Div = div(`class` := "panel panel-default")(
      div(`class` := "panel-heading")(h2(`class` := "panel-title")(s"Hot monitors (${threadDump.sharedLocks.size})"))
    ) render
    val tags: List[Div] = for (l <- threadDump.sharedLocks) yield {
      val theId: String = l.monitor.id
      div(`class`:="panel-group", id := theId , role:="tablist")(
        s"${l.monitor.id}: ${l.monitor.clazz}", br,
        s"Owned by : ", br,
        buildThreadAccordion(theId)(l.ownedBy),
        s"Locks ${l.blockedThreads.size} threads : ", br,
        div(`class`:="thread-listing", id:=s"blockedBy$theId}")(
          l.blockedThreads map buildThreadAccordion(theId)
        )
      ) render
    }

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

  def buildThreadListPanel(panelId:String, panelName:String,threads: List[AppThread])=
    div(`class`:="col-md-4")::div(`class`:="col-md-8", id:=panelId)(
      div(`class` := "panel panel-default")(
        div(`class` := "panel-heading")(h2(`class` := "panel-title")(s"$panelName (${threads.size})")),
        div(`class` := "thread-listing panel-body", id:="runningTreads", role:="tablist")(
          threads map buildThreadAccordion("runningTreads")
        )
      )
    )::Nil

  def buildThreadAccordion(groupId:String)(thread: AppThread)={
    val theId: String = groupId + thread.id
    div(`class`:= "")(
      div(role:="tab")(
        a("data-toggle".attr := "collapse", href := s"#$theId", "data-parent".attr := s"#$groupId")(
          span(`class`:="leading-tab")(bulletForThreadState(thread.state)), " ", thread.name
        )
      ),
      div(id := theId, `class`:="panel-collapse collapse", role:="tabpanel")(
        div(`class`:="panel-body")(
          pre(thread.stackTrace map (_ + "\n"))
        )
      )
    )
  }

  val bulletForThreadState: PartialFunction[ThreadState, TypedTag[Span]]={
    case Runnable(_) => runningBullet
    case Blocked(_) => blockedBullet
    case Waiting(_) => waitingBullet
    case TimedWaiting => timedWaitingBullet
    case New => newBullet
  }
}


