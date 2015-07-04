package io.blep.tda

import io.blep.tda.ThreadDumpAnalyzer.ThreadDump
import org.scalajs.dom.html.Div

import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._

object View {
  val runningBullet = span(`class`:="glyphicon glyphicon-repeat running-thread")
  val blockedBullet = span(`class`:="glyphicon glyphicon-remove-circle blocked-thread")
  val waitingBullet = span(`class`:="glyphicon glyphicon-time waiting-thread")

  val resultContainer = div.render

  def listThreads(threadDump: ThreadDump): Unit ={
    resultContainer.appendChild(p(s"Thread count: ${threadDump.threads.size}").render)
    resultContainer.appendChild(
      div(`class`:="row")(
        div(`class`:="col-md-4 thread-listing panel panel-default")(
          h2(s"Running threads (${threadDump.runningThreads.size})"),
          for(t <- threadDump.runningThreads)yield div(runningBullet, s" ${t.name}", br)
        ),
        div(`class`:="col-md-4 thread-listing panel panel-default")(
          h2(s"Blocked threads (${threadDump.blockedThreads.size})"),
          for(t <- threadDump.blockedThreads)yield div(blockedBullet, s" ${t.name}", br)
        ),
        div(`class`:="col-md-4 thread-listing panel panel-default")(
          h2(s"Waiting threads (${threadDump.waitingThreads.size})"),
          for(t <- threadDump.waitingThreads)yield div(waitingBullet, s" ${t.name}", br)
        )
      ) render
    )
  }

  def buildAlert(msg: String) = div(cls := "alert alert-danger alert-dismissible fade in")(
    button(`type` := "button", `class` := "close", "data-dismiss".attr := "alert") {
      span("x")
    },
    s"Parsing error : ${msg}"
  ) render

  def displaySharedLocks(threadDump: ThreadDump)={
    resultContainer.appendChild(
      h2(s"Hot monitors (${threadDump.sharedLocks.size})") render
    )
    val tags: List[Div] = for (l <- threadDump.sharedLocks) yield {
      div(
        s"${l.monitor.id}: ${l.monitor.clazz}", br,
        s"Owned by : ", br,
        div(`class`:="leading-tab")(runningBullet, s" ${l.ownedBy.name}"),
        s"Locks ${l.blockedThreads.size} threads : ", br,
        div(`class`:="thread-listing")(
          for (t <- l.blockedThreads) yield
          div(`class`:="leading-tab")(blockedBullet, s" ${t.name}")
        )
      ) render
    }
    tags foreach resultContainer.appendChild
  }


}

