package io.blep.tda

import io.blep.tda.ThreadDumpAnalyzer.ThreadDump
import org.scalajs.dom.html.Div

import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._

object View {
  val runningBullet = span(`class`:="glyphicon glyphicon-repeat running-thread")
  val blockedBullet = span(`class`:="glyphicon glyphicon-remove-circle blocked-thread")
  val waitingBullet = span(`class`:="glyphicon glyphicon-time waiting-thread")
  val newBullet = span(`class`:="glyphicon glyphicon-time waiting-thread")

  val resultContainer = div.render
  
  def displayResults(threadDump: ThreadDump)={
    resultContainer.appendChild(
      div(`class` := "row")(
        div(`class` := "col-md-6")(
          buildGeneralInfoPanel(threadDump)
        ),
        div(`class` := "col-md-6")(
          buildSharedLockPanel(threadDump)
        )
      ) render
    )

    resultContainer.appendChild(listThreads(threadDump))
  }

  def buildGeneralInfoPanel(threadDump: ThreadDump)={
    div(`class` := "panel panel-default")(
      div(`class` := "panel-heading")(h2(`class` := "panel-title")("General information")),
      div(`class` := "thread-listing panel-body")(
        strong("Dump date: "), threadDump.dateStr, br,
        strong("Java version: "), threadDump.javaVersion, br,
        strong("Number of threads: "), threadDump.threads.size, br,
        span(`class`:="leading-tab")(runningBullet)," Running threads: ", threadDump.runningThreads.size, br,
        span(`class`:="leading-tab")(blockedBullet)," Blocked threads: ", threadDump.blockedThreads.size, br,
        span(`class`:="leading-tab")(waitingBullet)," Waiting threads: ", threadDump.waitingThreads.size, br,
        span(`class`:="leading-tab")(waitingBullet)," Timed waiting threads: ", threadDump.timedWaitingThreads.size, br,
        span(`class`:="leading-tab")(newBullet)," New threads: ", threadDump.newThreads.size, br,
        strong("Number of system threads: "), threadDump.systemThreads.size, br
      )
    ) render

  }

  def buildSharedLockPanel(threadDump: ThreadDump)={
    val panel: Div = div(`class` := "panel panel-default")(
      div(`class` := "panel-heading")(h2(`class` := "panel-title")(s"Hot monitors (${threadDump.sharedLocks.size})"))
    ) render
    val tags: List[Div] = for (l <- threadDump.sharedLocks) yield {
      div(
        s"${l.monitor.id}: ${l.monitor.clazz}", br,
        s"Owned by : ", br,
        div(`class`:="leading-tab")(runningBullet, s" ${l.ownedBy.name}"),
        s"Locks ${l.blockedThreads.size} threads : ", br,
        div(`class`:="thread-listing")(
          for (t <- l.blockedThreads) yield div(`class`:="leading-tab")(blockedBullet, s" ${t.name}")
        )
      ) render
    }

    tags foreach panel.appendChild
    panel
  }


  def listThreads(threadDump: ThreadDump) ={
      div(`class`:="row")(
        div(`class`:="col-md-4")(
          div(`class` := "panel panel-default")(
            div(`class` := "panel-heading")(h2(`class` := "panel-title")(s"Running threads (${threadDump.runningThreads.size})")),
            div(`class` := "thread-listing panel-body")(
              for (t <- threadDump.runningThreads) yield div(runningBullet, s" ${t.name}", br)
            )
          )
        ),
        div(`class`:="col-md-4")(
          div(`class` := "panel panel-default")(
            div(`class` := "panel-heading")(h2(`class` := "panel-title")(s"Blocked threads (${threadDump.blockedThreads.size})")),
            div(`class` := "thread-listing panel-body")(
              for (t <- threadDump.blockedThreads) yield div(blockedBullet, s" ${t.name}", br)
            )
          )
        ),
        div(`class`:="col-md-4")(
          div(`class` := "panel panel-default")(
            div(`class` := "panel-heading")(h2(`class` := "panel-title")(s"Waiting threads (${threadDump.waitingThreads.size})")),
            div(`class` := "thread-listing panel-body")(
              for (t <- threadDump.waitingThreads) yield div(waitingBullet, s" ${t.name}", br)
            )
          )
        )
      )render
  }

  def buildAlert(msg: String) = div(cls := "alert alert-danger alert-dismissible fade in")(
    button(`type` := "button", `class` := "close", "data-dismiss".attr := "alert") {
      span("x")
    },
    s"Parsing error : ${msg}"
  ) render



}

