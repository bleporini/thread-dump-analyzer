package io.blep.tda

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.{JSExportAll, JSExport}

object ThreadDumpAnalyzer {
//  @JSExportAll
  case class ThreadDump(dateStr: String,
                        javaVersion : String,
                        blockedThreads: List[BlockedThread],
                        runningThreads: List[RunningThread],
                        waitingThreads: List[WaitingThread],
                        timedWaitingThreads: List[TimedWaitingThread],
                        newThreads: List[NewThread],
                         systemThreads: List[SysThread]){

    val nonBlockedThreads = runningThreads ++ waitingThreads ++ timedWaitingThreads ++ newThreads

    val appThreads = blockedThreads ++ nonBlockedThreads

    val threads = appThreads ++ systemThreads

    val sharedLocks = nonBlockedThreads.map(t=>findSharingThreads(t, blockedThreads)).flatten

    private def findSharingThreads(nonBlocked:AppThread,allBlocked:List[BlockedThread]): List[SharedLock] ={
      nonBlocked.state.monitors
        .filter(_.isInstanceOf[OwnedMonitor])
        .map(m => (m, allBlocked.filter(_.state.monitors.head.id == m.id)))
        .filter(_._2 != Nil)
        .map{case (m, ts)=> SharedLock(m,nonBlocked,ts )}

    }
  }

  sealed trait Thread
  abstract class AppThread(val name:String, val state: ThreadState) extends Thread
  case class RunningThread(override val name:String, override val state: ThreadState) extends AppThread(name, state)
  case class WaitingThread(override val name:String, override val state: ThreadState) extends AppThread(name, state)
  case class BlockedThread(override val name:String, override val state: ThreadState) extends AppThread(name, state)
  case class NewThread(override val name:String) extends AppThread(name, New)
  case class TimedWaitingThread(override val name:String) extends AppThread(name,TimedWaiting)
  case class SysThread(name: String) extends Thread

  object ThreadState {

    def parseState(s:String, monitors : List[Monitor]) = (s, monitors) match{
      case ("NEW", _)=> New
      case ("RUNNABLE", l) => Runnable(l)
      case ("BLOCKED", l)=> Blocked (l)
      case ("WAITING",l)=> Waiting(l)
      case ("TIMED_WAITING", _) => TimedWaiting
    }
  }

  abstract class ThreadState(val state:String, val monitors:List[Monitor])
  case object New extends ThreadState("NEW", Nil)
  case class Runnable(override val monitors:List[Monitor]) extends ThreadState("RUNNABLE", monitors)
  case class Blocked(override val monitors:List[Monitor]) extends ThreadState("BLOCKED", monitors)
  case class Waiting(override val monitors:List[Monitor]) extends ThreadState("WAITING", monitors)
  case object TimedWaiting extends ThreadState("TIMED_WAITING", Nil)

  sealed trait Monitor{
    def id:String
    def clazz:String
  }
  case class BlockingMonitor(override val id:String, override val clazz:String) extends Monitor
  case class OwnedMonitor(override val id:String, override val clazz:String) extends Monitor
  case class WaitedMonitor(override val id:String, override val clazz:String) extends Monitor

  case class SharedLock(monitor:Monitor, ownedBy:AppThread, blockedThreads:List[AppThread])

  private val versionRe = """^Full thread dump ([^:]+):.*$""".r
  private[tda] def parseVersion(line: String)= line match{
    case versionRe(version) => version
    case _ => "Unknown version"
  }


  abstract class ThreadDumpParseStatus
  case object StartParsing extends ThreadDumpParseStatus
  case class DateParsed(dateStr :String) extends ThreadDumpParseStatus
  case class ParsingThreads(dateStr: String, javaVersion: String,
                            blockedThreads: List[BlockedThread] = Nil,
                            runningThreads: List[RunningThread] = Nil,
                            waitingThreads: List[WaitingThread] = Nil,
                            timedWaitingThreads: List[TimedWaitingThread] =Nil,
                            newThreads: List[NewThread] = Nil,
                            systemThreads: List[SysThread] = Nil) extends ThreadDumpParseStatus{
    def addSysThread(st:SysThread):ParsingThreads = ParsingThreads(dateStr,javaVersion,
      blockedThreads,runningThreads,waitingThreads,timedWaitingThreads, newThreads, st::systemThreads)

    def addThread(header:String, state: ThreadState):ParsingThreads = state match {
      case Blocked(_) => ParsingThreads(dateStr,javaVersion,
        BlockedThread(header,state)::blockedThreads,runningThreads,waitingThreads, timedWaitingThreads, newThreads, systemThreads)
      case Waiting(_) => ParsingThreads(dateStr,javaVersion,
        blockedThreads,runningThreads,WaitingThread(header,state)::waitingThreads, timedWaitingThreads, newThreads, systemThreads)
      case TimedWaiting => ParsingThreads(dateStr,javaVersion,
        blockedThreads,runningThreads,waitingThreads, TimedWaitingThread(header) :: timedWaitingThreads, newThreads, systemThreads)
      case New => ParsingThreads(dateStr,javaVersion,
        blockedThreads,runningThreads,waitingThreads, timedWaitingThreads, NewThread(header)::newThreads, systemThreads)
      case Runnable(_) => ParsingThreads(dateStr,javaVersion,
        blockedThreads,RunningThread(header,state)::runningThreads,waitingThreads, timedWaitingThreads, newThreads, systemThreads)
    }
  }
  @JSExport
  def parseDump(dumpStr: String):ThreadDump= {

    def _parseDump(lines:List[String], builder:ThreadDumpParseStatus):ThreadDump =
      (lines, builder) match{
        case (l::tail, StartParsing)=> _parseDump(tail,DateParsed(l))
        case (l::tail, DateParsed(dateStr)) => _parseDump(tail, ParsingThreads(dateStr, parseVersion(l)))
        case (Nil, ParsingThreads(dateStr, javaVersion, bs, rs, ws, ts, ns, ss)) =>
          ThreadDump(dateStr, javaVersion, bs, rs, ws, ts, ns, ss)
        case (lst, ParsingThreads(_, _, _, _,_,_,_,_)) => val (status, remaining) = parseThread(lst, builder.asInstanceOf[ParsingThreads])
          _parseDump(remaining, status)
      }

    val lines = dumpStr.split("\n").toList

    _parseDump(lines, StartParsing)

  }

  private[tda] def parseThread(lines: List[String], builder:ParsingThreads):(ParsingThreads, List[String])={
    abstract class ThreadParseStatus
    case object StartParsingThread extends ThreadParseStatus
    case object NotAThread extends ThreadParseStatus
    case class ThreadHeader(header:String) extends ThreadParseStatus
    case class ParsingStack(header: String, stackFrames:List[String], state: ThreadState) extends ThreadParseStatus

    val threadStartLineRe = """^"([^"]+)" .*""".r
    val stateRe = """^.*java.lang.Thread.State: (\w+).*$""".r



    def _parseThread(lines: List[String], status:ThreadParseStatus, builder: ParsingThreads):
    (ParsingThreads, List[String])= (lines, status) match{
      case (""::tail, StartParsingThread) => _parseThread(tail, StartParsingThread, builder)
      case (l::tail, StartParsingThread) => _parseThread(tail, parseHeader(l), builder)
      case (""::tail, ThreadHeader(header)) => (builder.addSysThread(SysThread(header)), tail)
      case (""::tail, NotAThread) => (builder, tail)
      case (l::tail, NotAThread) => _parseThread(tail, NotAThread, builder)
      case (l::tail, ThreadHeader(header)) => val (state, remaining) = parseState(l, tail)
        _parseThread(remaining, ParsingStack(header, l::Nil, state), builder)
      case (Nil, ThreadHeader(header)) => (builder.addSysThread(SysThread(header)), lines)
      case (""::tail, ParsingStack(header,frames, state)) => (builder.addThread(header, state), tail)
      case (l::tail, ParsingStack(header,frames, _)) => _parseThread(tail, status, builder)
      case (Nil, ParsingStack(header,frames, state)) => (builder.addThread(header,state), Nil)
      case (Nil, NotAThread) => (builder, Nil)
    }

    def parseState(line:String, stackFrames : List[String]): (ThreadState, List[String]) = line match {
      case stateRe(state) =>
        val (monitors, remaining) = parseStackFrames(stackFrames)
        val value: ThreadState = ThreadState.parseState(state, monitors.reverse)
        (value, remaining)
      case _ => throw new IllegalArgumentException(s"Unable to parse state for ${line}")
    }

    def parseHeader(line:String) = line match {
      case threadStartLineRe(name) => ThreadHeader(name)
      case _ => NotAThread
    }

    _parseThread(lines, StartParsingThread, builder)

  }

  private[tda] def parseStackFrames(stackFrames:List[String]):(List[Monitor], List[String])={

    def _parseStackFrames(frames: List[String], foundMonitors:List[Option[Monitor]]): (List[Monitor], List[String])=frames match{
      case ""::tail => (foundMonitors flatten, frames)
      case Nil => (foundMonitors flatten, Nil)
      case frame::tail => _parseStackFrames(tail, parseFrame.lift(frame)::foundMonitors)
    }

    _parseStackFrames(stackFrames, Nil)
  }

  val waitingToLockRe = """.*- waiting to lock <([^>]+)> \(a ([^\)]+).*""".r
  val lockedRe = """.*- locked <([^>]+)> \(a ([^\)]+).*""".r
  val waitingRe = """.*- waiting on <([^>]+)> \(a ([^\)]+).*""".r
  val waitingRe2 = """.*- parking to wait for +<([^>]+)> \(a ([^\)]+).*""".r

  def parseFrame: PartialFunction[String,Monitor]={
    case waitingToLockRe(id, clazz) => BlockingMonitor(id, clazz)
    case lockedRe(id, clazz) => OwnedMonitor(id, clazz)
    case waitingRe(id, clazz) => WaitedMonitor(id, clazz)
    case waitingRe2(id, clazz) => WaitedMonitor(id, clazz)
  }

}

