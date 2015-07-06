package io.blep.tda

import scala.scalajs.js.annotation.JSExport

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

  sealed trait Thread{ def id:String}
  sealed trait AppThread  extends Thread{
    def name:String
    def state: ThreadState
    def stackTrace:Seq[String]
  }
  case class RunningThread(id:String,name:String,state: ThreadState, stackTrace:Seq[String]) extends AppThread
  case class WaitingThread(id:String,name:String,state: ThreadState, stackTrace:Seq[String]) extends AppThread
  case class BlockedThread(id:String,name:String,state: ThreadState, stackTrace:Seq[String]) extends AppThread
  case class NewThread(id:String, name:String, stackTrace:Seq[String]) extends AppThread{
    val state = New
  }
  case class TimedWaitingThread(id:String, name:String, stackTrace:Seq[String]) extends AppThread{
    val state = TimedWaiting
  }
  case class SysThread(id:String, name: String) extends Thread

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

    def addThread(id:String, header:String, state: ThreadState, frames:Seq[String]):ParsingThreads = state match {
      case Blocked(_) => ParsingThreads(dateStr,javaVersion,
        BlockedThread(id,header,state, frames)::blockedThreads,runningThreads,waitingThreads, timedWaitingThreads, newThreads, systemThreads)
      case Waiting(_) => ParsingThreads(dateStr,javaVersion,
        blockedThreads,runningThreads,WaitingThread(id,header,state, frames)::waitingThreads, timedWaitingThreads, newThreads, systemThreads)
      case TimedWaiting => ParsingThreads(dateStr,javaVersion,
        blockedThreads,runningThreads,waitingThreads, TimedWaitingThread(id,header, frames) :: timedWaitingThreads, newThreads, systemThreads)
      case New => ParsingThreads(dateStr,javaVersion,
        blockedThreads,runningThreads,waitingThreads, timedWaitingThreads, NewThread(id,header, frames)::newThreads, systemThreads)
      case Runnable(_) => ParsingThreads(dateStr,javaVersion,
        blockedThreads,RunningThread(id,header,state, frames)::runningThreads,waitingThreads, timedWaitingThreads, newThreads, systemThreads)
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
    case class ThreadHeader(header:String, id:String) extends ThreadParseStatus
    case class ParsingStack(header: String, id:String, stackFrames:List[String], state: ThreadState) extends ThreadParseStatus

    val threadStartLineRe = """^"([^"]+)" .*tid=([^ ]+).*""".r
    val stateRe = """^.*java.lang.Thread.State: (\w+).*$""".r



    def _parseThread(lines: List[String], status:ThreadParseStatus, builder: ParsingThreads):
    (ParsingThreads, List[String])= (lines, status) match{
      case (""::tail, StartParsingThread) => _parseThread(tail, StartParsingThread, builder)
      case (l::tail, StartParsingThread) => _parseThread(tail, parseHeader(l), builder)
      case (""::tail, ThreadHeader(header, id)) => (builder.addSysThread(SysThread(id, header)), tail)
      case (""::tail, NotAThread) => (builder, tail)
      case (l::tail, NotAThread) => _parseThread(tail, NotAThread, builder)
      case (l::tail, ThreadHeader(header, id)) => val (state, remaining, frames) = parseState(l, tail)
        _parseThread(remaining, ParsingStack(header, id, frames, state), builder)
      case (Nil, ThreadHeader(header, id)) => (builder.addSysThread(SysThread(id,header)), lines)
      case (""::tail, ParsingStack(header,id, frames, state)) => (builder.addThread(id,header, state, frames), tail)
      case (l::tail, ParsingStack(header,id, frames, state)) => _parseThread(tail, ParsingStack(header,id,l::frames, state), builder)
      case (Nil, ParsingStack(header,id,frames, state)) => (builder.addThread(id, header,state,frames), Nil)
      case (Nil, NotAThread) => (builder, Nil)
    }

    def parseState(line:String, stackFrames : List[String]): (ThreadState, List[String], List[String]) = line match {
      case stateRe(state) =>
        val (monitors, remaining, frames) = parseStackFrames(stackFrames)
        val value: ThreadState = ThreadState.parseState(state, monitors.reverse)
        (value, remaining, frames)
      case _ => throw new IllegalArgumentException(s"Unable to parse state for ${line}")
    }

    def parseHeader(line:String) = line match {
      case threadStartLineRe(name, id) => ThreadHeader(name, id)
      case _ => NotAThread
    }

    _parseThread(lines, StartParsingThread, builder)

  }

  private[tda] def parseStackFrames(stackFrames:List[String]):(List[Monitor], List[String], List[String])={

    def _parseStackFrames(frames: List[String], foundMonitors:List[Monitor], notMonitors:List[String])
    : (List[Monitor], List[String], List[String])=frames match{
      case ""::tail => (foundMonitors , frames, notMonitors.reverse)
      case Nil => (foundMonitors , Nil, notMonitors.reverse)
      case frame::tail => parseFrame(frame) match {
        case Left(m) => _parseStackFrames(tail,m::foundMonitors, notMonitors)
        case Right(f) => _parseStackFrames(tail,foundMonitors,f::notMonitors)
      }
    }

    _parseStackFrames(stackFrames, Nil, Nil)
  }

  val waitingToLockRe = """.*- waiting to lock <([^>]+)> \(a ([^\)]+).*""".r
  val lockedRe = """.*- locked <([^>]+)> \(a ([^\)]+).*""".r
  val waitingRe = """.*- waiting on <([^>]+)> \(a ([^\)]+).*""".r
  val waitingRe2 = """.*- parking to wait for +<([^>]+)> \(a ([^\)]+).*""".r

  def parseFrame(frame:String)= frame match {
    case waitingToLockRe(id, clazz) => Left(BlockingMonitor(id, clazz))
    case lockedRe(id, clazz) => Left(OwnedMonitor(id, clazz))
    case waitingRe(id, clazz) => Left(WaitedMonitor(id, clazz))
    case waitingRe2(id, clazz) => Left(WaitedMonitor(id, clazz))
    case _ => Right(frame)
  }

}

