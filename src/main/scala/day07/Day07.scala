package day07

import cats.parse.Parser
import utils.ParserUtil
import scala.util.chaining._

import scala.annotation.tailrec
import scala.io.Source

object Day07 {

  // File system element
  sealed trait FSE {
    def name: String
  }

  object FSE {

    case class File(
        override val name: String,
        size: Int
    ) extends FSE

    case class Folder(
        override val name: String,
        contents: Vector[FSE]
    ) extends FSE

    val parser: Parser[FSE] = {
      val fileParser = for {
        size <- ParserUtil.int
        _ <- Parser.char(' ')
        name <- Parser.anyChar.rep0.map(_.mkString)
      } yield File(
        name = name,
        size = size
      )

      val dirParser = for {
        _ <- Parser.string("dir ")
        name <- Parser.anyChar.rep0.map(_.mkString)
      } yield Folder(name, Vector.empty)

      Parser.oneOf(
        List(
          fileParser,
          dirParser
        )
      )
    }

  }

  def addFSE(current: FSE, path: List[String], toAdd: Vector[FSE]): FSE =
    current match
      case file: FSE.File => file
      case folder @ FSE.Folder(name, contents) =>
        path match
          case step :: rest if step == name =>
            if rest.isEmpty then
              folder.copy(
                contents = folder.contents ++ toAdd
              )
            else
              folder.copy(
                contents = contents.map(addFSE(_, rest, toAdd))
              )
          case _ =>
            folder

  sealed trait Command

  object Command {
    case class Enter(name: String) extends Command
    case class Output(fse: FSE) extends Command
    case object Read extends Command
    case object Exit extends Command

    def isOutput(command: Command): Boolean =
      command match
        case Output(_) => true
        case _         => false

    val parser: Parser[Command] = {
      val exitParser = Parser.string("$ cd ..").map(_ => Command.Exit)
      val enterParser = for {
        _ <- Parser.string("$ cd ")
        name <- Parser.anyChar.rep0.map(_.mkString)
      } yield Command.Enter(name)

      val readParser = Parser.string("$ ls").map(_ => Command.Read)
      val outputParser = FSE.parser.map(Command.Output.apply)

      Parser.oneOf(
        List(
          exitParser,
          enterParser,
          readParser,
          outputParser
        )
      )
    }

  }

  def addViaCommands(commands: List[Command]): FSE = {
    val initial = FSE.Folder("/", Vector.empty)

    @tailrec
    def search(commands: List[Command], currentPath: List[String], fse: FSE): FSE =
      commands match
        case head :: next =>
          head match
            case Command.Enter(name) => search(next, currentPath :+ name, fse)
            case Command.Output(fse) =>
              // Should be subsumed by the Command.Read action
              search(next, currentPath, fse)
            case Command.Read =>
              val (outputs, rest) = next.span(Command.isOutput)
              search(
                commands = rest,
                currentPath = currentPath,
                fse = addFSE(
                  fse,
                  currentPath,
                  outputs.collect { case Command.Output(fse) => fse }.toVector
                )
              )
            case Command.Exit =>
              search(next, currentPath.dropRight(1), fse)
        case Nil => fse

    search(commands, currentPath = List.empty, initial)
  }

  val input: List[Command] = Source
    .fromResource("real/day07.txt")
    .getLines()
    .flatMap(Command.parser.parse(_).toOption.map(_._2))
    .toList

  def sizeOf(fse: FSE): BigInt =
    fse match
      case FSE.File(_, size) => BigInt(size)
      case FSE.Folder(_, contents) =>
        contents.map(sizeOf).sum

  def collectUpTo(fse: FSE, size: BigInt = BigInt(100000)): Vector[BigInt] = {
    def recur(fse: FSE): Vector[BigInt] =
      fse match
        case FSE.File(_, _) => Vector.empty
        case folder @ FSE.Folder(_, contents) =>
          val folderSize = sizeOf(folder)

          val subSizes = contents
            .flatMap(recur)
          if folderSize <= size then folderSize +: subSizes
          else subSizes

    recur(fse)
  }

  @main
  def solution1(): Unit =
    input
      .pipe(addViaCommands)
      .pipe(collectUpTo(_))
      .pipe(_.sum)
      .pipe(pprint.log(_))

  @main
  def solution2(): Unit =
    val fse = addViaCommands(input)
    val totalSize = sizeOf(fse)
    val unused = 70000000 - totalSize
    val upTo = collectUpTo(fse, totalSize)
    upTo.sorted
      .collectFirst { case size if unused + size >= 30000000 => size }
      .pipe(pprint.log(_))

}
