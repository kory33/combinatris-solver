package io.github.kory33.combinatris.core

import scala.annotation.tailrec
import cats.data.NonEmptyList

type BracketSize = 2 | 3

sealed trait Letter:
  def asCompleteTerm: CompleteTerm = CompleteTerm.Combinator(this)
  override def toString(): String =
    this match
      case Letter.S => "S"
      case Letter.K => "K"
      case Letter.I => "I"
      case Letter.Y => "Y"
object Letter:
  case object S extends Letter
  case object K extends Letter
  case object I extends Letter
  case object Y extends Letter

enum GameInput:
  case LetterInput(letter: Letter)
  case BracketInput(spaces: BracketSize)
object GameInput:
  val all: List[GameInput] =
    List(
      LetterInput(Letter.S),
      LetterInput(Letter.K),
      LetterInput(Letter.I),
      LetterInput(Letter.Y),
      BracketInput(2),
      BracketInput(3)
    )

sealed trait CompleteTerm {
  override def toString(): String =
    this match
      case CompleteTerm.Combinator(letter) =>
        letter.toString()
      case CompleteTerm.Bracket(size, args) =>
        "(" + args.map(_.toString()).mkString("") + ")"
}
object CompleteTerm:
  case class Combinator(letter: Letter) extends CompleteTerm
  case class Bracket(size: BracketSize, args: /* .length == size */ List[CompleteTerm])
      extends CompleteTerm {
    assert(args.length == size)
  }

  given Extensions: AnyRef with
    extension (ct: CompleteTerm)
      def length: Int =
        ct match
          case CompleteTerm.Combinator(_) => 1
          case CompleteTerm.Bracket(size, args) =>
            1 /* left bracket */ + args.toList.map(_.length).sum + 1 /* right bracket */
import CompleteTerm._

case class BracketWithSpaceSomewhere(
  size: BracketSize,
  rightmostSpace: SpaceOrBracketWithSpaceSomewhere,
  filledRest: /* .length < size */ List[CompleteTerm]
) {
  assert(filledRest.length < size)

  def length: Int =
    1 /* left bracket */ +
      (size - filledRest.length - 1) /* non-rightmost spaces */ +
      rightmostSpace.map(_.length).getOrElse(1) /* rightmost space */ +
      filledRest.map(_.length).sum /* filled rest */ +
      1 /* right bracket */

  def slideInInputWithoutReduction(input: GameInput)
    : Either[CompleteTerm, BracketWithSpaceSomewhere] = rightmostSpace match
    case Some(innerBracketWithSpace) =>
      innerBracketWithSpace.slideInInputWithoutReduction(input) match
        case Left(completeTerm) =>
          if filledRest.length + 1 == size then
            Left(CompleteTerm.Bracket(size, completeTerm :: filledRest))
          else
            Right(copy(
              rightmostSpace = None,
              filledRest = completeTerm :: filledRest
            ))
        case Right(newInnerBracket) =>
          Right(copy(rightmostSpace = Some(newInnerBracket)))
    case None =>
      input match
        case GameInput.LetterInput(letter) =>
          val term = CompleteTerm.Combinator(letter)
          if filledRest.length + 1 == size then
            Left(CompleteTerm.Bracket(size, term :: filledRest))
          else
            Right(copy(rightmostSpace = None, filledRest = term :: filledRest))
        case GameInput.BracketInput(spaces) =>
          Right(copy(rightmostSpace = Some(BracketWithSpaceSomewhere.emptyBracketOf(spaces))))

  override def toString(): String =
    "(" + " ".repeat(size - filledRest.length - 1) +
      rightmostSpace.map(_.toString()).getOrElse(" ") +
      filledRest.map(_.toString()).mkString("") +
      ")"
}
type SpaceOrBracketWithSpaceSomewhere = Option[BracketWithSpaceSomewhere]

object BracketWithSpaceSomewhere:
  def emptyBracketOf(spaces: BracketSize): BracketWithSpaceSomewhere =
    BracketWithSpaceSomewhere(spaces, None, Nil)

sealed trait /* Irreducible */ DenseLine {
  def asTermList: List[CompleteTerm] =
    this match
      case DenseLine.S(args) => Letter.S.asCompleteTerm :: args
      case DenseLine.K(args) => Letter.K.asCompleteTerm :: args
      case DenseLine.I       => List(Letter.I.asCompleteTerm)
      case DenseLine.Y       => List(Letter.Y.asCompleteTerm)
      case DenseLine.Empty   => Nil

  override def toString(): String =
    this match
      case DenseLine.S(args) =>
        "S" + args.map(_.toString()).mkString("")
      case DenseLine.K(args) =>
        "K" + args.map(_.toString()).mkString("")
      case DenseLine.I =>
        "I"
      case DenseLine.Y =>
        "Y"
      case DenseLine.Empty =>
        ""
}
object DenseLine:
  case class S(args: /* .length < 3 */ List[CompleteTerm]) extends DenseLine {
    assert(args.length < 3)
  }
  case class K(args: /* .length < 2 */ List[CompleteTerm]) extends DenseLine {
    assert(args.length < 2)
  }
  case object I extends DenseLine
  case object Y extends DenseLine
  case object Empty extends DenseLine

  given Extensions: AnyRef with
    extension (dl: DenseLine)
      def length: Int =
        dl match
          case S(args) =>
            1 /* S */ + args.toList.map(_.length).sum
          case K(args) =>
            1 /* K */ + args.toList.map(_.length).sum
          case I | Y =>
            1
          case Empty =>
            0

  def containing(letter: Letter): DenseLine =
    letter match
      case Letter.S => S(Nil)
      case Letter.K => K(Nil)
      case Letter.I => I
      case Letter.Y => Y

enum GameOver:
  case DetectedInfiniteLoop()
  case Overflowed()

val maxTermSize = 30

type LineUndergoingReduction = NonEmptyList[CompleteTerm]
@tailrec
def attemptLeftmostReduction(
  toReduce: LineUndergoingReduction,
  linesSeenSoFar: Set[LineUndergoingReduction] = Set(),
  historyOfLines: List[LineUndergoingReduction] = Nil
): (List[LineUndergoingReduction], Either[GameOver, /* .length <= maxTermSize */ DenseLine]) = {
  val updatedHistory = toReduce :: historyOfLines

  if (linesSeenSoFar.contains(toReduce)) {
    (updatedHistory, Left(GameOver.DetectedInfiniteLoop()))
  } else if (toReduce.map(_.length).toList.sum > maxTermSize) {
    (updatedHistory, Left(GameOver.Overflowed()))
  } else {
    // format: off
    toReduce match {
      case NonEmptyList(Combinator(letter), Nil) =>
        (updatedHistory, Right(DenseLine.containing(letter)))
      case NonEmptyList(Combinator(Letter.I), nextTerm :: rest) =>
        attemptLeftmostReduction(NonEmptyList(nextTerm, rest), linesSeenSoFar + toReduce, updatedHistory)
      case NonEmptyList(Combinator(Letter.K), firstTerm :: secondTerm :: rest) =>
        attemptLeftmostReduction(NonEmptyList(firstTerm, rest), linesSeenSoFar + toReduce, updatedHistory)
      case NonEmptyList(Combinator(Letter.K), tail) =>
        (updatedHistory, Right(DenseLine.K(tail.toList)))
      case NonEmptyList(Combinator(Letter.S), firstTerm :: secondTerm :: thirdTerm :: rest) =>
        attemptLeftmostReduction(
          NonEmptyList(firstTerm, thirdTerm :: CompleteTerm.Bracket(2, List(secondTerm, thirdTerm)) :: rest),
          linesSeenSoFar + toReduce,
          updatedHistory
        )
      case NonEmptyList(Combinator(Letter.S), tail) =>
        (updatedHistory, Right(DenseLine.S(tail.toList)))
      case NonEmptyList(Combinator(Letter.Y), firstTerm :: rest) =>
        attemptLeftmostReduction(
          NonEmptyList(firstTerm, CompleteTerm.Bracket(2, List(Combinator(Letter.Y), firstTerm)) :: rest),
          linesSeenSoFar + toReduce,
          updatedHistory
        )
      case NonEmptyList(Bracket(size, args), tail) =>
        attemptLeftmostReduction(
          NonEmptyList.fromListUnsafe(args) ++ tail,
          linesSeenSoFar + toReduce,
          updatedHistory
        )
    }
    // format: on
  }
}

case class StableLine(bracket: Option[BracketWithSpaceSomewhere], densePart: DenseLine) {
  def length: Int = bracket.map(_.length).getOrElse(0) + densePart.length

  def checkLength: Either[GameOver, StableLine] =
    if length > maxTermSize then Left(GameOver.Overflowed())
    else Right(this)

  override def toString(): String =
    bracket.map(_.toString()).getOrElse("") + densePart.toString()

  def prependInputAndReduce(input: GameInput)
    : ( /* Some if line becomes complete on input */ Option[List[LineUndergoingReduction]], Either[GameOver, StableLine]) =
    bracket match {
      case None =>
        input match
          case GameInput.LetterInput(letter) =>
            val (reductionHistory, r) =
              attemptLeftmostReduction(
                NonEmptyList(letter.asCompleteTerm, densePart.asTermList)
              )
            (Some(reductionHistory), r.map(StableLine(None, _)))
          case GameInput.BracketInput(spaces) =>
            (
              None,
              Right(StableLine(
                Some(BracketWithSpaceSomewhere.emptyBracketOf(spaces)),
                densePart
              ))
            )
      case Some(bracketWithSpaceSomewhere) =>
        bracketWithSpaceSomewhere.slideInInputWithoutReduction(input) match
          case Left(completeTerm) =>
            val (reductionHistory, r) =
              attemptLeftmostReduction(NonEmptyList(completeTerm, densePart.asTermList))
            (Some(reductionHistory), r.map(StableLine(None, _)))
          case Right(newBracket) =>
            (None, StableLine(Some(newBracket), densePart).checkLength)
    }
}

object StableLine:
  def empty: StableLine = StableLine(None, DenseLine.Empty)

trait PrintCombinatrisLines:
  def printLineWithLineString(lineString: String): Unit =
    if (lineString.length > 30) then
      println("    |" + lineString.take(30) + "|")
    else
      println("    |" + " ".repeat(30 - lineString.length) + lineString + "|")
  def printStableLine(line: StableLine): Unit =
    printLineWithLineString(line.toString())
  def printLineUndergoingReduction(lineUndergoingReduction: LineUndergoingReduction): Unit =
    printLineWithLineString(lineUndergoingReduction.map(_.toString()).toList.mkString(""))

object CombinatrisGame extends PrintCombinatrisLines:
  def main(args: Array[String]): Unit = {
    var currentLine = StableLine.empty

    def appendInput(input: GameInput): Unit =
      val (history, result) = currentLine.prependInputAndReduce(input)
      history.foreach(_.reverse.foreach(printLineUndergoingReduction))

      result match
        case Left(GameOver.DetectedInfiniteLoop()) =>
          println("Detected infinite loop!")
          println("-----------------------")
          printStableLine(currentLine)
        case Left(GameOver.Overflowed()) =>
          println("Overflowed!")
          println("-----------------------")
          printStableLine(currentLine)
        case Right(newLine) =>
          currentLine = newLine
          // no history was output, so the current line is not yet printed
          if (history.isEmpty) then printStableLine(currentLine)

    while true do
      print(" > ")
      val input = scala.io.StdIn.readLine()
      input match {
        case "exit" =>
          return
        case "S" =>
          appendInput(GameInput.LetterInput(Letter.S))
        case "K" =>
          appendInput(GameInput.LetterInput(Letter.K))
        case "I" =>
          appendInput(GameInput.LetterInput(Letter.I))
        case "Y" =>
          appendInput(GameInput.LetterInput(Letter.Y))
        case "2" =>
          appendInput(GameInput.BracketInput(2))
        case "3" =>
          appendInput(GameInput.BracketInput(3))
        case _ =>
          println("Invalid input!")
      }
  }

object CombinatrisChainReactionBFSExplorer extends PrintCombinatrisLines:
  def main(args: Array[String]): Unit = {
    val searchNext = collection.mutable.Queue[StableLine]()

    searchNext.enqueue(StableLine.empty)

    var maxChainReactionFound = 0

    while (searchNext.nonEmpty) {
      val nextLineToTry = searchNext.dequeue()
      GameInput.all.foreach { input =>
        val (history, result) = nextLineToTry.prependInputAndReduce(input)
        result match {
          case Left(_) => // do nothing
          case Right(newLine) =>
            history.foreach { reductionHistory =>
              val chainReactionLength = reductionHistory.length - 1
              if (chainReactionLength > maxChainReactionFound) {
                maxChainReactionFound = chainReactionLength
                println(s"Found chain reaction of length $chainReactionLength")
                reductionHistory.reverse.foreach(printLineUndergoingReduction(_))
              }
            }

            if (history.forall(_.length == 1)) {
              // We never record a result of reduction, so both
              // newLine nor nextLineToTry are result of construction
              // without reduction.
              // It follows that there is only one way to reach newLine
              // from StableLine.empty, so we can enqueue newLine
              // without checking if it is already enqueued.
              //
              // By not recording the result of reductions, we are not visiting
              // some configurations that can be reached by reduction.
              // For instance, S(SS) can be reached by
              //    SSS -(+S)> SSSS ~(reduce)> SS(SS) -(+K)> KSS(SS) ~(reduce)> S(SS)
              // or
              //    ( (  )) -(+S)> ... -(+S)> (S(SS)) ~(reduce)> S(SS)
              // but not without reduction.
              // The second path is actually an essence to how we can
              // construct an arbitrary DenseLine (that is of length <= 28),
              // which is to put everything in a bracket and then fill in the top-level
              // space to complete the construction.
              searchNext.enqueue(newLine)
            }
        }
      }
    }
  }
