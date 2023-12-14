package io.github.kory33.combinatoris.core

import scala.compiletime.ops.int._
import scala.annotation.tailrec
import cats.data.NonEmptyList
import io.github.kory33.combinatoris.core.CompleteTerm.Combinator
import io.github.kory33.combinatoris.core.CompleteTerm.Bracket

type None = None.type
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
  linesSeenSoFar: Set[LineUndergoingReduction] = Set()
): Either[GameOver, /* .length <= maxTermSize */ DenseLine] = {
  if (linesSeenSoFar.contains(toReduce)) {
    Left(GameOver.DetectedInfiniteLoop())
  } else if (toReduce.map(_.length).toList.sum > maxTermSize) {
    Left(GameOver.Overflowed())
  } else {
    // format: off
    toReduce match {
      case NonEmptyList(Combinator(letter), Nil) =>
        Right(DenseLine.containing(letter))
      case NonEmptyList(Combinator(Letter.I), nextTerm :: rest) =>
        attemptLeftmostReduction(NonEmptyList(nextTerm, rest), linesSeenSoFar + toReduce)
      case NonEmptyList(Combinator(Letter.K), firstTerm :: secondTerm :: rest) =>
        attemptLeftmostReduction(NonEmptyList(firstTerm, rest), linesSeenSoFar + toReduce)
      case NonEmptyList(Combinator(Letter.K), tail) =>
        Right(DenseLine.K(tail.toList))
      case NonEmptyList(Combinator(Letter.S), firstTerm :: secondTerm :: thirdTerm :: rest) =>
        attemptLeftmostReduction(
          NonEmptyList(firstTerm, thirdTerm :: CompleteTerm.Bracket(2, List(secondTerm, thirdTerm)) :: rest),
          linesSeenSoFar + toReduce
        )
      case NonEmptyList(Combinator(Letter.S), tail) =>
        Right(DenseLine.S(tail.toList))
      case NonEmptyList(Combinator(Letter.Y), firstTerm :: rest) =>
        attemptLeftmostReduction(
          NonEmptyList(firstTerm, CompleteTerm.Bracket(2, List(Combinator(Letter.Y), firstTerm)) :: rest),
          linesSeenSoFar + toReduce
      )
      case NonEmptyList(Bracket(size, args), tail) =>
        attemptLeftmostReduction(
          NonEmptyList.fromListUnsafe(args) ++ tail,
          linesSeenSoFar + toReduce
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

  def prependInputAndReduce(input: GameInput): Either[GameOver, StableLine] =
    bracket match {
      case None =>
        input match
          case GameInput.LetterInput(letter) =>
            attemptLeftmostReduction(NonEmptyList(letter.asCompleteTerm, densePart.asTermList))
              .map(StableLine(None, _))
          case GameInput.BracketInput(spaces) =>
            Right(StableLine(
              Some(BracketWithSpaceSomewhere.emptyBracketOf(spaces)),
              DenseLine.Empty
            ))
      case Some(bracketWithSpaceSomewhere) =>
        bracketWithSpaceSomewhere.slideInInputWithoutReduction(input) match
          case Left(completeTerm) =>
            attemptLeftmostReduction(NonEmptyList(completeTerm, densePart.asTermList))
              .map(StableLine(None, _))
          case Right(newBracket) =>
            StableLine(Some(newBracket), densePart).checkLength
    }
}

object StableLine:
  def empty: StableLine = StableLine(None, DenseLine.Empty)
