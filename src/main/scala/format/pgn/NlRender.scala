package chess
package format
package pgn

import Tag._
import Nag._
import chess.format.pgn.Parser.MoveParser

import scalaz.Validation._
import scalaz.Success


object NlRender {

  var locale: String = "en"

  var reduced: Boolean = false

  implicit class PgnNl(val p: Pgn) {

    def toNlString(locale: String = "en", reduced: Boolean = false): String =
      """%s%s""".format( if (reduced) "" else
        orderTags(p.tags).foldLeft("")((s: String, t: Tag) => "%s%s\n" format (s, t.toNlString(locale, reduced))),
        p.turns.map(t => t.toNlString(locale, reduced)) mkString "\n" ).trim
    
    private def orderTags(l: List[Tag]): List[Tag] =
      orderTags0(l, Tag.tagTypes)

    private def orderTags0(l: List[Tag], order: List[TagType]): List[Tag] = order match {
      case tagType :: rest => {
        def pred(t: Tag) = t.name.equals(tagType)
        (l filter pred) ++ orderTags0(l filterNot pred, rest)
      }
      case _ => l
    }
  }

  implicit class TurnNl(val t: Turn) {

    def toNlString(locale: String = "en", reduced: Boolean = false): String = {
      val moveStr = if (t.number < 9) "%s move" format nth(t.number - 1) else "move number %d" format t.number
      val umoveStr = moveStr.charAt(0).toUpper + moveStr.substring(1)
      return (t.white, t.black) match {
        case (Some(w), Some(b)) if w.isLong => "%s, white moves %sContinuing the %s, black moves %s" format (umoveStr, w.toNlString(locale, reduced), moveStr, b.toNlString(locale, reduced))
        case (Some(w), Some(b))             => "%s, white moves %sBlack moves %s" format (umoveStr, w.toNlString(locale, reduced), b.toNlString(locale, reduced))
        case (Some(w), None)                => "%s, white moves %s" format (umoveStr, w.toNlString(locale, reduced))
        case (None, Some(b))                => "Continuing the %s, black moves %s" format (moveStr, b.toNlString(locale, reduced))
        case _                              => ""
      }
    }

    val nth = Array("first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eight", "nineth")
  }

  implicit class MoveNl(val m: Move) {
    def toNlString(locale: String = "en", reduced: Boolean = false): String = {
      val san = nlString(m.san, locale)
      val nag = m.nag.flatMap(n => Nag(n)).map(n => ". " + n.toNlString(locale, reduced) + ". ").getOrElse("")
      val commentOrTime =
        if (!reduced && (m.comment.isDefined || m.timeLeft.isDefined || m.opening.isDefined || m.result.isDefined))
          List(m.timeLeft.map(time => m.timeString(time)), m.opening, m.result, m.comment)
          .flatten.mkString(if(nag.isEmpty)". "else"", ". ", ". ")
        else ""
      val variationString = if (m.variation.isEmpty) "" else m.variation.mkString("", ". ", ". ")
      val result = s"$san$nag$commentOrTime$variationString"
      if(result.endsWith(". "))result else s"$result. "
    }

    def nlString(san: String, locale: String = "en"): String = MoveParser(san, variant.Standard) match {
      case Success(Std(dest, role, capture, file, rank, check, checkmate, promotion)) => 
        role.toString.toLowerCase() + " " + 
        (if(file.isDefined || rank.isDefined)"in "+(if(file.isDefined)(96 + file.get).asInstanceOf[Char] else String.valueOf(rank.get)) + " " else "") +
        "to " + dest.toString +
        (if(capture)" and it captures a piece"else"") +
        (if(promotion.isDefined)", promoting it to" + promotion.get.toString.toLowerCase else "") +
        (if(check)", the opponent is now in check"else"") +
        (if(checkmate)", the opponent has been checkmated"else"")
      case _ => san
    }
    
  }

  implicit class NagNl(val n: Nag) {
    def toNlString(locale: String = "en", reduced: Boolean = false): String = n match {
      case Good       => "Good move"
      case Mistake    => "Potential mistake"
      case Brilliant  => "Brilliant move"
      case Blunder    => "Very bad move"
      case Inaccuracy => "Potentially inaccurate move"
      case o          => o.toString
    }
  }

  implicit class TagNl(val t: Tag) {
    def toNlString(locale: String = "en", reduced: Boolean = false): String = t match {
      case Tag(Event, v)       => "Game held within the event %s." format v
      case Tag(Site, v)        => "Game held at %s." format v
      case Tag(Date, v)        => "Game held at %s." format v
      case Tag(White, v)       => "Whites played by %s." format v
      case Tag(Black, v)       => "Blacks played by %s." format v
      case Tag(TimeControl, v) => "Time controlled as %s." format v
      case Tag(WhiteClock, v)  => "Final clock for whites was %s." format v
      case Tag(BlackClock, v)  => "Final clock for blacks was %s." format v
      case Tag(WhiteElo, v)    => "The player plaing whites has an ELO of %s." format v
      case Tag(BlackElo, v)    => "The player plaing blacks has an ELO of %s." format v
      case Tag(Result, v)      => "The game concluded with %s." format v
      case Tag(FEN, v)         => "FEN information available."
      case Tag(Variant, v)     => "The variant played was %s." format v
      case Tag(ECO, v)         => "ECO information available."
      case Tag(Opening, v)     => "The game starts with the opening of %s." format v
      case Tag(Unknown(n), v)  => "Information about %s is also available." format n
    }
  }
}