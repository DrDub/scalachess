package chess
package variant

import Pos.posAt

abstract class Variant(
    val id: Int,
    val key: String,
    val name: String,
    val shortName: String,
    val title: String) {

  def standard = this == Standard
  def chess960 = this == Chess960
  def kingOfTheHill = this == KingOfTheHill
  def threeCheck = this == ThreeCheck
  def antichess = this == Antichess
  def atomic = this == Atomic

  def exotic = !standard

  def pieces: Map[Pos, Piece] = Variant.symmetricRank(
    IndexedSeq(Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook)
  )

  def isValidPromotion(promotion: Option[PromotableRole]) = promotion match {
    case None                                 => true
    case Some(Queen | Rook | Knight | Bishop) => true
    case _                                    => false
  }

  def validMoves(situation: Situation): Map[Pos, List[Move]] = situation.actors collect {
    case actor if actor.moves.nonEmpty => actor.pos -> actor.moves
  } toMap

  def move(situation: Situation, from: Pos, to: Pos, promotion: Option[PromotableRole]): Valid[Move] = {

    // Find the move in the variant specific list of valid moves
    def findMove(from: Pos, to: Pos) = situation.moves get from flatMap (_.find(_.dest == to))

    for {
      actor ← situation.board.actors get from toValid "No piece on " + from
      myActor ← actor.validIf(actor is situation.color, "Not my piece on " + from)
      m1 ← findMove(from, to) toValid "Piece on " + from + " cannot move to " + to
      m2 ← m1 withPromotion promotion toValid "Piece on " + from + " cannot promote to " + promotion
      m3 <- m2 validIf (isValidPromotion(promotion), "Cannot promote to " + promotion + " in this game mode")
    } yield m3
  }

  def staleMate(situation: Situation): Boolean = !situation.check && situation.moves.isEmpty

  // In most variants, the winner is the last player to have played and there is a possibility of either a traditional
  // checkmate or a variant end condition
  def winner(situation: Situation): Option[Color] =
    if (situation.checkMate || specialEnd(situation)) Some(!situation.color) else None

  def specialEnd(situation: Situation) = false

  def specialDraw(situation: Situation) = false

  def drawsOnInsufficientMaterial = true

  def finalizeMove(move: Move): Board = move.after

  // Some variants, such as atomic chess, give different properties to pieces by replacing them with
  // different piece objects
  def convertPiecesFromStandard(originalPieces: PieceMap): PieceMap = originalPieces

  def valid(board: Board, strict: Boolean) = {
    Color.all map board.rolesOf forall { roles =>
      ((roles count (_ == King)) == 1) :: {
        if (strict) List((roles count (_ == Pawn)) <= 8, roles.size <= 16) else Nil
      } forall identity
    }
  }

  def roles = List(Rook, Knight, King, Bishop, King, Queen, Pawn)

  def promotableRoles: List[PromotableRole] = List(Queen, Rook, Bishop, Knight)

  def rolesByForsyth: Map[Char, Role] = this.roles map { r => (r.forsyth, r) } toMap

  def rolesByPgn: Map[Char, Role] = this.roles map { r => (r.pgn, r) } toMap

  def rolesPromotableByPgn: Map[Char, PromotableRole] =
    promotableRoles map { r => (r.pgn, r) } toMap

  override def toString = s"Variant($name)"
}

object Variant {

  val all = List(Standard, Chess960, FromPosition, KingOfTheHill, ThreeCheck, Antichess, Atomic)
  val byId = all map { v => (v.id, v) } toMap

  val default = Standard

  def apply(id: Int): Option[Variant] = byId get id
  def orDefault(id: Int): Variant = apply(id) | default

  def byName(name: String): Option[Variant] =
    all find (_.name.toLowerCase == name.toLowerCase)

  def exists(id: Int): Boolean = byId contains id

  private[variant] def symmetricRank(rank: IndexedSeq[Role]): Map[Pos, Piece] =
    (for (y ← Seq(1, 2, 7, 8); x ← 1 to 8) yield {
      posAt(x, y) map { pos =>
        (pos, y match {
          case 1 => White - rank(x - 1)
          case 2 => White.pawn
          case 7 => Black.pawn
          case 8 => Black - rank(x - 1)
        })
      }
    }).flatten.toMap
}
