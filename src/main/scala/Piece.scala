package chess

case class Piece(color: Color, role: Role) {

  def is(c: Color) = c == color
  def is(r: Role) = r == role
  def isNot(r: Role) = r != role

  def oneOf(rs: Set[Role]) = rs(role)

  def isMinor = oneOf(Set(Knight, Bishop))
  def isMajor = oneOf(Set(Queen, Rook))

  def forsyth: Char = if (color == White) role.forsythUpper else role.forsyth

  // attackable positions assuming empty board
  def eyes(from: Pos, to: Pos): Boolean = role match {
    case King | Antiking => from touches to
    case Queen  => (from onSameLine to) || (from onSameDiagonal to)
    case Rook   => from onSameLine to
    case Bishop => from onSameDiagonal to
    case Knight =>
      val xd = from xDist to
      val yd = from yDist to
      (xd == 1 && yd == 2) || (xd == 2 && yd == 1)
    case Pawn => (from xDist to) == 1 && (to.y - from.y) == {
      if (color.white) 1 else -1
    }
  }

  // movable positions assuming empty board
  def eyesMovable(from: Pos, to: Pos): Boolean = role match {
    case Pawn => eyes(from, to) || {
      (from ?| to) && {
        val dy = to.y - from.y
        if (color.white) (dy == 1 || (from.y == 2 && dy == 2))
        else (dy == -1 || (from.y == 7 && dy == -2))
      }
    }
    case _ => eyes(from, to)
  }

  override def toString = (color + "-" + role).toLowerCase
}
