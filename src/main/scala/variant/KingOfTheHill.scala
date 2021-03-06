package chess
package variant

case object KingOfTheHill extends Variant(
  id = 4,
  key = "kingOfTheHill",
  name = "King of the Hill",
  shortName = "KotH",
  title = "Bring your king to the center to win the game") {

  private val center = Set(Pos.D4, Pos.D5, Pos.E4, Pos.E5)

  override def specialEnd(situation: Situation) =
    situation.board.kingPosOf(!situation.color) exists center.contains

  override def drawsOnInsufficientMaterial = false
}

