package com.mermaidparse.chart.flowchart

object LineDirections {
  object TipTypes {
    sealed trait TipType

    final case object Standard extends TipType
    final case object Crossed extends TipType
    final case object Circle extends TipType
  }

  import TipTypes._

  sealed trait Direction {
    def tipType: Option[TipType]
  }

  final object WithoutDirection extends Direction {
    override def tipType: Option[TipType] = None
  }

  sealed trait ArrowDirection extends Direction {
    def tipType: Some[TipType]
  }

  final case class LeftToRight(tipType: Some[TipType]) extends ArrowDirection

  final case class RightToLeft(tipType: Some[TipType]) extends ArrowDirection

  final case class MultipleDirection(tipType: Some[TipType]) extends Direction
}
