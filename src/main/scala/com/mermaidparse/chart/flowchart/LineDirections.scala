package com.mermaidparse.chart.flowchart

trait LineDirections extends TipTypes {

  sealed trait LineDirection {
    def tipType: Option[TipType]
  }

  final object WithoutDirection extends LineDirection {
    override def tipType: Option[TipType] = None
  }
  sealed trait ArrowDirection extends LineDirection {
    def tipType: Some[TipType]
  }
  final case class LeftToRight(tipType: Some[TipType]) extends ArrowDirection
  final case class RightToLeft(tipType: Some[TipType]) extends ArrowDirection
  final case class MultipleDirection(tipType: Some[TipType]) extends LineDirection
}
