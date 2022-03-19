package com.mermaidparse.chart.flowchart

object LineShapes {
  sealed trait LineType

  final case object Continuous extends LineType
  final case object Thick extends LineType
  final case object Dotted extends LineType
}
