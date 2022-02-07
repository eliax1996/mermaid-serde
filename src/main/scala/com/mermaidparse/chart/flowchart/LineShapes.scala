package com.mermaidparse.chart.flowchart

object LineShapes {
  sealed trait LineType

  final object Continuous extends LineType

  final object Thick extends LineType

  final object Dotted extends LineType
}
