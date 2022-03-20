package com.mermaidparse.chart.flowchart

trait LineTypes {
  sealed trait LineType

  final case object Continuous extends LineType
  final case object Thick extends LineType
  final case object Dotted extends LineType
}
