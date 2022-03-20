package com.mermaidparse.chart.flowchart

trait TipTypes {
  sealed trait TipType

  final case object Standard extends TipType
  final case object Crossed extends TipType
  final case object Circle extends TipType
}
