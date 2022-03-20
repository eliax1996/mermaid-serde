package com.mermaidparse.chart.flowchart

import eu.timepit.refined.api.Refined
import eu.timepit.refined.string.MatchesRegex

trait NodeShapes {
  sealed trait NodeShape

  final case object Squared extends NodeShape
  final case object Rounded extends NodeShape
  final case object StadiumShaped extends NodeShape
  final case object SubroutineShaped extends NodeShape
  final case object Cylindrical extends NodeShape
  final case object Circular extends NodeShape
  final case object Asymmetric extends NodeShape
  final case object Rhombus extends NodeShape
  final case object Hexagon extends NodeShape
  final case object ParallelogramLtoR extends NodeShape
  final case object ParallelogramRtoL extends NodeShape
  final case object TrapezoidBtoT extends NodeShape
  final case object TrapezoidTtoB extends NodeShape

  type nodeIdRegex = "([A-Z]|[a-z])+[0-9]*"
  type ID = String Refined MatchesRegex[nodeIdRegex]

  final case class Node(id: ID, text: String, shape: NodeShape)
  object Node {
    def apply(id: ID, text: String): Node = Node(id, text, Squared)
    def apply(id: ID): Node = Node(id, id.value, Squared)
  }
}
