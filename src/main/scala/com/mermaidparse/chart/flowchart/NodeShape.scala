package com.mermaidparse.chart.flowchart

import eu.timepit.refined.api.Refined
import eu.timepit.refined.string.MatchesRegex

object NodeShape {
  sealed trait NodeShape

  final object Squared extends NodeShape
  final object Rounded extends NodeShape
  final object StadiumShaped extends NodeShape
  final object SubroutineShaped extends NodeShape
  final object Cylindrical extends NodeShape
  final object Circle extends NodeShape
  final object Asymmetric extends NodeShape
  final object Rhombus extends NodeShape
  final object Hexagon extends NodeShape
  final object ParallelogramLtoR extends NodeShape
  final object ParallelogramRtoL extends NodeShape
  final object TrapezoidBtoT extends NodeShape
  final object TrapezoidTtoB extends NodeShape

  type nodeIdRegex = "([A-Z]|[a-z])+[0-9]*"
  type NodeId = String Refined MatchesRegex[nodeIdRegex]

  final case class Node(id: NodeId, text: String, shape: NodeShape)
  object Node {
    def apply(id: NodeId, text: String): Node = Node(id, text, Squared)
    def apply(id: NodeId): Node = Node(id, id.value, Squared)
  }
}
