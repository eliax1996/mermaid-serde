package com.mermaidparse.chart

import com.mermaidparse.chart.flowchart.NodeShape.Node

import scala.collection.immutable.SeqMap

package object flowchart {
  import LinkTypes._
  import eu.timepit.refined.auto._

  type SeqSet[T] = SeqMap[T, Unit]

  object SeqSet {
    def apply[T](): SeqSet[T] = empty()

    def empty[T](): SeqSet[T] = SeqMap()
  }

  sealed trait RenderDirection

  sealed trait TopBottom extends RenderDirection
  final object TB extends TopBottom
  final object TD extends TopBottom
  final object BT extends RenderDirection
  final object RL extends RenderDirection
  final object LR extends RenderDirection

  final case class FlowChart(
      direction: RenderDirection,
      nodes: SeqSet[Node],
      connections: SeqSet[Link],
      subGraph: SeqSet[FlowChart]
  )

  object FlowChart {
    def apply(direction: RenderDirection, nodes: SeqSet[Node]): FlowChart =
      FlowChart(direction, nodes, SeqSet(), SeqSet())

    def apply(
        direction: RenderDirection,
        nodes: SeqSet[Node],
        connections: SeqSet[Link]
    ): FlowChart =
      FlowChart(direction, nodes, connections, SeqSet())
  }
}
