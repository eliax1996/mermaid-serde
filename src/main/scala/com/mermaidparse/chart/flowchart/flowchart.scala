package com.mermaidparse.chart

import com.mermaidparse.chart.flowchart.NodeShape.Node

import scala.collection.immutable.SeqMap

package object flowchart {
  import LinkTypes._
  import eu.timepit.refined.auto._

  type SeqSet[T] = SeqMap[T, Unit]

  object SeqSet {
    def apply[T](): SeqSet[T] = empty

    def empty[T]: SeqSet[T] = SeqMap()
  }

  sealed trait RenderDirection

  sealed trait TopBottom extends RenderDirection
  final case object TB extends TopBottom
  final case object TD extends TopBottom
  final case object BT extends RenderDirection
  final case object RL extends RenderDirection
  final case object LR extends RenderDirection

  final case class FlowChart(
      direction: RenderDirection,
      nodes: Seq[Node],
      connections: Seq[Link],
      subGraph: Seq[SubGraph]
  )

  object FlowChart {
    def fromTuple(direction: RenderDirection)(
        data: (Seq[Node], Seq[Link], Seq[SubGraph])
    ): FlowChart =
      FlowChart(
        direction,
        (data._1 :++ data._2.flatMap(connection =>
          Seq(connection.source, connection.destination)
        )).distinct,
        data._2,
        data._3
      )
  }

  final case class SubGraph(
      name: String,
      nodes: Seq[Node],
      connections: Seq[Link],
      subGraph: Seq[SubGraph]
  )

  object SubGraph {
    def empty: SubGraph =
      SubGraph("", Seq.empty, Seq.empty, Seq.empty)

    def fromTuple(name: String)(
        tuple: (Seq[Node], Seq[Link], Seq[SubGraph])
    ): SubGraph = {
      val nodes = (tuple._1 :++ tuple._2.flatMap(connection =>
        Seq(connection.source, connection.destination)
      )).distinct

      SubGraph(name, nodes, tuple._2, tuple._3)
    }
  }
}
