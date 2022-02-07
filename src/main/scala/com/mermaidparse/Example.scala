package com.mermaidparse

import com.mermaidparse.chart.flowchart.LinkTypes

import scala.collection.immutable.SeqMap

object Example extends App {
  import LinkTypes._
  import eu.timepit.refined.auto._

  type SeqSet[T] = SeqMap[T, Unit]

  sealed trait RenderDirection

  sealed trait TB extends RenderDirection
  final object TD extends TB
  final object BT extends RenderDirection
  final object RL extends RenderDirection
  final object LR extends RenderDirection

  final case class FlowChart(
      direction: RenderDirection,
      connection: SeqSet[Link]
  )

  println(
    SeqMap[Int, Unit](
      (1, ()),
      (120120, ()),
      (1, ()),
      (10, ()),
      (11, ()),
      (10, ())
    )
  )

}
