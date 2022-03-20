package com.mermaidparse.chart.flowchart

import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.string.NonEmptyString

trait LinkTypes extends LineDirections with LineTypes with NodeShapes {

  final case class ConnectionType(
                                   direction: LineDirection,
                                   lineType: LineType
  )

  final case class Link(
      source: Node,
      destination: Node,
      text: Option[NonEmptyString],
      connection: ConnectionType,
      length: Int Refined Positive = 1
  )

  object predef {

    val continuousLine: ConnectionType =
      ConnectionType(WithoutDirection, Continuous)

    def continuousArrow(direction: ArrowDirection): ConnectionType =
      ConnectionType(direction, Continuous)

    def continuousArrow(tipType: TipType): ConnectionType =
      continuousArrow(
        LeftToRight(Some(tipType))
      )
  }

}
