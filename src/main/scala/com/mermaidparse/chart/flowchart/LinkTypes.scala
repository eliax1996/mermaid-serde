package com.mermaidparse.chart.flowchart

import com.mermaidparse.chart.flowchart.LineDirections.TipTypes.TipType
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.string.NonEmptyString
import eu.timepit.refined.auto._

object LinkTypes {

  import LineDirections._
  import LineShapes._
  import NodeShape._

  final case class ConnectionType(
      direction: Direction,
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

    import LineDirections._

    val continuousLine: ConnectionType =
      ConnectionType(LineDirections.WithoutDirection, LineShapes.Continuous)

    def continuousArrow(direction: ArrowDirection): ConnectionType =
      ConnectionType(direction, LineShapes.Continuous)

    def continuousArrow(tipType: TipType): ConnectionType =
      continuousArrow(
        LeftToRight(Some(tipType))
      )
  }

}
