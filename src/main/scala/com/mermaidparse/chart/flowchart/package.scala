package com.mermaidparse.chart

import fastparse.Parsed
import fastparse.Parsed.Failure

package object flowchart extends LineTypes with LinkTypes {
  import eu.timepit.refined.auto._

  implicit class MermaidHelper(val sc: StringContext) extends AnyVal {
    def mermaid(args: Any*): FlowChart = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      var buf = new StringBuilder(strings.next())
      while (strings.hasNext) {
        buf.append(expressions.next())
        buf.append(strings.next())
      }
      val parsed =
        FlowChart.parseFlowChart(s"""${buf.toString()}""".stripMargin)

      if (!parsed.isSuccess) {
        val parsingError = parsed.asInstanceOf[Failure].trace(true).toString
        throw new IllegalArgumentException(
          s"the interpolated string isn't a valid Mermaid graph: [$parsingError]"
        )
      }

      parsed.get.value
    }
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
  ) {
    def render: String = Serializer.flowChartRender
      .render(0, this)
      .map(instruction =>
        Seq.fill(3 * instruction.indentation)(" ").mkString + instruction.text
      )
      .mkString("\n")
  }

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

    def parseFlowChart(stringRepresentation: String): Parsed[FlowChart] =
      fastparse.parse(stringRepresentation, Parser.flowChart(_))
  }

  final case class SubGraphName(id: Option[ID], name: String)

  object SubGraphName {
    def apply(id: ID, name: String): SubGraphName = SubGraphName(Some(id), name)
    def apply(name: String): SubGraphName = SubGraphName(None, name)
  }

  final case class SubGraph(
      name: SubGraphName,
      maybeDirection: Option[RenderDirection] = None,
      nodes: Seq[Node],
      connections: Seq[Link],
      subGraph: Seq[SubGraph]
  )

  object SubGraph {
    def empty(name: SubGraphName, direction: RenderDirection): SubGraph =
      SubGraph(name, Some(direction), Seq.empty, Seq.empty, Seq.empty)

    def empty(name: SubGraphName): SubGraph =
      SubGraph(name, None, Seq.empty, Seq.empty, Seq.empty)

    def fromTuple(name: SubGraphName, maybeDirection: Option[RenderDirection])(
        tuple: (Seq[Node], Seq[Link], Seq[SubGraph])
    ): SubGraph = {
      val nodes = (tuple._1 :++ tuple._2.flatMap(connection =>
        Seq(connection.source, connection.destination)
      )).distinct

      SubGraph(name, maybeDirection, nodes, tuple._2, tuple._3)
    }
  }
}
