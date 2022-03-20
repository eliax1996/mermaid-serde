package com.mermaidparse.chart.flowchart

import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._
import eu.timepit.refined.refineV

object Serializer {

  type Indentation = Int Refined NonNegative

  trait Renderer[T] {
    def render(indentation: Int, t: T): List[Instruction]
  }

  class Instruction(val text: String, val indentation: Int Refined NonNegative)

  object Instruction {
    def apply(indentation: Int, text: String): Instruction = new Instruction(
      text,
      refineV[NonNegative].unsafeFrom(indentation)
    )
  }

  implicit val nodeRenderer: Renderer[Node] =
    (indentation: Int, node: Node) => {
      val text = node.shape match {
        case Squared           => s"[${node.text}])"
        case Rounded           => s"(${node.text})"
        case Asymmetric        => s">${node.text}]"
        case Rhombus           => s"{${node.text}}"
        case SubroutineShaped  => s"[[${node.text}]]"
        case Cylindrical       => s"[(${node.text})]"
        case Circular          => s"((${node.text}))"
        case Hexagon           => s"{{${node.text}}}"
        case ParallelogramLtoR => s"[/${node.text}/])"
        case ParallelogramRtoL => s"[\\${node.text}\\])"
        case TrapezoidBtoT     => s"[/${node.text}\\])"
        case TrapezoidTtoB     => s"[\\${node.text}/])"
        case StadiumShaped     => s"([${node.text}])"
      }
      List(Instruction(indentation, text))
    }

  implicit val linkRender: Renderer[Link] =
    (indentation: Int, link: Link) => {

      val headTailLength = link.connection.direction match {
        case WithoutDirection                => -1
        case LeftToRight(_) | RightToLeft(_) => 0
        case MultipleDirection(_)            => 1
      }

      val bodyLength = link.length.value - headTailLength

      // todo: manage text

      val body =
        (link.connection.lineType match {
          case Continuous => Seq.fill(bodyLength)("-")
          case Thick      => Seq.fill(bodyLength)("=")
          case Dotted     => Seq.fill(bodyLength - 1)(".") :+ "-"
        }).mkString

      val connectionBody = link.connection.direction match {
        case MultipleDirection(Some(Standard)) => s"<${body}>"
        case RightToLeft(Some(Standard))       => s"<${body}"
        case LeftToRight(Some(Standard))       => s"${body}>"
        case MultipleDirection(Some(Circle))   => s"o${body}o"
        case RightToLeft(Some(Circle))         => s"o${body}"
        case LeftToRight(Some(Circle))         => s"${body}o"
        case MultipleDirection(Some(Crossed))  => s"x${body}x"
        case RightToLeft(Some(Crossed))        => s"x${body}"
        case LeftToRight(Some(Crossed))        => s"${body}x"
        case WithoutDirection                  => body
      }

      List(
        Instruction(
          indentation,
          s"${link.source.id} ${connectionBody} ${link.destination.id}"
        )
      )
    }

  implicit val subGraphRender: Renderer[SubGraph] =
    (indentation: Int, subGraph: SubGraph) => {

      val name = subGraph.name
      val subGraphName =
        if (name.id.isDefined) {
          Instruction(indentation, s"${name.id.get} [${name.name}]")
        } else {
          Instruction(indentation, s"${name.name}")
        }

      val nodeDeclarations =
        subGraph.nodes
          .filter(node => node.id.value != node.text)
          .map { node =>
            val text =
              s"${node.id.value}${nodeRenderer.render(indentation + 1, node)}"
            Instruction(indentation + 1, text)
          }
          .toList

      val connections = subGraph.connections
        .flatMap(connection => linkRender.render(indentation + 1, connection))
        .toList

      val subGraphsRendered =
        subGraph.subGraph
          .flatMap(subGraph => subGraphRender.render(indentation + 1, subGraph))
          .toList

      val directionInstruction: List[Instruction] = subGraph.maybeDirection
        .fold[List[Instruction]](List.empty)(direction =>
          List(
            Instruction(
              indentation + 1,
              s"direction ${direction.toString}"
            )
          )
        )

      val header = Instruction(
        indentation,
        s"subgraph ${subGraphName.text}"
      )
      val tail = Instruction(
        indentation,
        "end"
      ) :: Nil

      header :: directionInstruction ::: nodeDeclarations ::: connections ::: subGraphsRendered ::: tail
    }

  implicit val flowChartRender: Renderer[FlowChart] =
    (indentation: Int, flowChart: FlowChart) => {

      val nodeDeclarations =
        flowChart.nodes
          .filter(node => node.id.value != node.text)
          .map { node =>
            val text =
              s"${node.id.value}${nodeRenderer.render(indentation + 1, node)}"
            Instruction(indentation + 1, text)
          }
          .toList

      val connections = flowChart.connections
        .flatMap(connection => linkRender.render(indentation + 1, connection))
        .toList

      val subGraphsRendered =
        flowChart.subGraph
          .flatMap(subGraph => subGraphRender.render(indentation + 1, subGraph))
          .toList

      val header = Instruction(indentation, s"flowchart ${flowChart.direction}")

      header :: nodeDeclarations ::: connections ::: subGraphsRendered ::: Nil
    }

}
