package com.mermaidparse.chart.flowchart

import com.mermaidparse.chart.flowchart.LineDirections.TipTypes.{
  Crossed,
  Standard
}
import com.mermaidparse.chart.flowchart.LineDirections._
import com.mermaidparse.chart.flowchart.LineShapes.{
  Continuous,
  Dotted,
  LineType,
  Thick
}
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._
import eu.timepit.refined.refineV
import eu.timepit.refined.string.MatchesRegex
import eu.timepit.refined.types.string.NonEmptyString
import fastparse.NoWhitespace._
import fastparse._

object Parser {
  def ws[_: P]: P[Unit] = P(" ".rep(1))

  def flowChartHeader[_: P]: P[RenderDirection] = (P("graph") ~ ws ~ P(
    "TB" | "TD" | "BT" | "RL" | "LR"
  ).! ~ ws.? ~ P(";")).map {
    case "TB" => TB
    case "TD" => TD
    case "BT" => BT
    case "RL" => RL
    case "LR" => LR
  }

  import NodeShape._

  def parseNodeIdOrFail(nodeId: String): NodeId = {
    refineV[MatchesRegex[nodeIdRegex]].unsafeFrom(nodeId)
  }

  def word[_: P]: P[String] = (CharIn("A-Z") | CharIn("a-z")).rep(1).!

  def nodeId[_: P]: P[NodeId] =
    (word ~ CharIn("0-9").rep).!.map(parseNodeIdOrFail)

  case class ShapeAndText(text: String, shape: NodeShape)

  def oneSizeShape[_: P]: P[ShapeAndText] =
    (P("[") ~ textOfShape ~ P("]")).map(ShapeAndText(_, Squared)) |
      (P("(") ~ textOfShape ~ P(")")).map(ShapeAndText(_, Rounded)) |
      (P(">") ~ textOfShape ~ P("]")).map(ShapeAndText(_, Asymmetric)) |
      (P("{") ~ textOfShape ~ P("}")).map(ShapeAndText(_, Rhombus))

  def twoSizeShape[_: P]: P[ShapeAndText] =
    (P("[[") ~ textOfShape ~ P("]]")).map(ShapeAndText(_, SubroutineShaped)) |
      (P("[(") ~ textOfShape ~ P(")]")).map(ShapeAndText(_, Cylindrical)) |
      (P("((") ~ textOfShape ~ P("))")).map(ShapeAndText(_, Circle)) |
      (P("{{") ~ textOfShape ~ P("}}")).map(ShapeAndText(_, Hexagon)) |
      (P("[/") ~ textOfShape ~ P("/]"))
        .map(ShapeAndText(_, ParallelogramLtoR)) |
      (P("[\\") ~ textOfShape ~ P("\\]"))
        .map(ShapeAndText(_, ParallelogramRtoL)) |
      (P("[/") ~ textOfShape ~ P("\\]")).map(ShapeAndText(_, TrapezoidBtoT)) |
      (P("[\\") ~ textOfShape ~ P("/]")).map(ShapeAndText(_, TrapezoidTtoB))

  def textOfShape[_: P]: P[String] =
    (ws.? ~ word ~ ws.?).rep.! // todo add allowed symbols in word

  def nodeShapeAndText[_: P]: P[ShapeAndText] = twoSizeShape | oneSizeShape

  def nodeDefinition[_: P]: P[Node] =
    (nodeId ~ nodeShapeAndText).map { case (id, st) =>
      Node(id, st.text, st.shape)
    }

  import LinkTypes._

  case class LinkAttr(
      text: Option[NonEmptyString],
      connection: ConnectionType,
      length: Int Refined Positive = 1
  )

  object LinkAttr {
    def fromArrowBodyAndDirection(
        arrowBody: ArrowBody,
        direction: Direction
    ): LinkAttr = LinkAttr(
      arrowBody.text,
      connection = ConnectionType(direction, arrowBody.lineType),
      arrowBody.length
    )
  }

  def nodeIdWithSpace[_: P]: P[NodeId] =
    ws.rep ~ nodeId ~ ws.rep

  def textOfLink[_: P]: P[String] =
    (ws.? ~ word ~ ws.?).rep(1).! // todo add allowed symbols in word

  def linkMapper(
      connectionType: ConnectionType
  ): PartialFunction[
    (String, String, String),
    LinkAttr
  ] = { case (left, text, right) =>
    LinkAttr(
      text = NonEmptyString.from(text).map(Some(_)).getOrElse(None),
      connection = connectionType,
      length = refineV[Positive].unsafeFrom(left.length + right.length)
    )
  }

  def linkAttrParserGen[_: P](
      startParser: => P[_],
      middleParser: => P[_]
  ): P[_] = {
    startParser ~ middleParser ~ startParser
  }

  case class ArrowBody(
      text: Option[NonEmptyString],
      lineType: LineType,
      length: Int Refined Positive
  )

  def linkWithArrowParser[_: P](
      arrowBodyParser: => P[ArrowBody]
  ): P[LinkAttr] = {
    ("<" ~ arrowBodyParser ~ ">").map(
      LinkAttr.fromArrowBodyAndDirection(_, MultipleDirection(Some(Standard)))
    ) |
      ("<" ~ arrowBodyParser).map(
        LinkAttr.fromArrowBodyAndDirection(_, RightToLeft(Some(Standard)))
      ) |
      (arrowBodyParser ~ ">").map(
        LinkAttr.fromArrowBodyAndDirection(_, LeftToRight(Some(Standard)))
      ) |
      ("o" ~ arrowBodyParser ~ "o").map(
        LinkAttr.fromArrowBodyAndDirection(
          _,
          MultipleDirection(Some(TipTypes.Circle))
        )
      ) |
      ("o" ~ arrowBodyParser).map(
        LinkAttr.fromArrowBodyAndDirection(
          _,
          RightToLeft(Some(TipTypes.Circle))
        )
      ) |
      (arrowBodyParser ~ "o").map(
        LinkAttr.fromArrowBodyAndDirection(
          _,
          LeftToRight(Some(TipTypes.Circle))
        )
      ) |
      ("x" ~ arrowBodyParser ~ "x").map(
        LinkAttr.fromArrowBodyAndDirection(_, MultipleDirection(Some(Crossed)))
      ) |
      ("x" ~ arrowBodyParser).map(
        LinkAttr.fromArrowBodyAndDirection(_, RightToLeft(Some(Crossed)))
      ) |
      (arrowBodyParser ~ "x").map(
        LinkAttr.fromArrowBodyAndDirection(_, LeftToRight(Some(Crossed)))
      ) |
      arrowBodyParser
        .map(LinkAttr.fromArrowBodyAndDirection(_, WithoutDirection))
  }

  def arrowBodyParser[_: P](
      bodyParser: => P[_],
      lineType: LineType
  ): P[ArrowBody] = {
    bodyParser
      .rep(1)
      .!
      .map(arrow =>
        ArrowBody(
          text = None,
          lineType = lineType,
          length = refineV[Positive].unsafeFrom(arrow.length)
        )
      )
  }

  def arrowWithTextBodyParser[_: P](
      arrowBodyChar: => P[_],
      lineType: LineType
  ): P[ArrowBody] = {
    (arrowBodyChar.rep(2).! ~ textOfShape.! ~ arrowBodyChar.rep(2).!)
      .map { case (beginArrow, text, endArrow) =>
        ArrowBody(
          Some(NonEmptyString.unsafeFrom(text)),
          lineType,
          refineV[Positive].unsafeFrom(beginArrow.length + endArrow.length)
        )
      }
  }

  def linkAttrParser[_: P]: P[LinkAttr] = {
    linkWithArrowParser(arrowWithTextBodyParser(P("-"), Continuous)) |
      linkWithArrowParser(arrowWithTextBodyParser(P("="), Thick)) |
      // todo: this one accept a wider language
      linkWithArrowParser(arrowWithTextBodyParser(P("-") | P("."), Dotted)) |
      linkWithArrowParser(arrowBodyParser(P("-"), Continuous)) |
      linkWithArrowParser(arrowBodyParser(P("="), Thick)) |
      // todo: this one accept a wider language
      linkWithArrowParser(arrowBodyParser(P("-") | P("."), Dotted))
  }

  def connection[_: P]: P[Link] =
    (nodeIdWithSpace ~ linkAttrParser ~ nodeIdWithSpace).log.map {
      case (source: NodeId, attr: LinkAttr, destination: NodeId) =>
        Link(
          Node(source),
          Node(destination),
          attr.text,
          attr.connection,
          attr.length
        )
    }

}
