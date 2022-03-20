package com.mermaidparse.chart.flowchart

import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._
import eu.timepit.refined.refineV
import eu.timepit.refined.string.MatchesRegex
import eu.timepit.refined.types.string.NonEmptyString
import fastparse.NoWhitespace._
import fastparse._

private[flowchart] object Parser {

  def ws[_: P]: P[Unit] = P(" ").rep(1)

  def nl[_: P]: P[Unit] = (ws.rep ~ P("\n") ~ ws.rep).rep(1)

  def renderDirection[_: P]: P[RenderDirection] = P(
    "TB" | "TD" | "BT" | "RL" | "LR"
  ).!.map {
    case "TB" => TB
    case "TD" => TD
    case "BT" => BT
    case "RL" => RL
    case "LR" => LR
  }

  def flowChartHeader[_: P]: P[RenderDirection] =
    nl.rep ~ (P("graph") | P("flowchart")) ~ ws ~
      renderDirection ~ ws.? ~ P(";").?

  def parseNodeIdOrFail(nodeId: String): ID =
    refineV[MatchesRegex[nodeIdRegex]].unsafeFrom(nodeId)

  def word[_: P]: P[String] =
    (CharIn("A-Z") | CharIn("a-z")).rep(1).!

  def nodeId[_: P]: P[ID] =
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
      (P("((") ~ textOfShape ~ P("))")).map(ShapeAndText(_, Circular)) |
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

  case class LinkAttr(
      text: Option[NonEmptyString],
      connection: ConnectionType,
      length: Int Refined Positive = 1
  )

  object LinkAttr {
    def fromArrowBodyAndDirection(
        arrowBody: ArrowBody,
        direction: LineDirection
    ): LinkAttr = LinkAttr(
      arrowBody.text,
      connection = ConnectionType(direction, arrowBody.lineType),
      arrowBody.length
    )
  }

  def nodeIdWithSpace[_: P]: P[ID] =
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
          MultipleDirection(Some(Circle))
        )
      ) |
      ("o" ~ arrowBodyParser).map(
        LinkAttr.fromArrowBodyAndDirection(
          _,
          RightToLeft(Some(Circle))
        )
      ) |
      (arrowBodyParser ~ "o").map(
        LinkAttr.fromArrowBodyAndDirection(
          _,
          LeftToRight(Some(Circle))
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
    (nodeIdWithSpace ~ linkAttrParser ~ nodeIdWithSpace).map {
      case (source: ID, attr: LinkAttr, destination: ID) =>
        Link(
          Node(source),
          Node(destination),
          attr.text,
          attr.connection,
          attr.length
        )
    }

  case class SubGraphHeader(
      subGraphName: SubGraphName,
      maybeRenderDirection: Option[RenderDirection]
  )

  def stringInBrackets[_: P]: P[String] =
    (P("[") ~
      (ws | CharIn("0-9") | CharIn("a-z") | CharIn("A-Z")).rep
      ~ P("]")).!

  def subGraphHeader[_: P]: P[SubGraphHeader] =
    (
      P("subgraph") ~ ws.rep(1) ~
        (
          nodeId ~ (ws.rep(1) ~ (word | stringInBrackets)).?
            ~ nl.rep(1)
        ) ~ (P("direction") ~ ws ~ renderDirection ~ nl.rep(1)).?
    ).map {
      case (
            idOrName: ID,
            maybeName: Option[String],
            maybeRenderDirection: Option[RenderDirection]
          ) =>
        if (maybeName.isDefined) {
          SubGraphHeader(
            SubGraphName(idOrName, maybeName.get),
            maybeRenderDirection
          )
        } else {
          SubGraphHeader(SubGraphName(idOrName), maybeRenderDirection)
        }
    }

  def subGraphTail[_: P]: P[Unit] = P("end") ~ ws.rep

  def subGraph[_: P]: P[SubGraph] =
    (
      subGraphHeader ~
        (
          (nodeDefinition ~ nl) |
            (connection ~ nl) |
            (subGraph ~ nl)
        ).rep ~ subGraphTail
    ).map { case (subgraphHeader: SubGraphHeader, data: Seq[Product]) =>
      SubGraph.fromTuple(
        subgraphHeader.subGraphName,
        subgraphHeader.maybeRenderDirection
      )(
        data.foldLeft(
          (Seq.empty[Node], Seq.empty[Link], Seq.empty[SubGraph])
        )((tuple, data) => {
          data match {
            case node: Node         => tuple.copy(_1 = tuple._1 :+ node)
            case link: Link         => tuple.copy(_2 = tuple._2 :+ link)
            case subGraph: SubGraph => tuple.copy(_3 = tuple._3 :+ subGraph)
          }
        })
      )
    }

  def flowChart[_: P]: P[FlowChart] =
    (
      flowChartHeader ~ nl ~
        (
          (nodeDefinition ~ nl) |
            (connection ~ nl) |
            (subGraph ~ nl)
        ).rep ~ nl.rep ~ End
    ).map { case (direction: RenderDirection, data: Seq[Product]) =>
      val flowChartData = data.foldLeft(
        (Seq.empty[Node], Seq.empty[Link], Seq.empty[SubGraph])
      )((tuple, data) => {
        data match {
          case node: Node         => tuple.copy(_1 = tuple._1 :+ node)
          case link: Link         => tuple.copy(_2 = tuple._2 :+ link)
          case subGraph: SubGraph => tuple.copy(_3 = tuple._3 :+ subGraph)
        }
      })
      FlowChart.fromTuple(direction)(flowChartData)
    }
}
