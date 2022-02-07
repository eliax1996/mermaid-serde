package com.mermaidparse.chart.flowchart

import com.mermaidparse.chart.flowchart.LineDirections.{LeftToRight, MultipleDirection, RightToLeft, WithoutDirection}
import com.mermaidparse.chart.flowchart.LineDirections.TipTypes.{Circle, Crossed, Standard}
import com.mermaidparse.chart.flowchart.LineShapes.{Continuous, Dotted, Thick}
import com.mermaidparse.chart.flowchart.LinkTypes.{ConnectionType, Link}
import com.mermaidparse.chart.flowchart.NodeShape.{Node, NodeId}
import fastparse.Parsed
import org.scalatest.funspec.AnyFunSpec

class FlowChartSpec extends AnyFunSpec {
  import eu.timepit.refined.auto._

  describe("The FlowChart header parser") {
    def parse(toParse: String): Parsed[RenderDirection] =
      fastparse.parse(toParse, Parser.flowChartHeader(_))

    it("should parse a well string") {
      assert(parse("graph TD;").get.value == TD)
      assert(parse("graph TD ;").get.value == TD)
      assert(parse("graph      TD              ;").get.value == TD)
    }

    it("fail on uncomplete or wrong string") {
      assert(!parse("graph TD").isSuccess)
      assert(!parse("graph LMAO;").isSuccess)
      assert(!parse("grah TB;").isSuccess)
      assert(!parse("graph TD\n;").isSuccess)
    }
  }

  describe("The NodeDefinition parser") {

    def parse(toParse: String): Parsed[Node] =
      fastparse.parse(toParse, Parser.nodeDefinition(_))

    it("should parse a well string") {
      val nodeId: NodeId = "A"
      val nodeIdLong: NodeId = "AnotherComplexId42"

      assert(
        parse("A[/Christmas\\]").get.value == Node(
          nodeId,
          "Christmas",
          NodeShape.TrapezoidBtoT
        )
      )

      assert(
        parse("AnotherComplexId42[/Christmas\\]").get.value == Node(
          nodeIdLong,
          "Christmas",
          NodeShape.TrapezoidBtoT
        )
      )

      assert(
        parse("A[Christmas]").get.value == Node(
          nodeId,
          "Christmas",
          NodeShape.Squared
        )
      )
    }
  }

  it("parse normal connections") {
    def parse(toParse: String): Parsed[Link] =
      fastparse.parse(toParse, Parser.connection(_))

    val toParse = "Start --> Stop"

    assert(
      parse(toParse).get.value == Link(
        Node("Start"),
        Node("Stop"),
        None,
        ConnectionType(LeftToRight(Some(Standard)), Continuous),
        2
      )
    )
  }

  it("parse normal connections with opposite direction") {
    def parse(toParse: String): Parsed[Link] =
      fastparse.parse(toParse, Parser.connection(_))

    val toParse = "Start <-- Stop"

    assert(
      parse(toParse).get.value == Link(
        Node("Start"),
        Node("Stop"),
        None,
        ConnectionType(RightToLeft(Some(Standard)), Continuous),
        2
      )
    )
  }

  it("parse normal connections without arrow") {
    def parse(toParse: String): Parsed[Link] =
      fastparse.parse(toParse, Parser.connection(_))

    val toParse = "Start -- Stop"

    assert(
      parse(toParse).get.value == Link(
        Node("Start"),
        Node("Stop"),
        None,
        ConnectionType(WithoutDirection, Continuous),
        2
      )
    )
  }

  it("parse normal connections with multiple direction arrow") {
    def parse(toParse: String): Parsed[Link] =
      fastparse.parse(toParse, Parser.connection(_))

    val toParse = "Start <--> Stop"

    assert(
      parse(toParse).get.value == Link(
        Node("Start"),
        Node("Stop"),
        None,
        ConnectionType(MultipleDirection(Some(Standard)), Continuous),
        2
      )
    )
  }

  it("parse connections with text") {
    def parse(toParse: String): Parsed[Link] =
      fastparse.parse(toParse, Parser.connection(_))

    val toParse = "Start --a piece of text--> Stop"

    assert(
      parse(toParse).get.value == Link(
        Node("Start"),
        Node("Stop"),
        Some("a piece of text"),
        ConnectionType(LeftToRight(Some(Standard)), Continuous),
        4
      )
    )
  }

  it("parse circle connections with text") {
    def parse(toParse: String): Parsed[Link] =
      fastparse.parse(toParse, Parser.connection(_))

    val toParse = "Start ===a piece of text===o Stop"

    assert(
      parse(toParse).get.value == Link(
        Node("Start"),
        Node("Stop"),
        Some("a piece of text"),
        ConnectionType(LeftToRight(Some(Circle)), Thick),
        6
      )
    )
  }

  it("parse right to left thick crossed connections with text") {
    def parse(toParse: String): Parsed[Link] =
      fastparse.parse(toParse, Parser.connection(_))

    val toParse = "Start x===a piece of text== Stop"

    assert(
      parse(toParse).get.value == Link(
        Node("Start"),
        Node("Stop"),
        Some("a piece of text"),
        ConnectionType(RightToLeft(Some(Crossed)), Thick),
        5
      )
    )
  }

  it("parse multiple dotted connections with multiple direction arrow") {
    def parse(toParse: String): Parsed[Link] =
      fastparse.parse(toParse, Parser.connection(_))

    val toParse = "Start x-.a piece of text..-x Stop"

    assert(
      parse(toParse).get.value == Link(
        Node("Start"),
        Node("Stop"),
        Some("a piece of text"),
        ConnectionType(MultipleDirection(Some(Crossed)), Dotted),
        5
      )
    )
  }

  it("parse a graph with two independent subgraph") {
    val toParse =
      """
        |flowchart TB
        |    c1-->a2
        |    subgraph one
        |    a1-->a2
        |    end
        |    subgraph two
        |    b1-->b2
        |    end
        |    subgraph three
        |    c1-->c2
        |    end
        |""".stripMargin
  }

  it("parse a graph a connection and a subgraph") {
    val toParse =
      """
        |flowchart TB
        |    c1-->a2
        |    subgraph ide1 [one]
        |    a1-->a2
        |    end
        |""".stripMargin
  }

  it("parse a graph with subgraph") {
    val toParse =
      """
        |flowchart TB
        |    c1-->a2
        |    subgraph one
        |    a1-->a2
        |    end
        |    subgraph two
        |    b1-->b2
        |    end
        |    subgraph three
        |    c1-->c2
        |    end
        |    one --> two
        |    three --> two
        |    two --> c2
        |""".stripMargin
  }

  it("parse a graph with subgraph and directions") {
    val toParse =
      """
        |flowchart LR
        |  subgraph TOP
        |    direction TB
        |    subgraph B1
        |        direction RL
        |        i1 -->f1
        |    end
        |    subgraph B2
        |        direction BT
        |        i2 -->f2
        |    end
        |  end
        |  A --> TOP --> B
        |  B1 --> B2
        |""".stripMargin
  }

  it("parse subgraph with directions") {
    val toParse =
      """
        |flowchart LR
        |  subgraph TOP
        |    direction TB
        |    subgraph B1
        |        direction RL
        |        i1 -->f1
        |    end
        |    subgraph B2
        |        direction BT
        |        i2 -->f2
        |    end
        |  end
        |  A --> TOP --> B
        |  B1 --> B2
        |""".stripMargin
  }

}
