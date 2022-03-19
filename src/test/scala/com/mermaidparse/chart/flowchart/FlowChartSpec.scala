package com.mermaidparse.chart.flowchart

import com.mermaidparse.chart.flowchart.LineDirections.{
  LeftToRight,
  MultipleDirection,
  RightToLeft,
  WithoutDirection
}
import com.mermaidparse.chart.flowchart.LineDirections.TipTypes.{
  Circle,
  Crossed,
  Standard
}
import com.mermaidparse.chart.flowchart.LineShapes.{Continuous, Dotted, Thick}
import com.mermaidparse.chart.flowchart.LinkTypes.{ConnectionType, Link}
import com.mermaidparse.chart.flowchart.NodeShape.{Node, NodeId, Squared}
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
      assert(!parse("graph LMAO;").isSuccess)
      assert(!parse("grah TB;").isSuccess)
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

    val toParse = "Start ==a piece of text===o Stop"

    assert(
      parse(toParse).get.value == Link(
        Node("Start"),
        Node("Stop"),
        Some("a piece of text"),
        ConnectionType(LeftToRight(Some(Circle)), Thick),
        5
      )
    )
  }

  it("parse right to left thick crossed connections with text") {
    def parse(toParse: String): Parsed[Link] =
      fastparse.parse(toParse, Parser.connection(_))

    val toParse = "Start x==a piece of text==x Stop"

    assert(
      parse(toParse).get.value == Link(
        Node("Start"),
        Node("Stop"),
        Some("a piece of text"),
        ConnectionType(MultipleDirection(Some(Crossed)), Thick),
        4
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

  it("parse a subgraph") {
    def parse(toParse: String): Parsed[SubGraph] =
      fastparse.parse(toParse, Parser.subGraph(_))

    val toParse =
      """subgraph one
        |   Start --> Stop
        |   a2 --> a3
        |end
        |""".stripMargin

    val first = Node(id = "Start", text = "Start", shape = Squared)
    val second = Node(id = "Stop", text = "Stop", shape = Squared)
    val third = Node(id = "a2", text = "a2", shape = Squared)
    val fourth = Node(id = "a3", text = "a3", shape = Squared)

    assert(
      parse(toParse).get.value ==
        SubGraph(
          name = "one",
          nodes = List(first, second, third, fourth),
          connections = List(
            Link(
              source = first,
              destination = second,
              text = None,
              connection = ConnectionType(
                direction = LeftToRight(tipType = Some(value = Standard)),
                lineType = Continuous
              ),
              length = 2
            ),
            Link(
              source = third,
              destination = fourth,
              text = None,
              connection = ConnectionType(
                direction = LeftToRight(tipType = Some(value = Standard)),
                lineType = Continuous
              ),
              length = 2
            )
          ),
          subGraph = List()
        )
    )
  }

  it("parse a graph with two independent subgraph") {
    def parse(toParse: String): Parsed[FlowChart] =
      fastparse.parse(toParse, Parser.flowChart(_))

    val toParse =
      """
        |flowchart TB
        |
        |    c1 --> a2
        |
        |    subgraph one
        |       a1-->a2
        |    end
        |    subgraph two
        |       b1-->b2
        |    end
        |    subgraph three
        |       c1-->c2
        |    end
        |""".stripMargin

    val first = Node(id = "c1", text = "c1", shape = Squared)
    val second = Node(id = "a2", text = "a2", shape = Squared)

    assert(
      parse(toParse).get.value == FlowChart(
        direction = TB,
        nodes = List(first, second),
        connections = List(
          Link(
            source = first,
            destination = second,
            text = None,
            connection = ConnectionType(
              direction = LeftToRight(tipType = Some(value = Standard)),
              lineType = Continuous
            ),
            length = 2
          )
        ),
        subGraph = List(
          SubGraph(
            name = "one",
            nodes = List(
              Node(id = "a1", text = "a1", shape = Squared),
              Node(id = "a2", text = "a2", shape = Squared)
            ),
            connections = List(
              Link(
                source = Node(id = "a1", text = "a1", shape = Squared),
                destination = Node(id = "a2", text = "a2", shape = Squared),
                text = None,
                connection = ConnectionType(
                  direction = LeftToRight(tipType = Some(value = Standard)),
                  lineType = Continuous
                ),
                length = 2
              )
            ),
            subGraph = List()
          ),
          SubGraph(
            name = "two",
            nodes = List(
              Node(id = "b1", text = "b1", shape = Squared),
              Node(id = "b2", text = "b2", shape = Squared)
            ),
            connections = List(
              Link(
                source = Node(id = "b1", text = "b1", shape = Squared),
                destination = Node(id = "b2", text = "b2", shape = Squared),
                text = None,
                connection = ConnectionType(
                  direction = LeftToRight(tipType = Some(value = Standard)),
                  lineType = Continuous
                ),
                length = 2
              )
            ),
            subGraph = List()
          ),
          SubGraph(
            name = "three",
            nodes = List(
              first,
              Node(id = "c2", text = "c2", shape = Squared)
            ),
            connections = List(
              Link(
                source = first,
                destination = Node(id = "c2", text = "c2", shape = Squared),
                text = None,
                connection = ConnectionType(
                  direction = LeftToRight(tipType = Some(value = Standard)),
                  lineType = Continuous
                ),
                length = 2
              )
            ),
            subGraph = List()
          )
        )
      )
    )
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
    def parse(toParse: String): Parsed[FlowChart] =
      fastparse.parse(toParse, Parser.flowChart(_))

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

    val first = Node(id = "c1", text = "c1", shape = Squared)
    val second = Node(id = "a2", text = "a2", shape = Squared)
    val third = Node(id = "one", text = "one", shape = Squared)
    val fourth = Node(id = "two", text = "two", shape = Squared)
    val fifth = Node(id = "three", text = "three", shape = Squared)
    val sixth = Node(id = "c2", text = "c2", shape = Squared)

    assert(
      parse(toParse).get.value == FlowChart(
        direction = TB,
        nodes = List(
          first,
          second,
          third,
          fourth,
          fifth,
          sixth
        ),
        connections = List(
          Link(
            source = first,
            destination = second,
            text = None,
            connection = ConnectionType(
              direction = LeftToRight(tipType = Some(value = Standard)),
              lineType = Continuous
            ),
            length = 2
          ),
          Link(
            source = third,
            destination = fourth,
            text = None,
            connection = ConnectionType(
              direction = LeftToRight(tipType = Some(value = Standard)),
              lineType = Continuous
            ),
            length = 2
          ),
          Link(
            source = fifth,
            destination = fourth,
            text = None,
            connection = ConnectionType(
              direction = LeftToRight(tipType = Some(value = Standard)),
              lineType = Continuous
            ),
            length = 2
          ),
          Link(
            source = fourth,
            destination = sixth,
            text = None,
            connection = ConnectionType(
              direction = LeftToRight(tipType = Some(value = Standard)),
              lineType = Continuous
            ),
            length = 2
          )
        ),
        subGraph = List(
          SubGraph(
            name = "one",
            nodes = List(
              Node(id = "a1", text = "a1", shape = Squared),
              second
            ),
            connections = List(
              Link(
                source = Node(id = "a1", text = "a1", shape = Squared),
                destination = second,
                text = None,
                connection = ConnectionType(
                  direction = LeftToRight(tipType = Some(value = Standard)),
                  lineType = Continuous
                ),
                length = 2
              )
            ),
            subGraph = List()
          ),
          SubGraph(
            name = "two",
            nodes = List(
              Node(id = "b1", text = "b1", shape = Squared),
              Node(id = "b2", text = "b2", shape = Squared)
            ),
            connections = List(
              Link(
                source = Node(id = "b1", text = "b1", shape = Squared),
                destination = Node(id = "b2", text = "b2", shape = Squared),
                text = None,
                connection = ConnectionType(
                  direction = LeftToRight(tipType = Some(value = Standard)),
                  lineType = Continuous
                ),
                length = 2
              )
            ),
            subGraph = List()
          ),
          SubGraph(
            name = "three",
            nodes = List(
              first,
              sixth
            ),
            connections = List(
              Link(
                source = first,
                destination = sixth,
                text = None,
                connection = ConnectionType(
                  direction = LeftToRight(tipType = Some(value = Standard)),
                  lineType = Continuous
                ),
                length = 2
              )
            ),
            subGraph = List()
          )
        )
      )
    )
  }

  it("parse a graph with subgraph and directions") {
    def parse(toParse: String): Parsed[FlowChart] =
      fastparse.parse(toParse, Parser.flowChart(_))

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

    pprint.pprintln(parse(toParse).get.value)
  }

  it("parse subgraph with directions") {
    def parse(toParse: String): Parsed[FlowChart] =
      fastparse.parse(toParse, Parser.flowChart(_))

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

    pprint.pprintln(parse(toParse).get.value)
  }

}
