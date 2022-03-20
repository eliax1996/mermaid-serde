package com.mermaidparse.chart.flowchart

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
      val nodeId: ID = "A"
      val nodeIdLong: ID = "AnotherComplexId42"

      assert(
        parse("A[/Christmas\\]").get.value == Node(
          nodeId,
          "Christmas",
          TrapezoidBtoT
        )
      )

      assert(
        parse("AnotherComplexId42[/Christmas\\]").get.value == Node(
          nodeIdLong,
          "Christmas",
          TrapezoidBtoT
        )
      )

      assert(
        parse("A[Christmas]").get.value == Node(
          nodeId,
          "Christmas",
          Squared
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
          name = SubGraphName("one"),
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
            name = SubGraphName("one"),
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
            name = SubGraphName("two"),
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
            name = SubGraphName("three"),
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
    def parse(toParse: String): Parsed[FlowChart] =
      fastparse.parse(toParse, Parser.flowChart(_))

    val toParse =
      """
        |flowchart TB
        |    c1-->a2
        |    subgraph ide1 [one]
        |       a1-->a2
        |    end
        |""".stripMargin

    val first = Node(id = "c1", text = "c1", shape = Squared)
    val second = Node(id = "a2", text = "a2", shape = Squared)

    assert(
      parse(toParse).get.value == FlowChart(
        direction = TB,
        nodes = List(
          first,
          second
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
          )
        ),
        subGraph = List(
          SubGraph(
            name = SubGraphName("ide1", name = "[one]"),
            maybeDirection = None,
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
          )
        )
      )
    )
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
            name = SubGraphName("one"),
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
            name = SubGraphName("two"),
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
            name = SubGraphName("three"),
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
        |  A --> B
        |  B1 --> B2
        |""".stripMargin

    val first = Node(id = "A", text = "A", shape = Squared)
    val second = Node(id = "B", text = "B", shape = Squared)
    val third = Node(id = "B1", text = "B1", shape = Squared)
    val fourth = Node(id = "B2", text = "B2", shape = Squared)

    assert(
      parse(toParse).get.value == FlowChart(
        direction = LR,
        nodes = List(
          first,
          second,
          third,
          fourth
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
          )
        ),
        subGraph = List(
          SubGraph(
            name = SubGraphName(id = None, name = "TOP"),
            maybeDirection = Some(value = TB),
            nodes = List(),
            connections = List(),
            subGraph = List(
              SubGraph(
                name = SubGraphName(id = None, name = "B1"),
                maybeDirection = Some(value = RL),
                nodes = List(
                  Node(id = "i1", text = "i1", shape = Squared),
                  Node(id = "f1", text = "f1", shape = Squared)
                ),
                connections = List(
                  Link(
                    source = Node(id = "i1", text = "i1", shape = Squared),
                    destination = Node(id = "f1", text = "f1", shape = Squared),
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
                name = SubGraphName(id = None, name = "B2"),
                maybeDirection = Some(value = BT),
                nodes = List(
                  Node(id = "i2", text = "i2", shape = Squared),
                  Node(id = "f2", text = "f2", shape = Squared)
                ),
                connections = List(
                  Link(
                    source = Node(id = "i2", text = "i2", shape = Squared),
                    destination = Node(id = "f2", text = "f2", shape = Squared),
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
      )
    )
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
        |  A --> B
        |  B1 --> B2
        |""".stripMargin

    val first = Node(id = "A", text = "A", shape = Squared)
    val second = Node(id = "B", text = "B", shape = Squared)
    val third = Node(id = "B1", text = "B1", shape = Squared)
    val fourth = Node(id = "B2", text = "B2", shape = Squared)

    assert(
      parse(toParse).get.value == FlowChart(
        direction = LR,
        nodes = List(
          first,
          second,
          third,
          fourth
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
          )
        ),
        subGraph = List(
          SubGraph(
            name = SubGraphName(id = None, name = "TOP"),
            maybeDirection = Some(value = TB),
            nodes = List(),
            connections = List(),
            subGraph = List(
              SubGraph(
                name = SubGraphName(id = None, name = "B1"),
                maybeDirection = Some(value = RL),
                nodes = List(
                  Node(id = "i1", text = "i1", shape = Squared),
                  Node(id = "f1", text = "f1", shape = Squared)
                ),
                connections = List(
                  Link(
                    source = Node(id = "i1", text = "i1", shape = Squared),
                    destination = Node(id = "f1", text = "f1", shape = Squared),
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
                name = SubGraphName(id = None, name = "B2"),
                maybeDirection = Some(value = BT),
                nodes = List(
                  Node(id = "i2", text = "i2", shape = Squared),
                  Node(id = "f2", text = "f2", shape = Squared)
                ),
                connections = List(
                  Link(
                    source = Node(id = "i2", text = "i2", shape = Squared),
                    destination = Node(id = "f2", text = "f2", shape = Squared),
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
      )
    )
  }

}
