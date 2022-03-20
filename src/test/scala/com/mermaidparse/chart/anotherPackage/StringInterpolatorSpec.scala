package com.mermaidparse.chart.anotherPackage

import org.scalatest.funspec.AnyFunSpec

class StringInterpolatorSpec extends AnyFunSpec {
  import eu.timepit.refined.auto._

  describe("The FlowChart header parser") {

    import com.mermaidparse.chart.flowchart._

    val interpolatedGraph =
      mermaid"""
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
               |  """

    assert(
      interpolatedGraph == FlowChart(
        direction = LR,
        nodes = List(
          Node(id = "A", text = "A", shape = Squared),
          Node(id = "B", text = "B", shape = Squared),
          Node(id = "B1", text = "B1", shape = Squared),
          Node(id = "B2", text = "B2", shape = Squared)
        ),
        connections = List(
          Link(
            source = Node(id = "A", text = "A", shape = Squared),
            destination = Node(id = "B", text = "B", shape = Squared),
            text = None,
            connection = ConnectionType(
              direction = LeftToRight(tipType = Some(value = Standard)),
              lineType = Continuous
            ),
            length = 2
          ),
          Link(
            source = Node(id = "B1", text = "B1", shape = Squared),
            destination = Node(id = "B2", text = "B2", shape = Squared),
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
