# Mermaid parse a scala parser for mermaid format

A simple parser for the [Mermaid](https://mermaid-js.github.io/mermaid/#/) format.

This small library allows to store a representation of a parsed mermaid graph into a case class (by parsing a string
that represents the graph) or do the reverse by rendering a case class to a mermaid string (by a serialization
mechanism).

You can get the mermaid case class representation by running the following string interpolation:

```scala
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
```

You can also do the reverse by calling:

```scala
val myGraph = FlowChart(
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
      
      println(myGraph.render())
```

The library is possible thanks to the Liaoyi fastparse2 library.