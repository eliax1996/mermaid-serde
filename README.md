# Mermaid parse a scala parser for mermaid format

A simple parser for the [Mermaid](https://mermaid-js.github.io/mermaid/#/) format.

It will allow to create a graph of the different shapes by creating and converting into mermaid the case class that
defines the AST of the mermaid language.

The reverse method is the `Mermaid.parse(myMermaidCode)` method that allows you to have the case class in memory
representation of the mermaid code.

The library is possible thanks to the Liaoyi fastparse2 library and the pretty print library.