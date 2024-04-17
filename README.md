# Ag<sub>2</sub>O: A Rust parser and AST for Silver 

This project contains a set of crates which define a parser and AST for [Silver](https://github.com/viperproject/silver), Viper's intermediate verification language. 
Eventually, this will also include a pretty printer for Viper allowing other tools to be built on top of this common infrastructure. 

For the moment this focuses on legibility of the parser code so that conformance can be easily ascertained. 
If the need occurs, we will add a second higher performance parser which can be validated against the simpler PEG parser. 

