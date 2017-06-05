# Introduction
This is the project of the course "Linguagem de programação: conceitos e paradigmas" ministered by professor Umberto Souza da Costa at Universidade Federal do Rio Grande do Norte. The entire course was based on Sebesta's "Concepts of Programming Languages". The project is to create a programming language and an interpreter for it, using Haskell.

The purpose of the language was to be used for developing Operating Systems, hence, it is based in C and Ada. The name \"OWLS\" is just a pun with "OWLperating Systems". Even though the original idea was to support concurrency, our language does not; since it is out of the scope of the course.

# Compiling and running
In order to compile and run the code, it is need to have [Alex](https://www.haskell.org/alex/), [Parsec](https://hackage.haskell.org/package/parsec) and [cabal](https://www.haskell.org/cabal/).

To compile, execute the following commands in the Terminal:

```>> alex Tokens.x```

This generates a .hs file \(more specifically, \"Tokens.hs\"\). The file \"Tokens.x\" contains the tokens that represents the language's grammar.

```>> ghc main.hs```

This compiles the haskell code, generating the interpreter.

```>> ./main < <file_name>.owls```

By executing this command, the interpreter will execute the desired code. It is not hard to conclude that \"main\" is the name of the executable generated by the second command. In order for the interpreter to run, it is necessary to pass a file containing some owls code. For example, suppose we have a file named "hello_world.owls", then, the following command would execute it.

```>> ./main < hello_world.owls```

# TODO
- [ ] Do TODO list
- [ ] Resolve TODOs in Program.hs
- [ ] Resolve TODOs in Expr.hs
- [ ] Resolve TODOs in BoolExpr.hs
- [ ] Resolve TODOs in State.hs
- [ ] Resolve TODOs in FuncCall.hs
- [ ] Resolve TODOs in Tokens.x
- [ ] Resolve TODOs in Tokens.hs (?)