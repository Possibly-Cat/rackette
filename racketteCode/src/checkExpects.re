open Rackette;
open Types;
open Read.Reader;
open CS17SetupRackette;

checkExpectConcreteProgram(
  readAll("(+ 4 5)"),
  [ListC([SymbolC("+"), NumberC(4), NumberC(5)])],
  "readAll test - 1",
);
checkExpectConcreteProgram(
  readAll("(+ 4 (* 16 932))"),
  [
    ListC([
      SymbolC("+"),
      NumberC(4),
      ListC([SymbolC("*"), NumberC(16), NumberC(932)]),
    ]),
  ],
  "readAll test - 2",
);
checkExpectConcreteProgram(
  readAll("(first (cons 3 (cons 2 empty)))"),
  [
    ListC([
      SymbolC("first"),
      ListC([
        SymbolC("cons"),
        NumberC(3),
        ListC([SymbolC("cons"), NumberC(2), SymbolC("empty")]),
      ]),
    ]),
  ],
  "readAll test - 3",
);
checkExpect(
  lambdaNamesToName([SymbolC("+"), SymbolC("x")]),
  [Name("+"), Name("x")],
  "lambdaNamesToName - 1",
);

checkExpectExpression(
  parseExpression(NumberC(4)),
  NumE(4),
  "parseExpression - single num",
);
checkExpectExpression(
  parseExpression(SymbolC("true")),
  BoolE(true),
  "parseExpression - true",
);
checkExpectExpression(
  parseExpression(SymbolC("false")),
  BoolE(false),
  "parseExpression - false",
);
checkExpectExpression(
  parseExpression(SymbolC("empty")),
  EmptyE,
  "parseExpression - empty",
);
checkExpectExpression(
  parseExpression(SymbolC("x")),
  NameE(Name("x")),
  "parseExpression - some variable",
);
checkExpectExpression(
  parseExpression(
    ListC([
      SymbolC("and"),
      SymbolC("true"),
      ListC([SymbolC("="), NumberC(4), NumberC(4)]),
    ]),
  ),
  AndE(BoolE(true), ApplicationE([NameE(Name("=")), NumE(4), NumE(4)])),
  "parseExpression - and statement",
);
checkExpectExpression(
  parseExpression(
    ListC([
      SymbolC("or"),
      SymbolC("true"),
      ListC([SymbolC("="), NumberC(4), NumberC(4)]),
    ]),
  ),
  OrE(BoolE(true), ApplicationE([NameE(Name("=")), NumE(4), NumE(4)])),
  "parseExpression - or statement",
);
checkExpectExpression(
  parseExpression(
    ListC([
      SymbolC("if"),
      SymbolC("true"),
      NumberC(7),
      ListC([SymbolC("+"), NumberC(4), NumberC(18)]),
    ]),
  ),
  IfE({
    boolExpr: BoolE(true),
    trueExpr: NumE(7),
    falseExpr: ApplicationE([NameE(Name("+")), NumE(4), NumE(18)]),
  }),
  "parseExpression - if statement",
);
checkExpectExpression(
  parseExpression(
    ListC([
      SymbolC("cond"),
      ListC([SymbolC("true"), NumberC(1)]),
      ListC([ListC([SymbolC("="), NumberC(4), NumberC(4)]), NumberC(2)]),
    ]),
  ),
  CondE([
    {conditionExpr: BoolE(true), resultExpr: NumE(1)},
    {
      conditionExpr: ApplicationE([NameE(Name("=")), NumE(4), NumE(4)]),
      resultExpr: NumE(2),
    },
  ]),
  "parseExpression - cond statement",
);
checkExpectExpression(
  parseExpression(
    ListC([
      SymbolC("lambda"),
      ListC([SymbolC("x"), SymbolC("y")]),
      ListC([SymbolC("+"), SymbolC("x"), SymbolC("y")]),
    ]),
  ),
  LambdaE({
    nameList: [Name("x"), Name("y")],
    lambdaBody:
      ApplicationE([
        NameE(Name("+")),
        NameE(Name("x")),
        NameE(Name("y")),
      ]),
  }),
  "parseExpression - lambda statement",
);
checkExpectExpression(
  parseExpression(
    ListC([
      SymbolC("let"),
      ListC([
        ListC([SymbolC("x"), NumberC(5)]),
        ListC([SymbolC("y"), NumberC(4)]),
      ]),
      ListC([SymbolC("-"), SymbolC("x"), SymbolC("y")]),
    ]),
  ),
  LetE({
    letPairs: [
      {pairName: Name("x"), pairExpr: NumE(5)},
      {pairName: Name("y"), pairExpr: NumE(4)},
    ],
    letBody:
      ApplicationE([
        NameE(Name("-")),
        NameE(Name("x")),
        NameE(Name("y")),
      ]),
  }),
  "parseExpression - let statement",
);
checkExpectAbstractProgramPiece(
  parsePiece(SymbolC("empty")),
  Expression(EmptyE),
  "parsePiece - empty",
);
