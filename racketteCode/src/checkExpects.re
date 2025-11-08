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

checkExpectDefinition(
  parseDefinition(ListC([SymbolC("define"), SymbolC("x"), NumberC(5)])),
  (Name("x"), NumE(5)),
  "parseDefinition  - var to number",
);
checkExpectDefinition(
  parseDefinition(
    ListC([
      SymbolC("define"),
      SymbolC("addOne"),
      ListC([
        SymbolC("lambda"),
        ListC([SymbolC("x")]),
        ListC([SymbolC("+"), SymbolC("x"), NumberC(1)]),
      ]),
    ]),
  ),
  (
    Name("addOne"),
    LambdaE({
      nameList: [Name("x")],
      lambdaBody:
        ApplicationE([NameE(Name("+")), NameE(Name("x")), NumE(1)]),
    }),
  ),
  "parseDefinition - function with lambda",
);
checkExpectAbstractProgramPiece(
  parsePiece(NumberC(1)),
  Expression(NumE(1)),
  "parsePiece - single number",
);
checkExpectAbstractProgramPiece(
  parsePiece(SymbolC("empty")),
  Expression(EmptyE),
  "parsePiece - empty",
);
checkExpectAbstractProgramPiece(
  parsePiece(ListC([SymbolC("+"), NumberC(2), NumberC(5)])),
  Expression(ApplicationE([NameE(Name("+")), NumE(2), NumE(5)])),
  "parsePiece - procedure application expression",
);
checkExpectAbstractProgramPiece(
  parsePiece(ListC([SymbolC("define"), SymbolC("x"), NumberC(5)])),
  Definition((Name("x"), NumE(5))),
  "parsePiece - definition",
);
checkExpectAbstractProgram(
  parse([NumberC(5), NumberC(4)]),
  [Expression(NumE(5)), Expression(NumE(4))],
  "parse - two nums",
);
checkExpectAbstractProgram(
  parse([
    ListC([SymbolC("define"), SymbolC("x"), NumberC(5)]),
    SymbolC("x"),
  ]),
  [Definition((Name("x"), NumE(5))), Expression(NameE(Name("x")))],
  "parse - definition and variable call",
);
//Handle cond check relegated to check of eval which regards cond
//Eval and addDefinition checks relegated to process which is essentially just a
//  container for the two
checkExpect(
  process([Expression(NumE(5))]),
  [NumV(5)],
  "process - single number",
);
checkExpect(
  process([
    Expression(ApplicationE([NameE(Name("+")), NumE(5), NumE(12)])),
  ]),
  [NumV(17)],
  "process - addition builtin",
);
checkExpect(
  process([
    Definition((
      Name("addOne"),
      LambdaE({
        nameList: [Name("x")],
        lambdaBody:
          ApplicationE([NameE(Name("+")), NameE(Name("x")), NumE(1)]),
      }),
    )),
    Expression(ApplicationE([NameE(Name("addOne")), NumE(5)])),
  ]),
  [NumV(6)],
  "process - user defined expression definition and application",
);
checkExpect(
  process([
    Definition((Name("x"), NumE(23))),
    Expression(NameE(Name("x"))),
  ]),
  [NumV(23)],
  "process - variable definition and use",
);
checkExpect(
  process([Expression(BoolE(true))]),
  [BoolV(true)],
  "process - true",
);
checkExpect(
  process([Expression(BoolE(false))]),
  [BoolV(false)],
  "process - false",
);
checkExpect(
  process([Expression(EmptyE)]),
  [ListV([])],
  "process - empty",
);
checkExpect(
  process([Expression(AndE(BoolE(true), BoolE(true)))]),
  [BoolV(true)],
  "process - and with two trues",
);
checkExpect(
  process([Expression(AndE(BoolE(true), BoolE(false)))]),
  [BoolV(false)],
  "process - and with one true - 1",
);
checkExpect(
  process([Expression(AndE(BoolE(false), BoolE(true)))]),
  [BoolV(false)],
  "process - and with one true - 2",
);
checkExpect(
  process([Expression(AndE(BoolE(false), BoolE(false)))]),
  [BoolV(false)],
  "process - and with two falses",
);
checkExpect(
  process([Expression(OrE(BoolE(true), BoolE(true)))]),
  [BoolV(true)],
  "process - or with two trues",
);
checkExpect(
  process([Expression(OrE(BoolE(true), BoolE(false)))]),
  [BoolV(true)],
  "process - or with one true - 1",
);
checkExpect(
  process([Expression(OrE(BoolE(false), BoolE(true)))]),
  [BoolV(true)],
  "process - or with one true - 2",
);
checkExpect(
  process([Expression(OrE(BoolE(false), BoolE(false)))]),
  [BoolV(false)],
  "process - or with two falses",
);
checkExpect(
  process([
    Expression(
      IfE({boolExpr: BoolE(true), trueExpr: NumE(1), falseExpr: NumE(-1)}),
    ),
  ]),
  [NumV(1)],
  "process - if with true",
);
checkExpect(
  process([
    Expression(
      IfE({
        boolExpr: BoolE(false),
        trueExpr: NumE(1),
        falseExpr: NumE(-1),
      }),
    ),
  ]),
  [NumV(-1)],
  "process - if with false",
);
checkExpect(
  process([
    Expression(
      CondE([
        {conditionExpr: BoolE(false), resultExpr: NumE(0)},
        {conditionExpr: BoolE(true), resultExpr: NumE(1)},
      ]),
    ),
  ]),
  [NumV(1)],
  "prcoess - cond 1",
);
checkExpect(
  process([
    Expression(
      CondE([
        {conditionExpr: BoolE(true), resultExpr: NumE(0)},
        {conditionExpr: BoolE(true), resultExpr: NumE(1)},
      ]),
    ),
  ]),
  [NumV(0)],
  "prcoess - cond 2",
);
checkExpect(
  process([
    Expression(
      LambdaE({
        nameList: [Name("x"), Name("y")],
        lambdaBody:
          ApplicationE([
            NameE(Name("-")),
            NameE(Name("x")),
            NameE(Name("y")),
          ]),
      }),
    ),
  ]),
  [
    ClosureV({
      cNameList: [Name("x"), Name("y")],
      cExpr:
        ApplicationE([
          NameE(Name("-")),
          NameE(Name("x")),
          NameE(Name("y")),
        ]),
      cEnv: [],
    }),
  ],
  "process - lambda",
);
checkExpect(
  process([
    Expression(
      LetE({
        letPairs: [
          {pairName: Name("x"), pairExpr: NumE(10)},
          {pairName: Name("y"), pairExpr: NumE(2)},
        ],
        letBody:
          ApplicationE([
            NameE(Name("*")),
            NameE(Name("x")),
            NameE(Name("y")),
          ]),
      }),
    ),
  ]),
  [NumV(20)],
  "process - let",
) /*applicationE has been suficiently tested through other check expect, I do not alot it its own*/;
checkExpect(stringOfValue(NumV(4)), "4", "stringOfValue - single number");
checkExpect(stringOfValue(BoolV(true)), "#t", "stringOfValue - true");
checkExpect(stringOfValue(BoolV(false)), "#f", "stringOfValue - false");
checkExpect(
  stringOfValue(ListV([NumV(1), NumV(2), NumV(3)])),
  "[1,2,3]",
  "stringOfValue - list",
);
checkExpect(
  stringOfValue(ListV([])),
  "empty",
  "stringOfValue - empty list",
);
checkExpect(
  stringOfValue(
    BuiltinV({
      printedRep: "myPrint",
      bProc: _thisIsAPlaceholder => BoolV(true),
    }),
  ),
  "myPrint",
  "StringOfValue - builtIn",
);
