type rawProgram = string;
//Examples:
// "(+ 5 4)"
// "(if (= 4 4) (/ (+ 3 2) 5) (- 4 2))"
// "(cons (cons 2 empty) empty)"
type concreteProgramPiece =
  | NumberC(int)
  // NumberC(1), NumberC(14), NumberC(-123)
  | SymbolC(string)
  // SymbolC("+"), SymbolC("/"), SymbolC("and")
  | ListC(list(concreteProgramPiece));
//ListC([SymbolC("+"), NumberC(4), NumberC(2)])
//ListC([SymbolC("and"), ListC([SymbolC("cons?"), SymbolC("lst1")]), ListC([SymbolC("="), NumberC(5), NumberC(5)])])
//ListC([SymbolC("cons"), NumberC(5), SymbolC("empty")])
type concreteProgram = list(concreteProgramPiece);
//[ListC([SymbolC("+"), NumberC(5), NumberC(4)]), numberC(5)]
//[NumberC(4), NumberC(4)]
//[SymbolC("+")]
/* a Rackette name */
type name =
  | Name(string);
//Name("lst1"), Name("alod"), Name("foobar")
/* a Rackette expression */
type expression =
  | NumE(int)
  //NumE(1), NumE(-14), NumE(149209)
  | BoolE(bool)
  //BoolE(true), BoolE(false)
  | EmptyE
  //EmptyE
  | NameE(name)
  //NameE("lst1"), NameE("alod"), NameE("foobar")
  | AndE(expression, expression)
  //AndE(BoolE(true), BoolE(true))
  //AndE(ApplicationE([NameE("empty?"), EmptyE]), BoolE(false))
  //AndE(ApplicationE([NameE("="), NumE(5), NumE(4)]), BoolE(true))
  | OrE(expression, expression)
  //orE(BoolE(true), BoolE(true))
  //orE(ApplicationE([NameE("empty?"), EmptyE]), BoolE(false))
  //orE(ApplicationE([NameE("="), NumE(5), NumE(4)]), BoolE(true))
  | IfE(ifData)
  //ifE({conditionExpr: BoolE(true), trueExpr: NumE(5), falseExpr: ApplicationE(NameE("+"), NumE(4), NumE(5))})
  //ifE({conditionExpr: ApplicationE(NameE("cons?"), EmptyE), trueExpr: NumE(142), falseExpr: NumE(5234))})
  //ifE({conditionExpr: BoolE(false), trueExpr: BoolE(true), falseExpr: BoolE(false)})
  | CondE(list(condData))
  //CondE({conditionExpr: BoolE(true), resultExpr: ApplicationE(NameE("+"), NumE(4), NumE(5))})
  //CondE({conditionExpr: ApplicationE(NameE("cons?"), EmptyE), ResultExpr: NumE(142))})
  //CondE({conditionExpr: BoolE(false), resultExpr: BoolE(false)})
  | LambdaE(lambdaData)
  //lambdaE({nameList: [Name("var1")], lambdaBody: AndE(ApplicationE([NameE("="), NameE("var1"), NumE(4)]), BoolE(true))})
  //lambdaE({nameList: [Name("num1"), Name("num2")], lambdaBody: ApplicationE(NameE("+"), NameE(num1), NameE(num2))})
  //lambdaE({nameList: [], lambdaBody: numE(5)})
  | LetE(letData)
  //LetE({pairName: Name("var1"), pairExpr: NumE(5)})
  //LetE({pairName: Name("aloi"), pairExpr: ApplicationE(NameE("cons"), NumE(5), EmptyE)})
  //LetE({pairName: Name("alob"), pairExpr: ApplicationE(NameE("cons"), BoolE(true), ApplicationE(NameE("cons"), BoolE(false), EmptyE))})
  | ApplicationE(list(expression))
//ApplicationE(NameE("+"), NumE(4), NumE(5))
//ApplicationE([NameE("="), NumE(5), NumE(4)])
//ApplicationE(NameE("cons?"), EmptyE)
and ifData = {
  conditionExpr: expression,
  trueExpr: expression,
  falseExpr: expression,
}
//{conditionExpr: BoolE(true), trueExpr: NumE(5), falseExpr: ApplicationE([NameE("+"), NumE(4), NumE(5)])}
//{conditionExpr: ApplicationE([NameE("cons?"), EmptyE]), trueExpr: NumE(142), falseExpr: NumE(5234))}
//{conditionExpr: BoolE(false), trueExpr: BoolE(true), falseExpr: BoolE(false)}
and condData = {
  conditionExpr: expression,
  resultExpr: expression,
}
//{conditionExpr: BoolE(true), resultExpr: ApplicationE(NameE("+"), NumE(4), NumE(5))}
//{conditionExpr: ApplicationE(NameE("cons?"), EmptyE), ResultExpr: NumE(142))}
//{conditionExpr: BoolE(false), resultExpr: BoolE(false)}
and lambdaData = {
  nameList: list(name),
  lambdaBody: expression,
}
//{nameList: [Name("var1")], lambdaBody: AndE(ApplicationE([NameE("="), NameE("var1"), NumE(4)]), BoolE(true))}
//{nameList: [Name("num1"), Name("num2")], lambdaBody: ApplicationE([NameE("+"), NameE(num1), NameE(num2)])}
//{nameList: [], lambdaBody: numE(5)}
and letPair = {
  pairName: name,
  pairExpr: expression,
}
//{pairName: Name("var1"), pairExpr: NumE(5)}
//{pairName: Name("aloi"), pairExpr: ApplicationE([NameE("cons"), NumE(5), EmptyE)]}
//{pairName: Name("alob"), pairExpr: ApplicationE([NameE("cons"), BoolE(true), ApplicationE(NameE("cons"), BoolE(false), EmptyE)])}
and letData = {
  letPairs: list(letPair),
  letBody: expression,
};
//{letPairs: [], letBody: ApplicationE([NameE("+"), NumE(4), NumE(5)])}
//{letPairs: [{pairName: Name("var1"), pairExpr: NumE(5)}], letBody: ApplicationE([NameE("-"), NumE(7), NameE(var1)])}

/* a Rackette definition */
type definition = (name, expression);
//(Name("var1"), NumE(5))
//(Name("myBool"), BoolE(true))
//(Name("alod"), ApplicationE(NameE("cons"), NumE(5), EmptyE))
/* a piece of Rackette that can be processed:
 * either a definition or an expression */
type abstractProgramPiece =
  | Definition(definition)
  //Definition((Name("var1"), NumE(5)))
  //Definition((Name("myBool"), BoolE(true)))
  //Definition((Name("alod"), ApplicationE(NameE("cons"), NumE(5), EmptyE)))
  | Expression(expression);
//Expression(NameE("lst1"))
//Expression(orE(ApplicationE([NameE("empty?"), EmptyE]), BoolE(false)))
//Expression(ifE({conditionExpr: BoolE(false), trueExpr: BoolE(true), falseExpr: BoolE(false)}))
/* a representation of a Rackette program -
 * any number of pieces */
type abstractProgram = list(abstractProgramPiece);
//[Definition((Name("var1"), NumE(5))), Expression(ifE({conditionExpr: BoolE(false), trueExpr: BoolE(true), falseExpr: BoolE(false)}))]
//[Definition((Name("myBool"), BoolE(true))), Expression(orE(ApplicationE([NameE("empty?"), EmptyE]), NameE("myBool"))))]
//[Definiton(Name("number"), NumE(5)), Expression(ApplicationE([NameE("+"), NumE(7), NameE("number")]))]
/* a Rackette value: the result of evaluating a Rackette expression */
type value =
  | NumV(int)
  //NumV(5), NumV(14), NumV(-14)
  | BoolV(bool)
  //BoolV(true), BoolV(false)
  | ListV(list(value))
  //ListV([NumV(5), NumV(12534)])
  //ListV([BoolV(true), BoolV(false), BoolV(false)])
  | BuiltinV(builtinData)
  | ClosureV(closureData)
and builtinData = {
  printedRep: string,
  bProc: list(value) => value,
}
and closureData = {
  cNameList: list(name),
  cExpr: expression,
  cEnv: environment,
}
/* Environments, bindingLists, and bindings aren't values
   But we use "and" here so closures have access to environments,
   bindings have access to values, etc. */
and environment = list(bindingList)
and bindingList = list(binding)
and binding = (name, value);
