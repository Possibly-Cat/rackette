open CS17SetupRackette;
open Read.Reader;
open Types;

/* TODO: fill this with your initial top level environment,
 * consisting of built-in procedures like + or empty? */
let add: list(value) => value =
  alov =>
    switch (alov) {
    | [] => failwith("too few arguments for add")
    | [NumV(_hd)] => failwith("too few arguments for add")
    | [NumV(hd), NumV(tl)] => NumV(hd + tl)
    | [NumV(_e1), NumV(_e2), NumV(_e3), ..._tl] =>
      failwith("too many arguments for add")
    | _lst => failwith("non-number arguments in add")
    };
let subtract: list(value) => value =
  alov =>
    switch (alov) {
    | [] => failwith("too few arguments for subtract")
    | [NumV(_hd)] => failwith("too few arguments for subtract")
    | [NumV(hd), NumV(tl)] => NumV(hd - tl)
    | [NumV(_e1), NumV(_e2), NumV(_e3), ..._tl] =>
      failwith("too many arguments for subtract")
    | _lst => failwith("non-number arguments in subtract")
    };
let multiply: list(value) => value =
  alov =>
    switch (alov) {
    | [] => failwith("too few arguments for multiply")
    | [NumV(_hd)] => failwith("too few arguments for multiply")
    | [NumV(hd), NumV(tl)] => NumV(hd * tl)
    | [NumV(_e1), NumV(_e2), NumV(_e3), ..._tl] =>
      failwith("too many arguments for multiply")
    | _lst => failwith("non-number arguments in multiply")
    };
let divide: list(value) => value =
  alov =>
    switch (alov) {
    | [] => failwith("too few arguments for divide")
    | [NumV(_hd)] => failwith("too few arguments for divide")
    | [NumV(_hd), NumV(0)] => failwith("Divide by zero error")
    | [NumV(hd), NumV(tl)] => NumV(hd / tl)
    | [NumV(_e1), NumV(_e2), NumV(_e3), ..._tl] =>
      failwith("too many arguments for divide")
    | _lst => failwith("non-number arguments in divide")
    };
let modulo: list(value) => value =
  alov =>
    switch (alov) {
    | [] => failwith("too few arguments for remainder")
    | [NumV(_hd)] => failwith("too few arguments for remainder")
    | [NumV(hd), NumV(tl)] => NumV(hd mod tl)
    | [NumV(_e1), NumV(_e2), NumV(_e3), ..._tl] =>
      failwith("too many arguments for remainder")
    | _lst => failwith("non-number arguments in remainder")
    };
let equalityq: list(value) => value =
  alov =>
    switch (alov) {
    | [] => failwith("too few arguments for equalityq")
    | [NumV(_hd)] => failwith("too few arguments for equalityq")
    | [NumV(hd), NumV(tl)] => BoolV(hd == tl)
    | [NumV(_e1), NumV(_e2), NumV(_e3), ..._tl] =>
      failwith("too many arguments for equalityq")
    | _lst => failwith("non-number arguments in equalityq")
    };
let lessThanq: list(value) => value =
  alov =>
    switch (alov) {
    | [] => failwith("too few arguments for less than")
    | [NumV(_hd)] => failwith("too few arguments for less than")
    | [NumV(hd), NumV(tl)] => BoolV(hd < tl)
    | [NumV(_e1), NumV(_e2), NumV(_e3), ..._tl] =>
      failwith("too many arguments for less than")
    | _lst => failwith("non-number arguments in less than")
    };
let greaterThanq: list(value) => value =
  alov =>
    switch (alov) {
    | [] => failwith("too few arguments for greater than")
    | [NumV(_hd)] => failwith("too few arguments for greater than")
    | [NumV(hd), NumV(tl)] => BoolV(hd > tl)
    | [NumV(_e1), NumV(_e2), NumV(_e3), ..._tl] =>
      failwith("too many arguments for greater than")
    | _lst => failwith("non-number arguments in greater than")
    };
let leqq: list(value) => value =
  alov =>
    switch (alov) {
    | [] => failwith("too few arguments for <=")
    | [NumV(_hd)] => failwith("too few arguments for <=")
    | [NumV(hd), NumV(tl)] => BoolV(hd <= tl)
    | [NumV(_e1), NumV(_e2), NumV(_e3), ..._tl] =>
      failwith("too many arguments for <=")
    | _lst => failwith("non-number arguments in <=")
    };
let geqq: list(value) => value =
  alov =>
    switch (alov) {
    | [] => failwith("too few arguments for >=")
    | [NumV(_hd)] => failwith("too few arguments for >=")
    | [NumV(hd), NumV(tl)] => BoolV(hd >= tl)
    | [NumV(_e1), NumV(_e2), NumV(_e3), ..._tl] =>
      failwith("too many arguments for >=")
    | _lst => failwith("non-number arguments in >=")
    };
let equalq: list(value) => value =
  alov =>
    switch (alov) {
    | [] => failwith("too few arguments for equal")
    | [_val] => failwith("too few arguments for equal")
    | [NumV(hd), NumV(tl)] => BoolV(hd == tl)
    | [BoolV(hd), BoolV(tl)] => BoolV(hd == tl)
    | [_val1, _val2, _val3, ..._tl] =>
      failwith("too many arguments for equal")
    | _lst => failwith("non-number arguments in equal")
    };
let numberq: list(value) => value =
  alov =>
    switch (alov) {
    | [] => failwith("too few arguments for number?")
    | [NumV(_hd)] => BoolV(true)
    | [_val1] => BoolV(false)
    | [_val1, _val2, ..._tl] => failwith("too many arguments for number?")
    };
let zeroq: list(value) => value =
  alov =>
    switch (alov) {
    | [] => failwith("too few arguments for zero?")
    | [NumV(hd)] => BoolV(hd == 0)
    | [_val1] => failwith("non-number argument for zero?")
    | [_val1, _val2, ..._tl] => failwith("too many arguments for zero?")
    };
let cons: list(value) => value =
  alov =>
    switch (alov) {
    | [] => failwith("too few arguments for cons")
    | [_val] => failwith("too few arguments for cons")
    | [myVal, ListV(myList)] => ListV([myVal, ...myList])
    | [_val1, _val2] => failwith("improper arguments for cons")
    | [_val1, _val2, ..._tl] => failwith("too many arguments for cons")
    };
let first: list(value) => value =
  alov =>
    switch (alov) {
    | [] => failwith("too few arguments for first")
    | [ListV(myList)] =>
      switch (myList) {
      | [] => failwith("Called first on empty list")
      | [hd, ..._tl] => hd
      }
    | [_val1] => failwith("non list argument in first")
    | [_val1, _val2, ..._tl] => failwith("too many arguments for first")
    };
let rest: list(value) => value =
  alov =>
    switch (alov) {
    | [] => failwith("too few arguments for rest")
    | [ListV(myList)] =>
      switch (myList) {
      | [] => failwith("Called rest on empty list")
      | [_hd] => ListV([])
      | [_hd, ...tl] => ListV(tl)
      }
    | [_val1] => failwith("non list argument in rest")
    | [_val1, _val2, ..._tl] => failwith("too many arguments for rest")
    };
let emptyq: list(value) => value =
  alov =>
    switch (alov) {
    | [] => failwith("too few arguments for empty?")
    | [ListV(myList)] =>
      switch (myList) {
      | [] => BoolV(true)
      | [hd, ..._tl] => BoolV(false)
      }
    | [_val1] => failwith("non list argument in empty?")
    | [_val1, _val2, ..._tl] => failwith("too many arguments for empty?")
    };
let consq: list(value) => value =
  alov =>
    switch (alov) {
    | [] => failwith("too few arguments for cons?")
    | [ListV(myList)] =>
      switch (myList) {
      | [] => BoolV(false)
      | [hd, ..._tl] => BoolV(true)
      }
    | [_val1] => failwith("non list argument in cons?")
    | [_val1, _val2, ..._tl] => failwith("too many arguments for cons?")
    };
let not: list(value) => value =
  alov =>
    switch (alov) {
    | [] => failwith("too few arguments for not")
    | [BoolV(myBool)] => BoolV(!myBool)
    | [_val1] => failwith("non bool argument in not")
    | [_val1, _val2, ..._tl] => failwith("too many arguments for not")
    };
let initialTle: environment = [
  [
    (Name("+"), BuiltinV({printedRep: "builtin+", bProc: add})),
    (Name("-"), BuiltinV({printedRep: "builtin-", bProc: subtract})),
    (Name("*"), BuiltinV({printedRep: "builtin*", bProc: multiply})),
    (Name("/"), BuiltinV({printedRep: "builtin/", bProc: divide})),
    (
      Name("remainder"),
      BuiltinV({printedRep: "builtinRemainder", bProc: modulo}),
    ),
    (Name("="), BuiltinV({printedRep: "builtin=", bProc: equalityq})),
    (Name("<"), BuiltinV({printedRep: "builtin<", bProc: lessThanq})),
    (Name(">"), BuiltinV({printedRep: "builtin>", bProc: greaterThanq})),
    (Name("<="), BuiltinV({printedRep: "builtin<=", bProc: leqq})),
    (Name(">="), BuiltinV({printedRep: "builtin>=", bProc: geqq})),
    (
      Name("equal?"),
      BuiltinV({printedRep: "builtinEqual?", bProc: equalq}),
    ),
    (
      Name("number?"),
      BuiltinV({printedRep: "builtinNumber?", bProc: numberq}),
    ),
    (Name("zero?"), BuiltinV({printedRep: "builtinZero?", bProc: zeroq})),
    (Name("cons"), BuiltinV({printedRep: "builtinCons", bProc: cons})),
    (Name("first"), BuiltinV({printedRep: "builtinFirst", bProc: first})),
    (Name("rest"), BuiltinV({printedRep: "builtinRest", bProc: rest})),
    (
      Name("empty?"),
      BuiltinV({printedRep: "builtinEmpty?", bProc: emptyq}),
    ),
    (Name("cons?"), BuiltinV({printedRep: "builtinCons?", bProc: consq})),
    (Name("not"), BuiltinV({printedRep: "builtinNot", bProc: not})),
  ],
];

/* TODO: write the header comment parts required by the Design Recipe
 * and implement parseExpression */

let lambdaNamesToName: list(concreteProgramPiece) => list(name) =
  lambdaNames =>
    List.map(
      symb =>
        switch (symb) {
        | SymbolC(myName) => Name(myName)
        },
      lambdaNames,
    );

let rec lstOfCondsToCondDatas: list(concreteProgramPiece) => list(condData) =
  condDatas =>
    List.map(
      condEntry =>
        switch (condEntry) {
        | ListC([condition, result]) => {
            conditionExpr: parseExpression(condition),
            resultExpr: parseExpression(result),
          }
        },
      condDatas,
    )
and processLetNames: list(concreteProgramPiece) => list(letPair) =
  myLetPairs =>
    List.map(
      pair =>
        switch (pair) {
        | ListC([SymbolC(myName), myVal]) => {
            pairName: Name(myName),
            pairExpr: parseExpression(myVal),
          }
        },
      myLetPairs,
    )
and parseExpression: concreteProgramPiece => expression =
  input =>
    switch (input) {
    | NumberC(num) => NumE(num)
    | SymbolC("true") => BoolE(true)
    | SymbolC("false") => BoolE(false)
    | SymbolC("empty") => EmptyE
    | SymbolC(someVar) => NameE(Name(someVar))
    | ListC([SymbolC("and"), conc1, conc2]) =>
      AndE(parseExpression(conc1), parseExpression(conc2))
    | ListC([SymbolC("or"), conc1, conc2]) =>
      OrE(parseExpression(conc1), parseExpression(conc2))
    | ListC([SymbolC("if"), conc1, conc2, conc3]) =>
      IfE({
        boolExpr: parseExpression(conc1),
        trueExpr: parseExpression(conc2),
        falseExpr: parseExpression(conc3),
      })
    | ListC([SymbolC("cond"), ListC(condDatas)]) =>
      CondE(lstOfCondsToCondDatas(condDatas))
    | ListC([SymbolC("lambda"), ListC(names), body]) =>
      LambdaE({
        nameList: lambdaNamesToName(names),
        lambdaBody: parseExpression(body),
      })
    | ListC([SymbolC("let"), ListC(myLetPairs), body]) =>
      LetE({
        letPairs: processLetNames(myLetPairs),
        letBody: parseExpression(body),
      })
    | ListC([SymbolC(someFunction), ...args]) =>
      ApplicationE(List.map(parseExpression, args))
    | something => failwith("Syntax error")
    };

/* TODO: write the header comment parts required by the Design Recipe
 * and implement parseDefinition */
let parseDefinition: concreteProgramPiece => definition =
  input =>
    switch (input) {
    | ListC([SymbolC("define"), SymbolC(myName), boundValue]) => (
        Name(myName),
        parseExpression(boundValue),
      )
    };

/* TODO: write the header comment parts required by the Design Recipe
 * and implement parsePiece */
let parsePiece: concreteProgramPiece => abstractProgramPiece =
  input =>
    switch (input) {
    | ListC([SymbolC("define"), ..._]) =>
      Definition(parseDefinition(input))
    | _ => Expression(parseExpression(input))
    };

/* TODO: write the header comment parts required by the Design Recipe
 * for parse */
let parse: concreteProgram => abstractProgram =
  input =>
    /* this will parse all of the pieces of this program,
     * giving us a list of pieces, our abstract syntax */
    List.map(parsePiece, input);

/* TODO: write the header comment parts required by the Design Recipe
 * and implement eval */
 let rec inBindingList: (bindingList, name) => bool =
  (alob, myName) =>
    switch (alob) {
    | [] => false
    | [(myName, _something), ..._tl] => true
    | [(someName, _something), ...tl] => inBindingList(tl, myName)
    };
let rec inEnviorment: (environment, name) => bool =
  (env, myName) =>
    switch (env) {
    | [[]] => false
    | [bindingList1, ...tl] =>
      if (inBindingList(bindingList1, myName)) {
        true;
      } else {
        inEnviorment(tl, myName);
      }
    };

let rec getFromBindingList: (bindingList, string) => value = (alob, myName) => switch(alob){
  | [(Name(myName), someValue), ..._tl] => someValue
  | [_hd, ...tl] => getFromBindingList(tl, myName)
}

let rec findVarInEnvironment: (environment, string) => value = (env, myName) => switch(env){
  | [[]] => failwith("variable used before definition")
  | [firstBindingList, ...tl] => if(inBindingList(firstBindingList, Name(myName))) {getFromBindingList(firstBindingList, myName)} else {findVarInEnvironment([tl], myName)}
}
let rec handleCond: (environment, environment, list(condData)) => expression =
  (tle, env, myCondDatas) =>
    switch (myCondDatas) {
    | [] => failwith("no conditions evaluated to true in a cond statement")
    | [{conditionExpr: condition, resultExpr: result}, ...tl] =>
      switch (eval(tle, env, condition)) {
      | BoolV(true) => result
      | BoolV(false) => handleCond(tle, env, tl)
      | _ => failwith("non-bool as condition in a cond statement")
      }
    }
and eval: (environment, environment, expression) => value =
  (tle, env, expr) =>
    switch (expr) {
    | NumE(myNum) => NumV(myNum)
    | BoolE(myBool) => BoolV(myBool)
    | EmptyE => ListV([])
    | NameE(Name(myName)) => findVarInEnvirenment(env, myName)
    | AndE(expr1, expr2) =>
      switch (eval(tle, env, expr1), eval(tle, env, expr2)) {
      | (BoolV(aBool1), BoolV(aBool2)) => BoolV(aBool1 && aBool2)
      | _ => failwith("non booleans in an and statement")
      }
    | OrE(expr1, expr2) =>
      switch (eval(tle, env, expr1), eval(tle, env, expr2)) {
      | (BoolV(aBool1), BoolV(aBool2)) => BoolV(aBool1 || aBool2)
      | _ => failwith("non booleans in an or statement")
      }
    | IfE({boolExpr: expr1, trueExpr: expr2, falseExpr: expr3}) =>
      switch (eval(tle, env, expr1)) {
      | BoolV(true) => eval(tle, env, expr2)
      | BoolV(false) => eval(tle, env, expr3)
      | _ => failwith("non booleans as first argument in an if statement")
      }
    | CondE(myCondDatas) =>
      eval(tle, env, handleCond(tle, env, myCondDatas))
    | LambdaE({nameList: myNames, lambdaBody: body}) =>
      ClosureV({cNameList: myNames, cExpr: body, cEnv: env})
    | LetE({letPairs: listOfLetPairs, letBody: body}) =>
      eval(
        tle,
        [
          List.map(
            myLetPair =>
              switch (myLetPair) { //This whole switch just sends a letPair to 
              | {pairName: Name(myName), pairExpr: expr} => (// an equivalent
                  Name(myName), // binding
                  eval(tle, env, expr),
                )
              },
            listOfLetPairs,
          ),
          ...env,
        ],
        body,
      )
    // | ApplicationE([NameE(Name(someFunction)), ...args]) =>
    };


/* TODO: write the header comment parts required by the Design Recipe */
let rec addDefinition: (environment, (name, expression)) => environment =
  (env, (id, expr)) =>
    if (inEnviorment(env, id)) {
      env;
    } else {
      switch (env) {
      | [bindingList1, ...tl] => [
          [(id, List.hd(process([Expression(expr)]))), ...bindingList1],
          ...tl,
        ]
      };
    }
and process: abstractProgram => list(value) =
  pieces => {
    let rec processHelper: (environment, abstractProgram) => list(value) =
      (tle, pieces) =>
        switch (pieces) {
        | [] => []
        | [Definition(d), ...tl] =>
          switch (d) {
          | (myName, ex) =>
            processHelper(addDefinition(tle, (myName, ex)), tl)
          }

        | [Expression(e), ...tl] => [
            eval(tle, [], e),
            ...processHelper(tle, tl),
          ]
        };
    processHelper(initialTle, pieces);
  };
/* TODO: write the header comment parts required by the Design Recipe
 * and implement stringOfValue*/
let rec stringOfValue: value => string =
  aValue => failwith("stringOfValue is not yet implemented");

/* TODO: write the header comment parts required by the Design Recipe */

/* TODO: write the header comment parts required by the Design Recipe */
let rackette: rawProgram => list(string) =
  program => List.map(stringOfValue, process(parse(readAll(program))));

/* TODO: Test Cases (we have included a few sample check-expects) */
// sample test: parseExpression on concreteProgramPiece
checkExpectExpression(
  parseExpression(SymbolC("empty")),
  EmptyE,
  "parse empty expression",
);
// sample test: parseExpression with read
checkExpectExpression(
  parseExpression(read("empty")),
  EmptyE,
  "read and parse empty expression",
);
