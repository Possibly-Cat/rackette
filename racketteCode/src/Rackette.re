open CS17SetupRackettePREDESIGNCHECK;
open Read.Reader;
open TypesPREDESIGNCHECK;

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
let lessThan: list(value) => value =
  alov =>
    switch (alov) {
    | [] => failwith("too few arguments for less than")
    | [NumV(_hd)] => failwith("too few arguments for less than")
    | [NumV(hd), NumV(tl)] => BoolV(hd > tl)
    | [NumV(_e1), NumV(_e2), NumV(_e3), ..._tl] =>
      failwith("too many arguments for less than")
    | _lst => failwith("non-number arguments in less than")
    };
let greaterThan: list(value) => value =
  alov =>
    switch (alov) {
    | [] => failwith("too few arguments for greater than")
    | [NumV(_hd)] => failwith("too few arguments for greater than")
    | [NumV(hd), NumV(tl)] => BoolV(hd > tl)
    | [NumV(_e1), NumV(_e2), NumV(_e3), ..._tl] =>
      failwith("too many arguments for greater than")
    | _lst => failwith("non-number arguments in greater than")
    };
let leq: list(value) => value =
  alov =>
    switch (alov) {
    | [] => failwith("too few arguments for <=")
    | [NumV(_hd)] => failwith("too few arguments for <=")
    | [NumV(hd), NumV(tl)] => BoolV(hd <= tl)
    | [NumV(_e1), NumV(_e2), NumV(_e3), ..._tl] =>
      failwith("too many arguments for <=")
    | _lst => failwith("non-number arguments in <=")
    };
let geq: list(value) => value =
  alov =>
    switch (alov) {
    | [] => failwith("too few arguments for >=")
    | [NumV(_hd)] => failwith("too few arguments for >=")
    | [NumV(hd), NumV(tl)] => BoolV(hd >= tl)
    | [NumV(_e1), NumV(_e2), NumV(_e3), ..._tl] =>
      failwith("too many arguments for >=")
    | _lst => failwith("non-number arguments in >=")
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
    (Name("<"), BuiltinV({printedRep: "builtin<", bProc: lessThan})),
    (Name(">"), BuiltinV({printedRep: "builtin>", bProc: greaterThan})),
    (Name("<="), BuiltinV({printedRep: "builtin<=", bProc: leq})),
    (Name(">="), BuiltinV({printedRep: "builtin>=", bProc: geq})),
    // (Name("equal?"), BuiltinV({printedRep: "builtinEqual?", bProc: minus})),
    // (Name("number?"), BuiltinV({printedRep: "builtinNumber?", bProc: minus})),
    // (Name("zero?"), BuiltinV({printedRep: "builtinZero?", bProc: minus})),
    // (Name("cons"), BuiltinV({printedRep: "builtinCons", bProc: minus})),
    // (Name("first"), BuiltinV({printedRep: "builtinFirst", bProc: minus})),
    // (Name("rest"), BuiltinV({printedRep: "builtinRest", bProc: minus})),
    // (Name("empty?"), BuiltinV({printedRep: "builtinEmpty?", bProc: minus})),
    // (Name("cons?"), BuiltinV({printedRep: "builtinCons?", bProc: minus})),
    // (Name("not"), BuiltinV({printedRep: "builtinNot", bProc: minus})),
  ],
];

/* TODO: write the header comment parts required by the Design Recipe
 * and implement parseExpression */
let rec parseExpression: concreteProgramPiece => expression =
  input => failwith("parseExpression is not yet implemented");

/* TODO: write the header comment parts required by the Design Recipe
 * and implement parseDefinition */
let parseDefinition: concreteProgramPiece => definition =
  input => failwith("parseDefinition is not yet implemented");

/* TODO: write the header comment parts required by the Design Recipe
 * and implement parsePiece */
let parsePiece: concreteProgramPiece => abstractProgramPiece =
  input =>
    switch (input) {
    | ListC([SymbolC("define"), ..._]) =>
      failwith("definitions not yet parsed")
    | _ => failwith("expressions not yet parsed")
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
let rec eval: (environment, environment, expression) => value =
  (tle, env, expr) =>
    /* NOTE: tle is top level environment and env is local environment */
    failwith("eval is not yet implemented");

/* TODO: write the header comment parts required by the Design Recipe */
let addDefinition: (environment, (name, expression)) => environment =
  (env, (id, expr)) => failwith("addDefinition is not yet implemented");

/* TODO: write the header comment parts required by the Design Recipe
 * and implement stringOfValue*/
let rec stringOfValue: value => string =
  aValue => failwith("stringOfValue is not yet implemented");

/* TODO: write the header comment parts required by the Design Recipe */
let process: abstractProgram => list(value) =
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
            eval(initialTle, tle, e),
            ...processHelper(tle, tl),
          ]
        };
    processHelper(initialTle, pieces);
  };

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
