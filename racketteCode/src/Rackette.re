open CS17SetupRackette;
open Read.Reader;
open Types;

/* TODO: fill this with your initial top level environment,
 * consisting of built-in procedures like + or empty? */
let initialTle: environment = [];

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
        | [Definition(d), ...tl] => [] /* TODO: replace this with correct definition-
                                          handling code and a recursive call to processHelper */

        | [Expression(e), ...tl] => [] /* TODO: replace this with correct expression-
                                          handling code and a recursive call to processHelper */
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
