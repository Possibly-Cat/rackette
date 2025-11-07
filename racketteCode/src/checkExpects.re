open Rackette;
open Types;
open Read.Reader;
open CS17SetupRackette;

checkExpectConcreteProgram(
    readAll("(+ 4 5)"),
    [ListC([SymbolC("+"), NumberC(4), NumberC(5)])],
    "readAll test - 1"
)
checkExpectConcreteProgram(
    readAll("(+ 4 (* 16 932))"),
    [ListC([SymbolC("+"), NumberC(4), ListC([SymbolC("*"), NumberC(16), NumberC(932)])])],
    "readAll test - 2"
)
checkExpectConcreteProgram(
    readAll("(first (cons 3 (cons 2 empty)))"),
    [ListC([SymbolC("first"), ListC([SymbolC("cons"), NumberC(3), ListC([SymbolC("cons"), NumberC(2), SymbolC("empty")])])])],
    "readAll test - 3"
)
