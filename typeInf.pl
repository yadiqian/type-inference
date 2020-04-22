:- dynamic gvar/2.

typeExp(X, int) :-
    integer(X).

typeExp(X, float) :-
    float(X).

typeExp(X, bool) :-
    typeBoolExp(X).

typeExp(X, string) :-
    string(X).

/* match functions by unifying with arguments 
    and infering the result
*/
typeExp(Fct, T):-
    \+ var(Fct), /* make sure Fct is not a variable */ 
    \+ atom(Fct), /* or an atom */
    functor(Fct, Fname, _Nargs), /* ensure we have a functor */
    !, /* if we make it here we do not try anything else */
    Fct =.. [Fname|Args], /* get list of arguments */
    append(Args, [T], FType), /* make it loook like a function signature */
    functionType(Fname, TArgs), /* get type of arguments from definition */
    typeExpList(FType, TArgs). /* recurisvely match types */

/* propagate types */
typeExp(T, T) :- 
    bType(T).

typeExp(X, T) :-
    gvar(X, T).

/* list version to allow function mathine */
typeExpList([], []).
typeExpList([Hin|Tin], [Hout|Tout]):-
    typeExp(Hin, Hout), /* type infer the head */
    typeExpList(Tin, Tout). /* recurse */

hasComparison(int).
hasComparison(float).
hasComparison(string). 

/* predicate to infer types for boolean expressions */
typeBoolExp(true).
typeBoolExp(false).
typeBoolExp(X) :-
  gvar(X, bool). 
typeBoolExp(X < Y) :-
    canCompare(X, Y).
typeBoolExp(X > Y) :-
    canCompare(X, Y).
typeBoolExp(X == Y) :-
    canCompare(X, Y).
typeBoolExp(and(X, Y)) :-
    areBool(X, Y).
typeBoolExp(or(X, Y)) :-
    areBool(X, Y).
typeBoolExp(not(X)) :-
    typeBoolExp(X).
    
canCompare(X, Y) :- 
    typeExp(X, T),
    typeExp(Y, T),
    hasComparison(T).

areBool(X, Y) :-
    typeBoolExp(X),
    typeBoolExp(Y).


/* TODO: add statements types and their type checking */

typeStatement(X, T) :-
    typeExp(X, T).

/* global variable definition
    Example:
        gvLet(v, T, int) ~ let v = 3;
 */
typeStatement(gvLet(Name, T, Code), unit) :-
    atom(Name), /* make sure we have a bound name */
    typeExp(Code, T), /* infer the type of Code and ensure it is T */
    bType(T), /* make sure we have an infered type */
    asserta(gvar(Name, T)). /* add definition to database */

/* global function definition
*/
typeStatement(funcLet(Name, Params, Stmts), unit) :-
    atom(Name),
    asserta(gvar(Name, Params)),
    typeCode(Stmts, T),
    paramList(Params, T).

/* expression computation
*/
typeStatement(exp(Exp), T) :-
  typeExp(Exp, T),
  bType(T).

/* if statements are encodes as:
    if(condition:Boolean, trueCode: [Statements], falseCode: [Statements])
*/
typeStatement(if(Cond, TrueB, FalseB), T) :-
    typeBoolExp(Cond),
    typeCode(TrueB, T),
    typeCode(FalseB, T).

/* let in :
    let n = Statement in [Statement]
*/
typeStatement(lvLet(Name, T, Code, Stmts), Type) :-
    atom(Name),
    typeExp(Code, T),
    bType(T),
    asserta(gvar(Name, T)),
    typeCode(Stmts, Type),
    retract(gvar(Name, T)).

/* where
    Statement where
      a = Statement
      b = Statement
      ...
*/
typeStatement(where(Code, Vars), T) :-
    whereVar(Vars),
    typeCode(Code, T),
    removeVar(Vars).

/* code blocks
    [Statement]
*/
typeStatement(block(Code), T) :-
    executeBlocks(Code, T).

executeBlocks([Block], T) :-
    typeCode(Block, T).
executeBlocks([B1, B2|Block], T) :-
    typeCode(B1, _T),
    executeBlocks([B2|Block], T).

whereVar([]).
whereVar([Name, T, Code]) :- 
    typeExp(Code, T),
    asserta(gvar(Name, T)).
whereVar([H|Tail]) :-
    whereVar(H),
    whereVar(Tail).

removeVar([]).
removeVar([Name, T, Code]) :-
    typeExp(Code, T),
    retract(gvar(Name, T)).
removeVar([H|Tail]) :-
    removeVar(H),
    removeVar(Tail).

/* recursively check if function return type matches the 
    definition in the parameter list
*/
paramList([], unit).
paramList([T], T).
paramList([_H|T], Type) :-
    paramList(T, Type).

/* Code is simply a list of statements. The type is 
    the type of the last statement 
*/
typeCode([S], T):-typeStatement(S, T).
typeCode([S, S2|Code], T):-
    typeStatement(S,_T),
    typeCode([S2|Code], T).

/* top level function */
infer(Code, T) :-
    is_list(Code), /* make sure Code is a list */
    deleteGVars(), /* delete all global definitions */
    typeCode(Code, T).

/* Basic types
    TODO: add more types if needed
 */
bType(int).
bType(float).
bType(string).
bType(bool).
bType(unit). /* unit type for things that are not expressions */
/*  functions type.
    The type is a list, the last element is the return type
    E.g. add: int->int->int is represented as [int, int, int]
    and can be called as add(1,2)->3
 */
bType([H]):- bType(H).
bType([H|T]):- bType(H), bType(T).

/*
    TODO: as you encounter global variable definitions
    or global functions add their definitions to 
    the database using:
        asserta( gvar(Name, Type) )
    To check the types as you encounter them in the code
    use:
        gvar(Name, Type) with the Name bound to the name.
    Type will be bound to the global type
    Examples:
        g

    Call the predicate deleveGVars() to delete all global 
    variables. Best wy to do this is in your top predicate
*/

deleteGVars():-retractall(gvar), asserta(gvar(_X,_Y):-false()).

/*  builtin functions
    Each definition specifies the name and the 
    type as a function type
*/

% iplus :: int -> int -> int
fType(iplus, [int,int,int]).
% fplus :: float -> float -> float
fType(fplus, [float, float, float]).
% ifplus :: int -> float -> float
fType(ifplus, [int, float, float]).
% fiplus :: float -> int -> float
fType(fiplus, [float, int, float]).

% iminus :: int -> int -> int
fType(iminus, [int, int, int]).
% fminus :: float -> float -> float
fType(fminus, [float, float, float]).
% ifminus :: int -> float -> float
fType(ifminus, [int, float, float]).
% fiminus :: float -> int -> float
fType(fiminus, [float, int, float]).

fType(itimes, [int, int, int]).

% itimes :: int -> int -> int
fType(itimes, [int, int, int]).
% ftimes :: float -> float -> float
fType(ftimes, [float, float, float]).
% iftimes :: int -> float -> float
fType(iftimes, [int, float, float]).
% fitimes :: float -> int -> float
fType(fitimes, [float, int, float]).

% idiv :: int -> int -> int
fType(idiv, [int, int, int]).
% fdiv :: float -> float -> float
fType(fdiv, [float, float, float]).
% ifdiv :: int -> float -> float
fType(ifdiv, [int, float, float]).
% fidiv :: float -> int -> float
fType(fidiv, [float, int, float]).

% and :: bool -> bool -> bool
fType(and, [bool, bool, bool]).
% or :: bool -> bool -> bool
fType(or, [bool, bool, bool]).
% not :: bool -> bool
fType(not, [bool, bool]).

% fToInt :: float -> int
fType(fToInt, [float,int]).
% iToFloat :: int -> float
fType(iToFloat, [int,float]).

% concate :: string -> string -> string
fType(concate, [string, string, string]).
% print :: a -> unit
fType(print, [_X, unit]). /* simple print */

/* Find function signature
   A function is either built in using fType or
   added as a user definition with gvar(fct, List)
*/

% Check the user defined functions first
functionType(Name, Args):-
    gvar(Name, Args),
    is_list(Args). % make sure we have a function not a simple variable

% Check first built in functions
functionType(Name, Args) :-
    fType(Name, Args), !. % make deterministic

% This gets wiped out but we have it here to make the linter happy
gvar(_, _) :- false().
