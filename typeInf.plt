:- begin_tests(typeInf).
:- include(typeInf). 

/* Note: when writing tests keep in mind that 
    the use of of global variable and function definitions
    define facts for gvar() predicate. Either test
    directy infer() predicate or call
    delegeGVars() predicate to clean up gvar().
*/

% tests for typeExp
test(typeInt, [nondet]) :-
    typeExp(1, int),
    typeExp(-88, int).

test(typeFloat, [nondet]) :-
    typeExp(1.0, float),
    typeExp(-5.7621, float).

test(typeBool, [nondet]) :-
    typeExp(true, bool),
    typeExp(5 > -3, bool),
    typeExp(not(and(true, false)), bool).

test(typeString, [nondet]) :-
    typeExp("hello world", string).

% iplus
test(typeExp_iplus, [nondet]) :- 
    typeExp(iplus(int,int), int).

% this test should fail
test(typeExp_iplus_F, [fail, nondet]) :-
    typeExp(iplus(int, int), float).

test(typeExp_iplus_T, [true(T == int), nondet]) :-
    typeExp(iplus(int, int), T).

% fplus
test(typeExp_fplus, [nondet]) :-
    typeExp(fplus(float, float), float).

test(typeExp_fplus_F, [fail, nondet]) :-
    typeExp(fplus(float, float), int).

test(typeExp_fplus_T, [true(T == float), nondet]) :-
    typeExp(fplus(float, float), T).

% ifplus
test(typeExp_ifplus, [nondet]) :-
    typeExp(ifplus(int, float), float).

test(typeExp_ifplus_F, [fail, nondet]) :-
    typeExp(ifplus(int, float), int).

test(typeExp_ifplus_T, [true(T == float), nondet]) :-
    typeExp(ifplus(int, float), T).

% fiplus
test(typeExp_fiplus, nondet) :-
  typeExp(fiplus(float, int), float).

test(typeExp_fiplus_F, [fail, nondet]) :-
  typeExp(fiplus(float, int), int).

test(typeExp_ifplus_T, [true(T == float), nondet]) :-
  typeExp(fiplus(float, int), T).

% iminus
test(typeExp_iminus, [nondet]) :-
    typeExp(iminus(int, int), int).

test(typeExp_iminus_F, [fail, nondet]) :-
    typeExp(iminus(int, int), float).

test(typeExp_iminus_T, [true(T == int), nondet]) :-
    typeExp(iminus(int, int), T).

% fminus
test(typeExp_fminus, [nondet]) :-
  typeExp(fminus(float, float), float).

test(typeExp_fminus_F, [fail, nondet]) :-
    typeExp(fminus(float, float), int).

test(typeExp_fminus_T, [true(T == float), nondet]) :-
    typeExp(fminus(float, float), T).

% ifminus
test(typeExp_ifminus, [nondet]) :-
  typeExp(ifminus(int, float), float).

test(typeExp_ifminus_F, [fail, nondet]) :-
  typeExp(ifminus(int, float), int).

test(typeExp_ifminus_T, [true(T == float), nondet]) :-
  typeExp(ifminus(int, float), T).

% fiminus
test(typeExp_fiminus, [nondet]) :-
  typeExp(fiminus(float, int), float).

test(typeExp_fiminus_F, [fail, nondet]) :-
  typeExp(fiminus(float, int), int).

test(typeExp_ifminus_T, [true(T == float), nondet]) :-
  typeExp(fiminus(float, int), T).

% and
test(typeExp_and, [nondet]) :-
    typeExp(and(bool, bool), bool).

test(typeExp_and_F, [fail, nondet]) :-
    typeExp(and(bool, bool), string).

% or
test(typeExp_or, [nondet]) :-
  typeExp(or(bool, bool), bool).

test(typeExp_or_F, [fail, nondet]) :-
  typeExp(or(bool, bool), string).

% not
test(typeExp_not, [nondet]) :-
  typeExp(not(bool), bool).

test(typeExp_not_F, [fail, nondet]) :-
  typeExp(not(bool), string).

% fToInt
test(typeExp_fToInt, [nondet]) :-
    typeExp(fToInt(float), int).

test(typeExp_fToInt_F, [fail, nondet]) :-
    typeExp(fToInt(float), float).

test(typeExp_fToInt_T, [true(T == int), nondet]) :-
    typeExp(fToInt(float), T).

% iToFloat
test(typeExp_iToFloat, [nondet]) :-
    typeExp(iToFloat(int), float).

test(typeExp_iToFloat_F, [fail, nondet]) :-
    typeExp(iToFloat(int), int).

test(typeExp_iToFloat_T, [true(T == float), nondet]) :-
    typeExp(iToFloat(int), T).

% print
test(typeExp_print, [nondet]) :-
    typeExp(print(string), unit).

test(typeExp_print_bool, [nondet]) :-
    typeExp(print(bool), unit).

test(typeExp_print_F, [fail, nondet]) :-
    typeExp(print(string), string).

test(typeExp_print_T, [true(T == unit), nondet]) :-
    typeExp(print(string), T).

% whereVar
test(whereVar, [nondet]) :-
    deleteGVars(),
    whereVar([[a, Ta, ifplus(int, float)], [b, Tb, string]]),
    assertion(Ta == float),
    assertion(Tb == string),
    gvar(a, float),
    gvar(b, string).

% removeVar
test(removeVar, [nondet]) :-
    deleteGVars(),
    asserta(gvar(v, string)),
    asserta(gvar(v, int)),
    asserta(gvar(v, bool)),
    removeVar([[v, _T1, bool], [v, _T2, int]]),
    gvar(v, string),
    \+ gvar(v, int),
    \+ gvar(v, bool).

% paramList
test(paramList, [nondet]) :-
    paramList([], unit),
    paramList([int], int),
    paramList([string, float, void, bool], bool).

% NOTE: use nondet as option to test if the test is nondeterministic

% test for statement with state cleaning
test(typeStatement_gvar, [nondet, true(T == int)]) :- % should succeed with T=int
    deleteGVars(), /* clean up variables */
    typeStatement(gvLet(v, T, iplus(X, Y)), unit),
    assertion(X == int), assertion( Y == int), % make sure the types are int
    gvar(v, int). % make sure the global variable is defined

% test custom function with mocked definition
test(mockedFct, [nondet]) :-
    deleteGVars(), % clean up variables since we cannot use infer
    asserta(gvar(my_fct, [int, float])), % add my_fct(int)-> float to the gloval variables
    typeExp(my_fct(X), T), % infer type of expression using or function
    assertion(X==int), assertion(T==float). % make sure the types infered are correct

test(funcLet, [nondet]) :-
    deleteGVars(),
    typeStatement(funcLet(func, [float, string, int], [print(string), iplus(1, 2)]), T),
    assertion(T == unit), 
    gvar(func, [float, string, int]),
    typeExp(func(X, Y), Type),
    assertion(X == float), assertion(Y == string), assertion(Type == int).

test(simple_if, [nondet]) :-
    typeStatement(if(true, [3], [4]), int),
    typeStatement(if(or(true, true), [3], [4]), int),
    typeStatement(if(3 < -90, ["true"], ["false"]), string),
    typeStatement(if(not(false), [false], [true]), bool),
    typeStatement(if(iplus(3, 5) > 0, [if(true, [1], [2])], [if(true, [3], [4])]), int).

test(let, [nondet]) :-
    typeStatement(lvLet(x, Tx, iplus(Y, Z), [gvLet(w, Tw, x)]), T),
    assertion(Y == int), assertion(Z == int), assertion(Tx == int),
    assertion(Tw == int), assertion(T == unit),
    \+ gvar(x, int),
    gvar(w, int).

test(block, [nondet]) :-
    typeStatement(block([print(true), print(3), iplus(int, int)]), T),
    assertion(T == int).

:-end_tests(typeInf).
