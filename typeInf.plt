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
test(typeExp_iplus) :- 
    typeExp(iplus(int,int), int).

% this test should fail
test(typeExp_iplus_F, [fail]) :-
    typeExp(iplus(int, int), float).

test(typeExp_iplus_T, [true(T == int)]) :-
    typeExp(iplus(int, int), T).

% fplus
test(typeExp_fplus) :-
    typeExp(fplus(float, float), float).

test(typeExp_fplus_F, [fail]) :-
    typeExp(fplus(float, float), int).

test(typeExp_fplus_T, [true(T == float)]) :-
    typeExp(fplus(float, float), T).

% ifplus
test(typeExp_ifplus) :-
    typeExp(ifplus(int, float), float).

test(typeExp_ifplus_F, [fail]) :-
    typeExp(ifplus(int, float), int).

test(typeExp_ifplus_T, [true(T == float)]) :-
    typeExp(ifplus(int, float), T).

% fiplus
test(typeExp_fiplus) :-
  typeExp(fiplus(float, int), float).

test(typeExp_fiplus_F, [fail]) :-
  typeExp(fiplus(float, int), int).

test(typeExp_ifplus_T, [true(T == float)]) :-
  typeExp(fiplus(float, int), T).

% iminus
test(typeExp_iminus) :-
    typeExp(iminus(int, int), int).

test(typeExp_iminus_F, [fail]) :-
    typeExp(iminus(int, int), float).

test(typeExp_iminus_T, [true(T == int)]) :-
    typeExp(iminus(int, int), T).

% fminus
test(typeExp_fminus) :-
  typeExp(fminus(float, float), float).

test(typeExp_fminus_F, [fail]) :-
    typeExp(fminus(float, float), int).

test(typeExp_fminus_T, [true(T == float)]) :-
    typeExp(fminus(float, float), T).

% ifminus
test(typeExp_ifminus) :-
  typeExp(ifminus(int, float), float).

test(typeExp_ifminus_F, [fail]) :-
  typeExp(ifminus(int, float), int).

test(typeExp_ifminus_T, [true(T == float)]) :-
  typeExp(ifminus(int, float), T).

% fiminus
test(typeExp_fiminus) :-
  typeExp(fiminus(float, int), float).

test(typeExp_fiminus_F, [fail]) :-
  typeExp(fiminus(float, int), int).

test(typeExp_ifminus_T, [true(T == float)]) :-
  typeExp(fiminus(float, int), T).

% and
test(typeExp_and) :-
    typeExp(and(bool, bool), bool).

test(typeExp_and_F, [fail]) :-
    typeExp(and(bool, bool), string).

% or
test(typeExp_or) :-
  typeExp(or(bool, bool), bool).

test(typeExp_or_F, [fail]) :-
  typeExp(or(bool, bool), string).

% not
test(typeExp_not) :-
  typeExp(not(bool), bool).

test(typeExp_not_F, [fail]) :-
  typeExp(not(bool), string).

% fToInt
test(typeExp_fToInt) :-
    typeExp(fToInt(float), int).

test(typeExp_fToInt_F, [fail]) :-
    typeExp(fToInt(float), float).

test(typeExp_fToInt_T, [true(T == int)]) :-
    typeExp(fToInt(float), T).

% iToFloat
test(typeExp_iToFloat) :-
    typeExp(iToFloat(int), float).

test(typeExp_iToFloat_F, [fail]) :-
    typeExp(iToFloat(int), int).

test(typeExp_iToFloat_T, [true(T == float)]) :-
    typeExp(iToFloat(int), T).

% print
test(typeExp_print) :-
    typeExp(print(string), unit).

test(typeExp_print_bool) :-
    typeExp(print(bool), unit).

test(typeExp_print_F, [fail]) :-
    typeExp(print(string), string).

test(typeExp_print_T, [true(T == unit)]) :-
    typeExp(print(string), T).

% NOTE: use nondet as option to test if the test is nondeterministic

% test for statement with state cleaning
test(typeStatement_gvar, [nondet, true(T == int)]) :- % should succeed with T=int
    deleteGVars(), /* clean up variables */
    typeStatement(gvLet(v, T, iplus(X, Y)), unit),
    assertion(X == int), assertion( Y == int), % make sure the types are int
    gvar(v, int). % make sure the global variable is defined

% same test as above but with infer 
test(infer_gvar, [nondet]) :-
    infer([gvLet(v, T, iplus(X, Y))], unit),
    assertion(T==int), assertion(X==int), assertion(Y=int),
    gvar(v,int).

% test custom function with mocked definition
test(mockedFct, [nondet]) :-
    deleteGVars(), % clean up variables since we cannot use infer
    asserta(gvar(my_fct, [int, float])), % add my_fct(int)-> float to the gloval variables
    typeExp(my_fct(X), T), % infer type of expression using or function
    assertion(X==int), assertion(T==float). % make sure the types infered are correct

test(simple_if, [nondet]) :-
    typeStatement(if(true, [3], [4]), int),
    typeStatement(if(or(true, true), [3], [4]), int),
    typeStatement(if(3 < -90, ["true"], ["false"]), string),
    typeStatement(if(not(false), [false], [true]), bool).

:-end_tests(typeInf).
