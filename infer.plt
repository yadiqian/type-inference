:- begin_tests(typeInf).
:- include(typeInf). 


% test expressions as statements
test(infer_exp, [nondet]) :-
  infer([
      int,
      float,
      bool
      ], T),
      assertion(T==bool).

% test global var
test(infer_gvar, [nondet]) :-
  infer([gvLet(v, T, iplus(X, Y))], unit),
  assertion(T==int), assertion(X==int), assertion(Y=int),
  gvar(v,int).

% test if statements
test(infer_if, [nondet]) :-
  infer([
      if(>(float,float), [iplus(int,int)], [iminus(int,int)])
      ], T),
      assertion(T==int).

% test global var and if
test(infer_gvLet_if, [nondet]) :-
  infer([
    gvLet(a, T, fToInt(X)),
    gvLet(b, U, a < 3),
    if(b, [print(b)], [print(a)])
  ], unit),
  assertion(T==int), assertion(X==float), assertion(U==bool),
  gvar(a, int),
  gvar(b, bool).

% test nested let in statements
test(infer_lvLet, [nondet]) :-
  deleteGVars(),
  infer([
      lvLet(x, Tx, iplus(int,int), [
          lvLet(y, Ty, fplus(float,float), [
              lvLet(z, Tz, <(float,float), [
                  print(z),
                  ifplus(x, y)
              ])
          ])
      ]) 
      ], T),

      assertion(T==float),
      assertion(Tx==int),
      assertion(Ty==float),
      assertion(Tz==bool),
      \+ gvar(x, _X),
      \+ gvar(y, _Y),
      \+ gvar(z, _Z).

% global variables from local scope
test(infer_global_from_local_scope, [nondet]) :-
  deleteGVars(),
  infer([
      gvLet(v, Tv, fminus(float,float)),
      lvLet(x, Tx, fminus(float,float), [
          lvLet(y, Ty, itimes(int,int), [
              lvLet(z, Tz, ==(float,float), [
                  fminus(v, x),
                  iminus(y, 10),
                  not(z)
              ])
          ])
      ])
      
      ], T),

      assertion(T==bool),
      assertion(Tv==float),
      assertion(Tx==float),
      assertion(Ty==int),
      assertion(Tz==bool),
      gvar(v, float),
      \+ gvar(x, _X),
      \+ gvar(y, _Y),
      \+ gvar(z, _Z).

% test definining and calling a function
test(infer_functionDefCall, [nondet]) :-
  deleteGVars(),
  infer([
      funcLet(runTest, [string, int, bool], [<(float, float)]),
      runTest(X,Y)
      ], bool),
      assertion(X==string),
      assertion(Y==int).

% test definining and calling a function
test(infer_functionDefCall, [nondet]) :-
  deleteGVars(),
  infer([
      funcLet(runTest, [string, int, float], [ifminus(4, -5.0)]),
      runTest(X,Y)
      ], T),
      assertion(X==string),
      assertion(Y==int),
      assertion(T==float).

% test where statement
test(infer_where, [nondet]) :-
    deleteGVars(),
    infer([
      gvLet(a, Ta1, string),
      where(fminus(a, b), [
          [a, Ta2, ifplus(int, float)], 
          [b, Tb, fminus(float, float)]
        ])
      ], T),
      assertion(Ta1==string),
      assertion(Ta2==float),
      assertion(Tb==float),
      assertion(T==float),
      gvar(a, string),
      gvar(a, int),
      \+ gvar(a, float),
      \+ gvar(b, float).

% test recusive function
test(infer_recursion, [nondet]) :-
    deleteGVars(),
    infer([
      gvLet(x, Tx, 10),
      funcLet(recur, [int, int], [
        if(or(==(x, 0), ==(x, 1)), [1], [
          iplus(
            recur(iminus(x, 1)), 
            recur(iminus(x, 2))
          )
        ])
      ]),        
      print(recur(x))
    ], T),

    gvar(x, int),
    assertion(Tx==int),
    assertion(T==unit).

:-end_tests(typeInf).
