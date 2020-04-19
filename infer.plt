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

%global variables from local scope
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

:-end_tests(typeInf).