# Type inference
In this project, I implemented some of the type inference mechanisms used in Haskell with Prolog. 
The below functionalities are implemted:

* Add more basic functions (fType predicate)
* global variables with expression initialization
* global function definitions
* expression computation
* if statements
* "let in" statement for local variables
* where statements
* code blocks
* Write tests for all predicates especially infer (20 cases)

## Running the code
Start the SWI prolog interpreter with
```
swipl.exe
```
Then load code with
```
[typeInf].
```

## Testing the code
There are 79 tests in ```typeInf.plt```, which contains unit tests for most predicates, and 20 tests in ```infer.plt```, which contains 20 tests for ```infer``` predicate only.
To load the test file in SWI prolog interpreter, do
```
["typeInf.plt"].
```
or 
```
["infer.plt"].
```
Then run the test use 
```
run_tests.
```
You need to exit SWI prolog interpreter with
```
halt.
```
then start a new SWI prolog interpreter in order to successfully load a different test.

## Notes
Recursive function is also tested in ```infer.plt```.

2 of the infer tests fail the second time ```infer.plt``` is run in the same SWI prolog interpreter possibly because ```gvar()``` is not cleaned up. To test this 
test file again, start a new SWI prolog interpreter.
