# Prolog DIMAC DPLL SAT

## What it is?

A simple SAT-Solver written in Prolog to solve PC formulas
specified in DIMAC CNF. The solution is computed via the DPLL-Algorithm.


## How to use:
Open a terminal, navigate to the main folder (sat) and tpye:

```
	./sat 'path/to/dimacfile.cfn'
```
The solution and the model to the formula will be printed to the command line
if there exists any.

Try this Example:
```	
		./sat ./tests/testdata/testfile1.cnf
		
		true
		[ (1,T)]%
		
```
