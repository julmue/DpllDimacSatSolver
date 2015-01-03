# Prolog DIMAC DPLL SAT

## What it is?

A simple SAT-Solver written in Prolog to solve PC formulas
specified in DIMAC CNF. The solution is computed via the DPLL-Algorithm.


## How to use:

### From Command Line
Open a terminal, navigate to the main folder (sat) and type:

```
	./sat 'path/to/dimacfile.cfn'
```
In command line mode only one model to the formula will be returned even if there 
are multiple solutions.

Try this Example:
```	
		./sat ./tests/testdata/testfile1.cnf
		
		true
		[ (1,T)]%
		
```

### Internal Use

 *  Go to `./src`
 *  Start `swipl`
 *  Load '[sat_internal].'

Now you can use the relation `dpll` to solve the formula,
DIMAC can be specified as a list of lists of integers,
where each disjunctive clause corresponds to a list of integers.
Multiple solutions can be queried:


```
	?- dpll([[-1,2],[-2]],X).
	X = [ (1, 'F'), (2, 'F')] ;
	false.

	?- dpll([[-1,2],[1,-2]],X).
	X = [ (1, 'T'), (2, 'T')] ;
	X = [ (1, 'F'), (2, 'F')] ;
	false.

```

## DIMAC
For the specification of the DIMAC file format consult `./docu/DIMAC_CNF.md`.
