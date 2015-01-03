literal:
A literal is either a boolean variable (e.g. x4)
or a negated boolean variable (NOT x4, written here as -x4).

clause:
A term is one or more literals joined by an OR. (e.g. x1 Or -x2)
boolean variables may not repeat inside a clause.

term:
A clause is a set of one or more terms, connected with an AND;

Example 1 (Term T in CNF):

  (x1 | -x5 | x4) &
  (-x1 | x5 | x3 | x4) &
  (-x3 | x4).

Any boolean expression can be converted into CNF.


Language specification for DIMAC cnf:

Every line beginning "c" is a comment.
The first non-comment line must be of the form:
        p cnf NUMBER_OF_VARIABLES NUMBER_OF_CLAUSES
Each of the non-comment lines afterwards defines a clause.
Each of these lines is a space-separated list of variables;
a positive value means that corresponding variable (so 4 means x4),
and a negative value means the negation of that variable (so -5 means -x5).
Each line must end in a space and the number 0.

Example 2 (Term T in DIMAC CNF):

c Here is a comment.
p cnf 5 3
1 -5 4 0
-1 5 3 4 0
-3 -4 0
