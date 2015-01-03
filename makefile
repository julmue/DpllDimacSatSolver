main:

test:
	swipl -g "run_tests,halt" ./tests/parser_dimac_tests.pl
	swipl -g "run_tests,halt" ./tests/sat_dimac_tests.pl
