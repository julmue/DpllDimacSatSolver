main:

test:
	swipl -g "run_tests,halt" ./tests/parser_internal_tests.pl
	swipl -g "run_tests,halt" ./tests/simplification_tests.pl
	swipl -g "run_tests,halt" ./tests/substitution_tests.pl
	swipl -g "run_tests,halt" ./tests/sat_tests.pl
