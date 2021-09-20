First run with implementation V1 - the character occurance lookup table

michaelmattie@beast Algorithms % make no-repeat-benchmark
source venv/bin/activate; export PYTHONPATH="$PYTHONPATH:/Users/michaelmattie/coding/Algorithms/src:/Users/michaelmattie/coding/Algorithms/tests:"; python /Users/michaelmattie/coding/Algorithms/tests/algorithms/test_longest_sub_str_no_repeats.py
length of performance is: 95
 ran in: 22.648531913757324

 Second run with implementation V2 - the occurance map, a 1:1 map containing the position of the
 next re-currence of the character.

 michaelmattie@beast Algorithms % make no-repeat-benchmark
source venv/bin/activate; export PYTHONPATH="$PYTHONPATH:/Users/michaelmattie/coding/Algorithms/src:/Users/michaelmattie/coding/Algorithms/tests:"; python /Users/michaelmattie/coding/Algorithms/tests/algorithms/test_longest_sub_str_no_repeats.py
length of performance is: 95
 ran in: 1.242314100265503

 Loop inlining - V3 - remove a function call by inlining it in the main Loop

 michaelmattie@beast Algorithms % make no-repeat-benchmark
source venv/bin/activate; export PYTHONPATH="$PYTHONPATH:/Users/michaelmattie/coding/Algorithms/src:/Users/michaelmattie/coding/Algorithms/tests:"; python /Users/michaelmattie/coding/Algorithms/tests/algorithms/test_longest_sub_str_no_repeats.py
length of performance is: 0
 ran in: 0.03775596618652344
 
