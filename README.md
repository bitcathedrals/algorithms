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
length of performance is: 95
 ran in: 0.853856086730957

 making nearest variable local - V4 - make the nearest varable in find_substring local instead of object

 michaelmattie@beast Algorithms % make no-repeat-benchmark
 source venv/bin/activate; export PYTHONPATH="$PYTHONPATH:/Users/michaelmattie/coding/Algorithms/src:/Users/michaelmattie/coding/Algorithms/tests:"; python /Users/michaelmattie/coding/Algorithms/tests/algorithms/test_longest_sub_str_no_repeats.py
 length of performance is: 95
  ran in: 0.6220526695251465

  make occur_map local in pre-compute - V5 - make occur_map a local variable

  michaelmattie@beast Algorithms % make no-repeat-benchmark
source venv/bin/activate; export PYTHONPATH="$PYTHONPATH:/Users/michaelmattie/coding/Algorithms/src:/Users/michaelmattie/coding/Algorithms/tests:"; python /Users/michaelmattie/coding/Algorithms/tests/algorithms/test_longest_sub_str_no_repeats.py
length of performance is: 95
 ran in: 0.29556870460510254

look behind memory bag - V6 - use a new algorithm where we look to remembered characters to see if we have already seen them. not as fast.

 michaelmattie@beast Algorithms % make no-repeat-benchmark
 source venv/bin/activate; export PYTHONPATH="$PYTHONPATH:/Users/michaelmattie/coding/Algorithms/src:/Users/michaelmattie/coding/Algorithms/tests:"; python /Users/michaelmattie/coding/Algorithms/tests/algorithms/test_longest_sub_str_no_repeats.py
 length of performance is: 95
  ran in: 0.497485876083374
