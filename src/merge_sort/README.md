# merge sort in Common Lisp

I wrote a merge sort in common lisp. The first iteration with a million integers was very slow. 

I determined that the basic list data structure was not well suited to the merging operations.

I implemented a double ended queue data structure and saw a massive increase in performance.


```bash
DEFAULT LIBRARY FUNCTIONS

merge-sort.lisp -> running now
Evaluation took:
  0.402 seconds of real time
  0.401896 seconds of total run time (0.374674 user, 0.027222 system)
  [ Run times consist of 0.012 seconds GC time, and 0.390 seconds non-GC time. ]
  100.00% CPU
  888,749,204 processor cycles
  1,599,489,264 bytes consed

sorted = to long to print

MY QUEUE FUNCTIONS

merge-sort.lisp -> running now
Evaluation took:
  0.009 seconds of real time
  0.008828 seconds of total run time (0.008275 user, 0.000553 system)
  100.00% CPU
  19,489,098 processor cycles
  2,752,416 bytes consed
```
