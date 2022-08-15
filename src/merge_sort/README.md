# merge sort in Common Lisp

I wrote a merge sort in common lisp. The first iteration with
a million integers was very slow. 

I determined that the basic list data structure was not well suited
to the merging operations.

I implemented a double ended queue data structure and saw a massive
increase in performance.

