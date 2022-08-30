# Algorithms

This is a workspace for a study of algorithms and data structures experimenting in different languages and techniques for fun and discovery of problem solving techniques.

## Longest non-repeating substring

This python project to solve finding the longest non repeating substring is interesting in that people find so many ways to solve it.

I found three ways to solve it and benchmark the different approaches revealing important insights into high performance Python code.

[Python - longest_nonrepeat_substring](src/longest_norepeat_substring/)

## Heap Sort in python

Heap Sort in Python is interesting in that I found the algorithm in a book and was intrigued. However when I implemented it I found bugs.

After some research I came to the conclusion that the textbook was wrong. Also much of the related code in the internet had subtle bugs as well.

My exploration of the algorithm is presented as the textbook algorithm, and the MIT version of it that is also the wikipedia version of the algorithm.

One nice feature is that it produces .dot graphs
To help verify that the sorted heaps are correct.

[Python - heap sort](src/heap_sort/)

## maze
A maze navigation algorithm was given to me in a challenge in an interview. I did not complete it in 45 minutes, however I did complete it later since I was fascinated by how to solve the problem.

Here is my solution:

[Python - maze](src/maze/)

## merge sort

I was interested in the merge sort algorithm as I thought it would be a good fit for LISP, which is rare. I wrote it in Common Lisp, more specifically the SBCL dialect.

I compiled the algorithm and benchmarked the result.

I re-designed the algorithm to use a double ended queue which massively speed up the algorithm and dramatically reduced memory consumption.

[Common Lisp](src/merge_sort/)

## shell - co-process

coproc is a unique approach of implementing a shell co-process that communicate with each other shell code that eval'd out of the pipes.

[shell](src/coproc/)

## FutureTask and Singleton

I have a lot of experience with Java code dating back to 2005. One of the two more interesting products was a class implementation of FutureTask with an internal worker thread.

I also wrote a singleton implementation which is notoriously difficult to get right.

[Java](src/java/)

## parser.el

I decided to write a parser in Emacs Lisp maximizing the use of macros to create a Domain Specific Language compiler that would generate compiled PEG/CFG parsers.

I was able to get the PEG semantics implemented fully and was working on CFG integration when I had to put it aside. However it is a tour de force of LISP meta-programming techniques including constructs that extended Lisp itself to produce highly optimized code.

[Elisp](src/elisp/)

 


