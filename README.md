# Algorithms for detecting the longest non-repeating substring

I wrote three algorithms: The first used a lookup table constructed with the
positions of repeated characters. The second used backward looking memoization.
The third I called skipper made a single pass and was able to both memoize
and forget.

## First run with implementation V1 - the character occurance lookup table

A lookup table for the occurances in the string. Implemented as a dict keyed
by character. A list of all the positions for that char are in the table. Data
structure design and traversal was klunky and slow.

>length of performance is: 95  
> ran in: 22.648531913757324  

## Second run with implementation V2 - the occurance map

a 1:1 map containing the position of the next re-currence of the character. compiled from the
occurance lookup table this is two passes of pre-compute.

This allows the algorithm to simply scan the string, and keep shrinking the
non-repeating range by taking the min of the nearest and the next occurance.

This was insanely fast compared to the first lookup table.

> length of performance is: 95  
> ran in: 1.242314100265503  

## Loop inlining - V3 - remove a function call by inlining it in the main Loop

Function calls are expensive, it was easy to inline so I removed the call
and it ran much faster.

> length of performance is: 95  
> ran in: 0.853856086730957  

## localize nearest - V4 - make the nearest varable in find_substring local instead of object

localizing variables can greatly speed up a Python program.

> length of performance is: 95  
>  ran in: 0.6220526695251465  

## localize occur_map in pre-compute - V5 - make occur_map a local variable


After localizing optimization in the pre-compute function I arrived at my
best time for the pre-compute algorithm.

> length of performance is: 95  
> ran in: 0.29556870460510254  

## look behind Memoize - V6

Ise a new algorithm where we look to remembered characters to see if we have already seen them. not as fast. These timings are not consistent with other results, so look at "Memoize" results in future
runs. But Map = 279ms, Memoize = 429ms on average.

> With Map length is: 95  
>  With Map ran in: 0.44652485847473145  
>  With Memory length is: 95  
>  With Memory ran in: 0.45049095153808594  

##  skipping algorithm: - V7

New one pass skiping algorithm. I was shocked that it was significantly slower
than the pre-compute. I attribute this to the use of cheap operations in the inner
core of pre-compute for it's speed. I think what is slowing down skip is
the expense of the dictionary manipulation.

It's starting speed isn't bad for no optimization.

> With Map length is: 95  
> With Map ran in: 0.2790658473968506  
> With Memoize length is: 95  
> With Memoize ran in: 0.42917799949645996  
> With Skipping length is: 95  
> With Skipping ran in: 0.47572994232177734  

## Ordered Dict optimization to skipper - V8

Major optimization, switch to an OrderedDict lookup, which allows us to
index by char,position - but also traverse the elements by insertion age
so all the positions are basically sorted. Now we just walk up the elements
until we reach a stopping point for removing old entries and just break instead of
iterating through the entire table every time like a filter function.

> With Map length is: 95  
> With Map ran in: 0.27973079681396484  
> With Memoize length is: 95  
> With Memoize ran in: 0.43615102767944336  
> With Skipping length is: 95  
> With Skipping ran in: 0.3790132999420166  

## Function inlining and caching - V9

Small optimization to inline the max call in a tight part of the Loop
and some smarter use of variables to cache repeated lookups.

> With Map length is: 95
> With Map ran in: 0.2730729579925537
> With Memoize length is: 95
> With Memoize ran in: 0.42336082458496094
> With Skipping length is: 95
> With Skipping ran in: 0.3557901382446289
