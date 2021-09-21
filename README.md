# Algorithms for detecting the longest non-repeating substring

## First run with implementation V1 - the character occurance lookup table

A lookup table for the occurances in the string. Implemented as a dict keyed
by character.

>length of performance is: 95  
> ran in: 22.648531913757324  

## Second run with implementation V2 - the occurance map

a 1:1 map containing the position of the next re-currence of the character. compiled from the
occurance lookup table this is two passes of pre-compute.

This is the fastest without micro-optimization

> length of performance is: 95  
> ran in: 1.242314100265503  

## Loop inlining - V3 - remove a function call by inlining it in the main Loop

> length of performance is: 95  
> ran in: 0.853856086730957  

## making nearest variable local - V4 - make the nearest varable in find_substring local instead of object

> length of performance is: 95  
>  ran in: 0.6220526695251465  

##  make occur_map local in pre-compute - V5 - make occur_map a local variable

> length of performance is: 95  
> ran in: 0.29556870460510254  

## look behind Memoize - V6

use a new algorithm where we look to remembered characters to see if we have already seen them. not as fast.

> With Map length is: 95  
>  With Map ran in: 0.44652485847473145  
>  With Memory length is: 95  
>  With Memory ran in: 0.45049095153808594  

##  skipping algorithm: - V7

new algorithm.

> With Map length is: 95  
> With Map ran in: 0.2790658473968506  
> With Memoize length is: 95  
> With Memoize ran in: 0.42917799949645996  
> With Skipping length is: 95  
> With Skipping ran in: 0.47572994232177734  

## Ordered Dict optimization to skipper - V8

> With Map length is: 95  
> With Map ran in: 0.27973079681396484  
> With Memoize length is: 95  
> With Memoize ran in: 0.43615102767944336  
> With Skipping length is: 95  
> With Skipping ran in: 0.3790132999420166  
