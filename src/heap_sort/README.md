# Heap Sort

I wanted to explore the heap sort algorithm since there were slight
variations on implementation and I believed I had found a error
in the textbook I was studying.

## bottom up

The text book specified a bottom up swapping strategy which I
believed produced errors.

## Wikipedia/MIT

The wikipedia canonical implementation implemented a top down
approach. This produced error free sorts.

## Findings

After writing both versions and checking the results with visual verification via .dot graph generation and checking against a known good implementation I found that the Wikipedia/MIT Introduction to Algorithms approach was the correct one.

This was a good lesson in checking the results of implementations even when they appear to be correct.