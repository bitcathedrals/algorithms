# parser.el

parser.el is an Emacs Lisp implementation of a PEG parser compiler. It uses meta-programming techniques heavily to both define the Domain Specific Language for PEG grammars, and to implement the compiler itself.

I used macros to save and restore the scope of functions extending Elisp, and wrote a algorithm that used a "paper tape" stream model to implement greedy packing of parser functions where it would pack as many operations into a function body, and when it maximally filled the function it would back up the stream and generate a new function body.

This technique fundamentally produced code that looked human optimized, was linked with anonymous symbols intern'd into a private symbol table, and compiled the resulting code for performance.

It was one of my most ambitious projects and exemplifies my ability to write innovative code.
