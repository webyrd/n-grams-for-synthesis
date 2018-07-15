# n-grams-for-synthesis, attempt #3 (at least!)

n-gram probabilities based on Scheme/Racket code, for directing search of the relational Scheme interpreter, and a prototype example relational interpreter with a search informed by these probabilities.


Scientific questions we are trying to answer with this code:

* How well do the various proposed optimizations for synthesis in miniKanren, including n-grams guided search and Greg's Barliman optimizations, compose?

* Do composing these proposed optimizations lead to significant speedups on a variety of interesting synthesis tasks?

* Which classes of synthesis tasks seem to benefit?

* Does composing effective optimization techniques ever slow down the resulting code?


Possible additional optimizations to try:

* From Barliman:

** Environment-splitting

** Turning on 'allow incomplete search' flag

* Other possible optimizations:

** Add type system

** Try to avoid redundant computation by generating only normal forms, taking advantages of symmetric operations, avoiding rebuilding named datastructures that were just torn apart, etc.

** Deep-learning or reinforcement-learning guided search

** Sketch-style synthesis & grammar-based synthesis

** Bottom-up approach to combining syntax

** Tabling/caching

** Oracle-based/algorithmic

** Recursion schemas, like in Leon (also, try to learn recursion schemas) [can we learn this, if we contiue looking at only one branch at a time?]

** Add standard helpers (fold, for example) to initial env

** Monte-carlo tree search to avoid explosion with more forms/larger number of helpers

** Modules/breaking down big synthesis problem into little synthesis problem

** Tie together decisions about base cases and recursive cases



OLD README TEXT

The ideas explored in this repo are inspired by conversations and hacking sessions that included: Rob Zinkov, Michael Ballantyne, Will Byrd, Greg Rosenblatt, Evan Donahue, Ramana Kumar, Nehal Patel, attendees of the Advanced miniKanren Hangout series, and members of the broader miniKanren community.  Early hacking to extract poatterns from Clojure code was done by Micahel Ballantyne and Will Byrd, with critical guidance from Rob Zinkov (see https://github.com/webyrd/rnn-clojure).  Will hacked up a crude, somewhat broken version of the system in the `old_code/original_version` directory.  This was improved by "mob programming" during miniKanren advanced hangouts #6 and #7 (see https://github.com/webyrd/miniKanren-hangout-summaries and https://github.com/webyrd/miniKanren-hangout-summaries/tree/master/code/advanced-hangouts)--the final improved code can be found in `old_code/advanced_hangout_7_version`.  The code in `old_code/advanced_hangout_7_version/interp-fastest-fixed-application-optimization.scm` benefits from Greg's optimization for variable-argument application, inspired by his work on Barliman (https://github.com/webyrd/Barliman).  This version also includes some nice abstractions to avoid duplicate code.  The interpreter (and several previous versions) does include a "cheat" in the application cases: "Cheat a little by requring the rator expression to be a variable reference. Note that this prevents us from synthesizing code like `(((lambda (x) x) (lambda (x) x)) '(1 2 3))`."

This latest attempt at n-gram-based guided search was inspired by Rob Zinkov hosting Will Byrd at University of Oxford in January of 02018.  Rob has been hacking on getting a larger corpus of trainign programs, while Will has been working on how to better integrate the resulting probabilities into the relational interpreters.

Many thanks to everyone who has helped improve the ideas and code!

TODO

Benchmarks for fairly comparing Barliman interpreter with optimizations, the vanilla full interpreter, and full interpreters with guided search.   Complications: beware of `boolean-conditions-only?` and `allow-incomplete-search?` flags in the Barliman interpreter, the `enable-conde1?` flag in `mk/mk.scm` of the Barliman mk implementation, and the application "cheat" in the `old_code/advanced_hangout_7_version/interp-fastest-fixed-application-optimization.scm` interpreter. Also, the full Barliman interpreter includes other optimizations not included in the `old_code/advanced_hangout_7_version/interp-fastest-fixed-application-optimization.scm` interpreter.  And the `mk.scm` file used by Barliman includes `state-deferred-defer*` and other code that the `mk.scm` in `old_code/advanced_hangout_7_version/` doesn't have.  Need to make sure we are doing a fair comparison!