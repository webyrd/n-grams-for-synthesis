# Neurally-guided search for Program Synthesis with Minikanren

## Introduction

The contributions of this paper are to show that neural search
helps program synthesis even in richer recursive languages, as
well as the value of minikanren as a platform for describing
those kinds of programs.

## Background on Minikanren

### Generation of goals in Minikanren
### Interleaving Search
### Relational Intepreters

## Method

### Neural Search in Minikanren

## Experiments

### Compare Deepcoder to mk with Neural search
### Compare Deepcoder to mk for recursive programs
### Compare mk without vanilla search to mk with neural search

### Possible programs
Synthesize addition with Church numerials
Synthesize multiplication with Church numerials
Synthesize exponentiation with Church numerials
Synthesize predecessor function with Church numerials

### Use mk with Neural search to recover larger programs
#### Demonstrate mk doing differential synthesis: lex and dyn in icfp 2017 pearl, or CBV vs CBN
#### Train on both type inferencer and evaluator, and show that the combination avoids generate and test
#### Synthesize interesting parts of the Scheme interp, or microKanren

## Related Work on Neural Search and program synthesis

## Conclusions
