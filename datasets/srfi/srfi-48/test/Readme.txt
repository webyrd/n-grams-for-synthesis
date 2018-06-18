== srfi-48 format test ==


cd test

gosh test-Gauche.scm

sash test-Sagittarius.scm

racket test-Racket.scm

gsi test-Gambit.scm

guile --no-auto-compile test-Guile.scm

mit-scheme --quiet --load test-MIT-Scheme.scm
# On windows, it seems that MIT/GNU Scheme can't run as a command line tool.
# So, this test program outputs a result to a file 'test-MIT-Scheme-result.txt'.

scheme --quiet test-ChezScheme.scm

csi -quiet test-Chicken.scm


