This directory contains a kernel benchmark that can help you
to decide which of the three kernels you want to use.  Don't
forget to take space efficiency into account as well as the
timings from this kernel benchmark.

To run the kernel benchmark from this directory:

larceny --path .. --r7rs --program kernel-benchmark.sps
sagittarius -L .. -r7 kernel-benchmark.sps
chibi-scheme -I .. kernel-benchmark.sps

To run in Gauche, foment, or Kawa:

mkdir /tmp/Gauche
cp *.txt kernel-benchmark.sps /tmp/Gauche
cp ../srfi/135/kernel8.sld ../srfi/135.sld /tmp/Gauche
cp ../srfi/135/kernel8.body.scm ../srfi/135.body.scm /tmp/Gauche
pushd /tmp/Gauche
cat kernel8.sld 135.sld kernel-benchmark.sps > temp.scm
gosh -r7 temp.scm
foment temp.scm
kawa temp.scm
popd

================================================================

Some general advice concerning the kernels:

kernel16 uses a representation based on UTF-16, and is likely to
         be a good choice when non-ASCII characters are common.

kernel8  uses a representation based on UTF-8, and is likely to
         use less space than kernel8 when ASCII characters are
         more common than non-ASCII characters.

kernel0  uses a representation based on Scheme's mutable strings,
         and is likely to be a good choice for interpreted systems
         whose strings are represented in UTF-8 or UTF-16, mainly
         because the overhead of interpretation means the built-in
         string-ref is likely to run faster on short strings than
         any UTF-8 or UTF-16 scanner that could be written in
         portable Scheme.

To change the kernel used by the sample implementations, edit
srfi/135.sld to import the kernel you want.

================================================================
