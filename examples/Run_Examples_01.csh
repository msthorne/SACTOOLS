#!/bin/csh

# Simple check on amp2sac and xy2sac programs


# generate table of amplitude values
cat << efg >! test.y
0.
0.
0.
1.
1.
1.
1.
1.
0.5
0.25
0.1
0.
0.
0.
0.
0.
efg

# convert to a SAC file with a 1.0 s sample interval
../bin/amp2sac test.y test_01.sac 1.0


# generate a table of time, amplitude values
cat << efg >! test.xy
0.0 0.
1.0 0.
2.0 0.
3.0 1.
4.0 1.
5.0 1.
6.0 1.
7.0 1.
8.0 0.5
9.0 0.25
10.0 0.1
11.0 0.
12.0 0.
13.0 0.
14.0 0.
15.0 0.
efg

# convert to a SAC file
../bin/xy2sac test.xy test_02.sac

# open up files with SAC to check them
sac << efg
r test_01.sac test_02.sac
qdp off
ppk
q
efg

rm test.y test.xy
rm test_01.sac test_02.sac


