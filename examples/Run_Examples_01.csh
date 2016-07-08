#!/bin/csh

# Simple check on amp2sac and xy2sac programs


#  Check 'amp2sac'
#------------------------------------------------------------------------#
# generate table of amplitude values
cat << efg >! test.y
0.
0.
1.
1.
1.
1.
-1.
-1.
-1.
-1.
1.
1.
1.
-1.
-1.
-1
1.
1.
-1.
-1.
0.
0.
efg

# convert to a SAC file with a 1.0 s sample interval
../bin/amp2sac test.y test_01.sac 1.0
#------------------------------------------------------------------------#


# Check 'xy2sac'
#------------------------------------------------------------------------#
# generate a table of time, amplitude values
cat << efg >! test.xy
0. 0.
1. 0.
2. 1.
3. 1.
4. 1.
5. 1.
6. -1.
7. -1.
8. -1.
9. -1.
10. 1.
11. 1.
12. 1.
13. -1.
14. -1.
15. -1
16. 1.
17. 1.
18. -1.
19. -1.
20. 0.
21. 0.
efg

# convert to a SAC file
../bin/xy2sac test.xy test_02.sac
#------------------------------------------------------------------------#

# check files in SAC
#------------------------------------------------------------------------#
sac << efg
r test_01.sac test_02.sac
qdp off
ppk
q
efg

rm test.y test.xy
rm test_01.sac test_02.sac
#------------------------------------------------------------------------#



