#!/bin/csh

#---------------------------------------------------------------------------------------#
# Script for doing some basic testing of routines.
# Plotting is done using the Generic Mapping Tools (GMT) package
#
# This script tests:
# sachead, sac2xy, sac2xyfill, stacksac, stacksacgc, stacksacaz, sacunused
#
#---------------------------------------------------------------------------------------#


# Test sachead
#---------------------------------------------------------------------------------------#
echo "Check GCARC header and send to stdout (should be = 72.54 deg)"
../bin/sachead 201002090103.AZ.LVA2.BHT.sac GCARC

echo "Check GCARC header, but only send to stdout the value of GCARC"
../bin/sachead 201002090103.AZ.LVA2.BHT.sac GCARC | awk '{print $2}'

echo "Set C-shell variable for KSTNM which can be used in later plotting"
set kstnm = `../bin/sachead 201002090103.AZ.LVA2.BHT.sac KSTNM | awk '{print $2}'`
echo $kstnm
#---------------------------------------------------------------------------------------#

# Test sacunused
#---------------------------------------------------------------------------------------#
echo "check sacunused.  write "45.5" to variable unused2 and read it back out"
../bin/sacunused 201002090103.AZ.LVA2.BHT.sac 2 45.5
../bin/sachead 201002090103.AZ.LVA2.BHT.sac UNUSED2
#---------------------------------------------------------------------------------------#

# Test sac2xy  and sac2xyfill
#---------------------------------------------------------------------------------------#
../bin/sac2xy 201002090103.AZ.LVA2.BHT.sac A.xy
../bin/sac2xyfill 201002090103.AZ.LVA2.BHT.sac Afill.xy

psxy Afill.xy -JX6i/2i -R-40/120/-1/1 -P -W1/20/140/180 -K  \
  -B25g10000f5:"Relative Time (s)":/1g1000nSeW -Y2.0i >! Examples_02.ps
psxy A.xy -JX -R -O -K -W6/0/0/0 >> Examples_02.ps

pstext -JX -R -N -O -K -P -G0/0/0 << eof >> Examples_02.ps
-40 1.1 12 0 1 1 Test sac2xy and sac2xyfill
-38. 0.8 12 0 1 1 KSTNM: $kstnm
eof
rm A.xy Afill.xy
#---------------------------------------------------------------------------------------#

# Test stacksac
#---------------------------------------------------------------------------------------#
ls 201002090103*.sac >! flist
../bin/stacksac flist
../bin/sac2xy stacked.sac stacked.xy
psxy stacked.xy -JX -R -O -K -W6/0/0/0 -B25g10000f5/1g1000nSeW -Y2.75i >> Examples_02.ps
rm stacked.sac stacked.xy
#---------------------------------------------------------------------------------------#

# Test stacksacgc
#---------------------------------------------------------------------------------------#
../bin/stacksacgc flist 70. 72. 1.
../bin/sac2xy stacked_001.sac s1.xy
../bin/sac2xy stacked_002.sac s2.xy

psxy s1.xy -JX -R -O -K -W4/205/0/0  >> Examples_02.ps
psxy s2.xy -JX -R -O -K -W4/0/0/205 >> Examples_02.ps

pstext -JX -R -N -O -K -P -G0/0/0 << eof >> Examples_02.ps
-40 1.5 12 0 1 1 Black:  All files stacked (stacksac)
eof
pstext -JX -R -N -O -K -P -G205/0/0 << eof >> Examples_02.ps
-40 1.3 12 0 1 1 Red: Distance range 70-71 deg (stacksacgc)
eof
pstext -JX -R -N -O -K -P -G0/0/205 << eof >> Examples_02.ps
-40 1.1 12 0 1 1 Blue: Distance range 71-72 deg (stacksacgc)
eof

rm s1.xy s2.xy
rm stacked_001.sac stacked_002.sac
#---------------------------------------------------------------------------------------#

# Test stacksacaz
#---------------------------------------------------------------------------------------#
../bin/stacksacaz flist 40. 48. 4.
../bin/sac2xy stacked_001.sac s1.xy
../bin/sac2xy stacked_002.sac s2.xy

psxy s1.xy -JX -R -O -K -W4/205/0/0 -B25g10000f5/1g1000nSeW -Y3.1i >> Examples_02.ps
psxy s2.xy -JX -R -O -K -W4/0/0/205 >> Examples_02.ps

pstext -JX -R -N -O -K -P -G205/0/0 << eof >> Examples_02.ps
-40 1.3 12 0 1 1 Red: Azimuth range 40-44 deg (stacksacaz)
eof
pstext -JX -R -N -O -P -G0/0/205 << eof >> Examples_02.ps
-40 1.1 12 0 1 1 Blue: Azimuth range 44-48 deg (stacksacaz)
eof

rm s1.xy s2.xy
rm stacked_001.sac stacked_002.sac

#---------------------------------------------------------------------------------------#

rm flist
rm .gmtcommands4


gs -sDEVICE=x11 Examples_02.ps

