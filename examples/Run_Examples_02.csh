#!/bin/csh

#---------------------------------------------------------------------------------------#
# Script for doing some basic testing of routines.
# Plotting is done using the Generic Mapping Tools (GMT) package
#
# This script tests:
# sachead, sac2xy, sac2xyfill, stacksac, stacksacall, stacksacallaz
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

# Test sac2xy  and sac2xyfill
#---------------------------------------------------------------------------------------#
../bin/sac2xy 201002090103.AZ.LVA2.BHT.sac A.xy
../bin/sac2xyfill 201002090103.AZ.LVA2.BHT.sac Afill.xy

psxy Afill.xy -JX6i/2i -R-40/120/-1/1 -P -W1/0/0/205 -K  \
  -B25g10000f5/1g1000nSeW >! Examples_02.ps
psxy A.xy -JX -R -O -K -W4/205/0/0 >> Examples_02.ps
rm A.xy Afill.xy
#---------------------------------------------------------------------------------------#

# Test stacksac
#---------------------------------------------------------------------------------------#
ls *.sac >! flist
../bin/stacksac flist
../bin/sac2xy stacked.sac stacked.xy
#psxy stacked.xy -JX -R -O -K -W6/0/0/0 -B25g10000f5/1g1000nSeW -Y2.5i >> sacxyplot.ps
psxy stacked.xy -JX -R -O -K -W6/0/0/0 >> Examples_02.ps
rm stacked.sac stacked.xy
#---------------------------------------------------------------------------------------#

# Test stacksacall
#---------------------------------------------------------------------------------------#
../bin/stacksacall flist 70. 72. 1.
../bin/sac2xy stacked_001.sac s1.xy
../bin/sac2xy stacked_002.sac s2.xy
psxy s1.xy -JX -R -O -K -W4/205/0/0 -Y2.5i -B25g10000f5/1g1000nSew >> Examples_02.ps
psxy s2.xy -JX -R -O -K -W4/0/0/205 >> Examples_02.ps
rm s1.xy s2.xy
rm stacked_001.sac stacked_002.sac
#---------------------------------------------------------------------------------------#

# Test stacksacallaz
#---------------------------------------------------------------------------------------#
../bin/stacksacallaz flist
#---------------------------------------------------------------------------------------#

rm flist


gs -sDEVICE=x11 Examples_02.ps

