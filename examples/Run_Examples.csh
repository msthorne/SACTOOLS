#!/bin/csh



# Test sachead
#---------------------------------------------------------------------------------------#
echo "testing sachead..."
../bin/sachead 201002090103.AZ.LVA2.BHT.sac GCARC
../bin/sachead 201002090103.AZ.LVA2.BHT.sac GCARC | awk '{print $2}'
set evdp = `../bin/sachead 201002090103.AZ.LVA2.BHT.sac EVDP | awk '{print $2}'`
echo $evdp
# Should be: GCARC=72.54 deg and EVDP = 10 km
#---------------------------------------------------------------------------------------#

# Test sac2xy and sac2xyfill
#---------------------------------------------------------------------------------------#
../bin/sac2xy 201002090103.AZ.LVA2.BHT.sac A.xy
../bin/sac2xyfill 201002090103.AZ.LVA2.BHT.sac Afill.xy

psxy Afill.xy -JX6i/2i -R-40/120/-1/1 -P -W1/0/0/205 -K  \
  -B25g10000f5/1g1000nSeW >! sacxyplot.ps
psxy A.xy -JX -R -O -K -W4/205/0/0 >> sacxyplot.ps
rm A.xy Afill.xy
#---------------------------------------------------------------------------------------#

# Test stacksac
#---------------------------------------------------------------------------------------#
ls *.sac >! flist
../bin/stacksac flist
../bin/sac2xy stacked.sac stacked.xy
#psxy stacked.xy -JX -R -O -K -W6/0/0/0 -B25g10000f5/1g1000nSeW -Y2.5i >> sacxyplot.ps
psxy stacked.xy -JX -R -O -K -W6/0/0/0 >> sacxyplot.ps
rm flist
#---------------------------------------------------------------------------------------#


gs -sDEVICE=x11 sacxyplot.ps

