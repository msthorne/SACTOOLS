#!/bin/csh

#---------------------------------------------------------------------------------------#
# Script for doing some basic testing of routines.
# Plotting is done using the Generic Mapping Tools (GMT) package
#
# This script tests:
# rmserror
#
#---------------------------------------------------------------------------------------#


# Test rmserror
#---------------------------------------------------------------------------------------#
# calculate error between data file: 200308250628.TILT.ASUH.LR.sac 
# and synthetic seismogram: ulvz_15_5_w1.5_mod53_047.R
../bin/rmserror 200308250628.TILT.ASUH.LR.sac ulvz_15_5_w1.5_mod53_047.R
set rms = `awk 'NR == 1 {print $1}' rmserror.txt`

# plot overlay of data and synthetic and report result
../bin/sac2xy ulvz_15_5_w1.5_mod53_047.R synth.xy
../bin/sac2xy 200308250628.TILT.ASUH.LR.sac data.xy

psxy data.xy -JX6i/2i -R-20/40/-0.75/1.1 -W6/0/0/205 -P -K \
  -B10g10000f1:"Relative Time (s)":/1g10000nSew -Y2.0i >! Examples_04.ps
psxy synth.xy -JX -R -W6/205/0/0 -P -O -K >> Examples_04.ps

pstext -JX -R -N -O -K -P -G0/0/205 << eof >> Examples_04.ps
-20 1.4 12 0 1 1 Data: 200308250628.TILT.ASUH.LR.sac 
eof
pstext -JX -R -N -O -K -P -G205/0/0 << eof >> Examples_04.ps
-20 1.2 12 0 1 1 Synthetic: ulvz_15_5_w1.5_mod53_047.R
eof
pstext -JX -R -N -O -K -P -G0/0/0 << eof >> Examples_04.ps
-20 1.6 12 0 1 1 Program: rmserror
eof
pstext -JX -R -N -O -K -P -G0/0/0 << eof >> Examples_04.ps
18 0.9 12 0 1 1 RMS Misfit: $rms (%)
eof


rm rmserror.txt
rm synth.xy data.xy
rm .gmtcommands4
#---------------------------------------------------------------------------------------#

gs -sDEVICE=x11 Examples_04.ps

