#!/bin/csh

#---------------------------------------------------------------------------------------#
# Script for doing some basic testing of routines.
# Plotting is done using the Generic Mapping Tools (GMT) package
#
# This script tests:
# sacpeaks
#
#---------------------------------------------------------------------------------------#


# Test sacpeaks
#---------------------------------------------------------------------------------------#
# Test with different sensitivities, e.g., 5 or 20
../bin/sacpeaks 200308250628.TILT.ASUH.LR.sac 5 1
mv peaks.xy max.xy
../bin/sacpeaks 200308250628.TILT.ASUH.LR.sac 5 2
mv peaks.xy min.xy

../bin/sac2xy 200308250628.TILT.ASUH.LR.sac data.xy

psxy data.xy -JX6i/2i -R-20/40/-.75/1.1 -P -K -Y1.5i \
  -B20g10000f5:"Relative Time (s)":nSew -W4/0/0/205 >! Examples_05.ps
psxy max.xy -JX -R -P -O -K -Sc.15i -W5/205/0/0 >> Examples_05.ps
psxy min.xy -JX -R -P -O -K -Sc.15i -W5/0/205/0 >> Examples_05.ps

pstext -JX -R -N -O -K -P -G0/0/205 << eof >> Examples_05.ps
-20 1.2 12 0 1 1 Data: 200308250628.TILT.ASUH.LR.sac
eof
pstext -JX -R -N -O -P -G0/0/0 << eof >> Examples_05.ps
-20 1.4 12 0 1 1 Program: sacpeaks
eof

rm data.xy max.xy min.xy
rm .gmtcommands4
#---------------------------------------------------------------------------------------#

gs -sDEVICE=x11 Examples_05.ps

