#!/bin/csh

#---------------------------------------------------------------------------------------#
# Script for doing some basic testing of routines.
# Plotting is done using the Generic Mapping Tools (GMT) package
#
# This script tests:
# mavg and stalta
#
#---------------------------------------------------------------------------------------#


# Test mavg
#---------------------------------------------------------------------------------------#
../bin/mavg apollostat14_envelope.sac 50

../bin/sac2xy apollostat14_envelope.sac original.xy
../bin/sac2xy mavg_apollostat14_envelope.sac envelope.xy

psxy original.xy -JX6i/2.5i -R500/3500/-.1/1.5 -P -K -W1/0/0/205 \
  -B500g10000f50:"Time (s)":/1g10000f2nSeW -Y1.5i >! Examples_03.ps
psxy envelope.xy -JX -R -P -O -K -W3/205/0/0 >> Examples_03.ps

pstext -JX -R -N -O -K -P -G0/0/205 << eof >> Examples_03.ps
2300 1.3 12 0 1 1 Blue: Original file
eof
pstext -JX -R -N -O -K -P -G205/0/0 << eof >> Examples_03.ps
2300 1.1 12 0 1 1 Red: 51 pt. moving avg
eof

pstext -JX -R -N -O -K -P -G0/0/0 << eof >> Examples_03.ps
500 1.6 14 0 1 1 Program: 'mavg.f90'
eof


rm mavg_apollostat14_envelope.sac
rm original.xy
rm envelope.xy
#---------------------------------------------------------------------------------------#

# Test stalta
#---------------------------------------------------------------------------------------#
../bin/stalta apollostat14_envelope.sac 5.0 200.0
../bin/sac2xy sta.sac sta.xy
../bin/sac2xy lta.sac lta.xy
../bin/sac2xy snr.sac snr.xy 

psxy sta.xy -JX6i/2i -R500/3500/-.1/1.1 -P -O -K -W3/0/0/205 \
  -B500g10000f50/1g10000f2nSeW -Y3.5i >> Examples_03.ps
psxy lta.xy -JX -R -P -O -K -W8/205/0/0 >> Examples_03.ps

pstext -JX -R -N -O -K -P -G0/0/205 << eof >> Examples_03.ps
2100 .95 12 0 1 1 Blue: Short-term average (sta.sac)
eof
pstext -JX -R -N -O -K -P -G205/0/0 << eof >> Examples_03.ps
2100 0.85 12 0 1 1 Red: Long-term average (lta.sac)
eof

psxy snr.xy -JX -R500/3500/-.1/9 -P -O -K -W3/0/0/0 -Y2.5i -B >> Examples_03.ps

pstext -JX -R -N -O -K -P -G0/0/0 << eof >> Examples_03.ps
2100 8. 12 0 1 1 Black: STA/LTA (snr.sac)
eof

pstext -JX -R -N -O -P -G0/0/0 << eof >> Examples_03.ps
500. 9.5 14 0 1 1 Program: 'stalta.f90'
eof


rm sta.sac lta.sac snr.sac
rm sta.xy lta.xy snr.xy
rm .gmtcommands4
#---------------------------------------------------------------------------------------#

gs -sDEVICE=x11 Examples_03.ps

