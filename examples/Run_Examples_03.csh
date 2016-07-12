#!/bin/csh


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
2300 1.1 12 0 1 1 Red: Smoothed with 'mavg'
eof


rm mavg_apollostat14_envelope.sac
rm original.xy
rm envelope.xy

gs -sDEVICE=x11 Examples_03.ps
#---------------------------------------------------------------------------------------#



rm .gmtcommands4

