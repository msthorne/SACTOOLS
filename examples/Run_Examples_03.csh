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
#---------------------------------------------------------------------------------------#

# Test stalta
#---------------------------------------------------------------------------------------#
../bin/stalta apollostat14_envelope.sac 5.0 100.0
../bin/sac2xy sta.sac sta.xy
../bin/sac2xy lta.sac lta.xy
../bin/sac2xy snr.sac snr.xy 

psxy sta.xy -JX6i/2i -R500/3500/-.1/1.1 -P -O -K -W2/0/0/205 \
  -B500g10000f50/1g10000f2nSeW -Y3i >> Examples_03.ps
psxy lta.xy -JX -R -P -O -K -W8/0/0/0 >> Examples_03.ps
psxy snr.xy -JX -R500/3500/-.1/9 -P -O -K -W6/205/0/0 -Y2.5i -B >> Examples_03.ps


rm sta.sac lta.sac snr.sac
rm sta.xy lta.xy snr.xy
#---------------------------------------------------------------------------------------#



rm .gmtcommands4
gs -sDEVICE=x11 Examples_03.ps

