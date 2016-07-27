#!/bin/csh

#---------------------------------------------------------------------------------------#
# Script for doing some basic testing of routines.
# Plotting is done using the Generic Mapping Tools (GMT) package
#
# This script tests:
# addnoise
#
#---------------------------------------------------------------------------------------#


# Test addnoise
#---------------------------------------------------------------------------------------#
set RMS = 0.2
set corner = 0.1


ls DDP*.T >! flist
../bin/addnoise flist $RMS $corner

# first lets look at an example of the noise generated
../bin/sac2xy noise.sac noise.xy

psxy noise.xy -JX6i/1.75i -Y1.25i -R1160/1260/-0.5/1.1 -P -K -W4/0/0/205 \
  -B20g10000f5:"Time (s)":/1g10000f.1nSeW >! Examples_06.ps
pstext -JX -R -N -O -K -G0/0/205 << eof >> Examples_06.ps
1160 1.2 12 0 1 1 'noise.sac'
eof
pstext -JX -R -N -O -K -G205/0/0 << eof >> Examples_06.ps
1162 0.9 11 0 1 1 RMS: $RMS
1162 0.75 11 0 1 1 Corner: $corner (s)
eof

# Now plot seismogram and seismogram with noise added
../bin/sac2xy DDP_h300_s2.0_210.T A.xy
../bin/sac2xy noise_DDP_h300_s2.0_210.T B.xy

psxy A.xy -JX -Y2.25i -R1160/1260/-0.5/1.1 -P -K -O -W4/0/0/205 -B20g10000f5/1g10000f.1nseW  >> Examples_06.ps
pstext -JX -R -N -O -K -G0/0/205 << eof >> Examples_06.ps
1160 1.2 12 0 1 1 Synthetic: DDP_h300_s2.0_210.T
eof


psxy B.xy -JX -Y2.25i -R1160/1260/-0.5/1.1 -P -K -O -W4/0/0/205 -B >> Examples_06.ps
pstext -JX -R -N -O -K -G0/0/205 << eof >> Examples_06.ps
1160 1.2 12 0 1 1 Synthetic + Noise
eof


# FFT on noise
sac << efg
r noise.sac
fft 
writesp 
q
efg

# plot amplitude spectrum as a function of period
../bin/sac2xy noise.sac.am A.xy
awk 'NR > 1 {print (1.0/$1), $2}' A.xy >! B.xy
psxy B.xy -JX6il/2il -Y2.75i -R0.01/1000/0.01/100 -W1/0/0/205 -P -O -K \
  -B10g10000:"Period (s)":/10g10000nSeW >> Examples_06.ps

# add true corner (2*pi*$corner)
set cc = `awk 'NR == 1 {print (2*3.141592654*'$corner')}' A.xy`
psxy -JX -R -W2/205/0/0 -P -O -K << eof >> Examples_06.ps
$cc .01
$cc 10
eof

pstext -JX -R -N -O -G0/0/205 << eof >> Examples_06.ps
0.01 102 12 0 1 1 Amplitude Spectrum of noise
eof


rm flist
rm noise.sac noise.xy
rm noise.sac.am noise.sac.ph
rm A.xy B.xy
rm auto_gauss.xy
rm noise_DDP_h300_s2.0_210.T
rm .gmtcommands4

gs -sDEVICE=x11 Examples_06.ps
