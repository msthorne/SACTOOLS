#!/bin/csh

#---------------------------------------------------------------------------------------#
# Script for doing some basic testing of routines.
# Plotting is done using the Generic Mapping Tools (GMT) package
#
# This script tests:
# rmserror, sacsnr, sacmax
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
#---------------------------------------------------------------------------------------#


# Test sacsnr
#---------------------------------------------------------------------------------------#
../bin/sacsnr 201001271742.CO.CASEE.HHT.sac -n -180. -40. -s 0. 20.
set tmax = `awk 'NR == 1 {print $2}' snr.txt`
set amax = `awk 'NR == 1 {print $3}' snr.txt`
set namp = `awk 'NR == 1 {print $4}' snr.txt`
set SNR = `awk 'NR == 1 {print $5}' snr.txt`

../bin/sac2xy 201001271742.CO.CASEE.HHT.sac data.xy

psxy data.xy -JX6i/2i -R-200/100/-.5/1.2 -P -O -K -Y3.2i \
  -B20g10000f5nSew -W4/0/0/205 >> Examples_04.ps
psxy -JX -R -P -O -K -Sc.15i -W4/205/0/0 << efg >> Examples_04.ps
$tmax $amax
efg
psxy -JX -R -P -O -K -W5/205/0/0 << efg >> Examples_04.ps
-180. $namp
-40. $namp
efg

pstext -JX -R -N -O -K -P -G0/0/0 << eof >> Examples_04.ps
-200 1.7 12 0 1 1 Program: sacsnr
eof
pstext -JX -R -N -O -K -P -G0/0/205 << eof >> Examples_04.ps
-200 1.5 12 0 1 1 Data: 201001271742.CO.CASEE.HHT.sac
eof
pstext -JX -R -N -O -K -P -G205/0/0 << eof >> Examples_04.ps
-200 1.3 12 0 1 1 SNR: $SNR
eof


rm snr.txt data.xy
#---------------------------------------------------------------------------------------#

# Test sacmax
#---------------------------------------------------------------------------------------#
../bin/sacmax 200308250628.TILT.ASUH.LR.sac -10. 20. >! smax.txt
set Amax = `awk 'NR == 1 {print $2}' smax.txt`
set Tmax = `awk 'NR == 1 {print $4}' smax.txt`
set Amin = `awk 'NR == 1 {print $3}' smax.txt`
set Tmin = `awk 'NR == 1 {print $5}' smax.txt`

../bin/sac2xy 200308250628.TILT.ASUH.LR.sac data.xy
psxy data.xy -JX6i/2i -R-20/40/-.75/1.1 -P -O -K -Y3.2i \
  -B20g10000f5nSew -W4/0/0/205 >> Examples_04.ps

# plot circle on max within time window
psxy -JX -R -P -O -K -Sc.15i -W4/205/0/0 << eof >> Examples_04.ps
$Tmax $Amax
eof

# plot circle on min within time window
psxy -JX -R -P -O -K -Sc.15i -W4/205/0/0 << eof >> Examples_04.ps
$Tmin $Amin
eof

pstext -JX -R -N -O -K -P -G0/0/205 << eof >> Examples_04.ps
-20 1.2 12 0 1 1 Data: 200308250628.TILT.ASUH.LR.sac
eof
pstext -JX -R -N -O -P -G0/0/0 << eof >> Examples_04.ps
-20 1.4 12 0 1 1 Program: sacmax
eof

rm smax.txt data.xy
rm .gmtcommands4
#---------------------------------------------------------------------------------------#

gs -sDEVICE=x11 Examples_04.ps

