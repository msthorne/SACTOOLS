PROGRAM movingaverage
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
! computes simple n-point moving average on a SAC file
!
! michael.thorne@utah.edu
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
USE sac_i_o
IMPLICIT NONE
REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: unfiltered, averaged
REAL(KIND=4) :: summ
INTEGER(KIND=4)    :: NN, ios
INTEGER(KIND=4) :: navg
INTEGER(KIND=4) :: start, endd
INTEGER(KIND=4) :: J, KK
CHARACTER(LEN=112) :: ifile, ofile
CHARACTER(LEN=4) :: input

!    --  READ USER INPUT  --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
NN = IARGC()
IF (NN < 2) THEN
  write(*,'(a)') "usage:  mavg ifile navg"
  write(*,'(a)') "        ifile: input SAC file"
  write(*,'(a)') "        navg: number of points to use in moving average."
  STOP
ENDIF

CALL GETARG(1, ifile)
  OPEN(UNIT=1,FILE=ifile,STATUS='OLD',IOSTAT=ios)
  IF (ios > 0) THEN
    write(*,*) "ERROR - Input file: '", TRIM(adjustl(ifile)), "' does not exist ..."
    CLOSE(1)
    STOP
  ENDIF
  CLOSE(1) 

CALL GETARG(2,input)
READ(input,*) navg

!If navg is given as an even number then make it odd
IF (mod(navg,2) == 0) THEN
  navg = navg + 1
ENDIF

write(*,*) "Smoothing File: ", TRIM(adjustl(ifile))
write(*,*) "navg = ", navg

!    -- READ INPUT SAC FILES  --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!

CALL rbsac(ifile,delta,depmin,depmax,scale,odelta,b,e,o,a,internal1,         &
t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,f,resp0,resp1,resp2,resp3,resp4,resp5,resp6,   &
resp7,resp8,resp9,stla,stlo,stel,stdp,evla,evlo,evel,evdp,mag,user0,user1,   &
user2,user3,user4,user5,user6,user7,user8,user9,dist,az,baz,gcarc,internal2, &
internal3,depmen,cmpaz,cmpinc,xminimum,xmaximum,yminimum,ymaximum,unused1,   &
unused2,unused3,unused4,unused5,unused6,unused7,nzyear,nzjday,nzhour,nzmin,  &
nzsec,nzmsec,nvhdr,norid,nevid,npts,internal4,nwfid,nxsize,nysize,unused8,   &
iftype,idep,iztype,unused9,iinst,istreg,ievreg,ievtyp,iqual,isynth,imagtyp,  &
imagsrc,unused10,unused11,unused12,unused13,unused14,unused15,unused16,      &
unused17,leven,lpspol,lovrok,lcalda,unused18,kevnm,kstnm,khole,ko,ka,kt0,kt1,&
kt2,kt3,kt4,kt5,kt6,kt7,kt8,kt9,kf,kuser0,kuser1,kuser2,kcmpnm,knetwk,kdatrd,&
kinst,unfiltered)

IF (nvhdr /= 6) THEN
  write(*,*) "ERROR - File: '", TRIM(adjustl(ifile)), "' appears to be of non-native &
  &byte-order or is not a SAC file."
  STOP
ENDIF

!    --  CALCULATE MOVING AVERAGE  --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
ALLOCATE(averaged(npts))

start = 1+((navg-1)/2)
endd = npts - (navg-1)/2

averaged = 0.0
DO J=start,endd
  summ = 0.0
  DO KK = -((navg-1)/2),((navg-1)/2)
    summ = summ + unfiltered(J+KK)
  ENDDO
  averaged(J) = summ/navg
ENDDO



!    --  WRITE OUT MOVING AVERAGED FILE --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!

!change pertinent header variables
ofile = 'mavg_'//ifile
kuser0 = 'mavg'

!write averaged file
CALL wbsac(ofile,delta,depmin,depmax,scale,odelta,b,e,o,a,internal1,      &
t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,f,resp0,resp1,resp2,resp3,resp4,resp5,resp6,   &
resp7,resp8,resp9,stla,stlo,stel,stdp,evla,evlo,evel,evdp,mag,user0,user1,   &
user2,user3,user4,user5,user6,user7,user8,user9,dist,az,baz,gcarc,internal2, &
internal3,depmen,cmpaz,cmpinc,xminimum,xmaximum,yminimum,ymaximum,unused1,   &
unused2,unused3,unused4,unused5,unused6,unused7,nzyear,nzjday,nzhour,nzmin,  &
nzsec,nzmsec,nvhdr,norid,nevid,npts,internal4,nwfid,nxsize,nysize,unused8,  &
iftype,idep,iztype,unused9,iinst,istreg,ievreg,ievtyp,iqual,isynth,imagtyp,  &
imagsrc,unused10,unused11,unused12,unused13,unused14,unused15,unused16,      &
unused17,leven,lpspol,lovrok,lcalda,unused18,kevnm,kstnm,khole,ko,ka,kt0,kt1,&
kt2,kt3,kt4,kt5,kt6,kt7,kt8,kt9,kf,kuser0,kuser1,kuser2,kcmpnm,knetwk,kdatrd,&
kinst,averaged)



END PROGRAM movingaverage
