PROGRAM getsacmax
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
! Get max amplitude from a SAC file within a specified time range.
!
! michael.thorne@utah.edu
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
USE sac_i_o
IMPLICIT NONE
REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: idata
REAL(KIND=4) :: time1, time2, Amax, Amin, Tmax, Tmin
REAL(KIND=4) :: delta1, b1, e1, tt
INTEGER(KIND=4) :: NN, J
INTEGER(KIND=4) :: npts1
CHARACTER(LEN=16) :: input
CHARACTER(LEN=100) :: sacfile

!    --  READ USER INPUT  --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
NN = IARGC()
IF (NN < 3) THEN
  write(*,'(a)') "usage:  sacmax sacfile time1 time2"
  write(*,'(a)') "        time1: begin time of search"
  write(*,'(a)') "        time2: end time of search"
  STOP
ENDIF

CALL GETARG(1,sacfile)
CALL GETARG(2,input)
read(input,*) time1
CALL GETARG(3,input)
read(input,*) time2

!    --  READ INPUT SAC FILE   --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!

!read sacfile
CALL rbsac(sacfile,delta1,depmin,depmax,scale,odelta,b1,e1,o,a,internal1,      &
t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,f,resp0,resp1,resp2,resp3,resp4,resp5,resp6,   &
resp7,resp8,resp9,stla,stlo,stel,stdp,evla,evlo,evel,evdp,mag,user0,user1,   &
user2,user3,user4,user5,user6,user7,user8,user9,dist,az,baz,gcarc,internal2, &
internal3,depmen,cmpaz,cmpinc,xminimum,xmaximum,yminimum,ymaximum,unused1,   &
unused2,unused3,unused4,unused5,unused6,unused7,nzyear,nzjday,nzhour,nzmin,  &
nzsec,nzmsec,nvhdr,norid,nevid,npts1,internal4,nwfid,nxsize,nysize,unused8,  &
iftype,idep,iztype,unused9,iinst,istreg,ievreg,ievtyp,iqual,isynth,imagtyp,  &
imagsrc,unused10,unused11,unused12,unused13,unused14,unused15,unused16,      &
unused17,leven,lpspol,lovrok,lcalda,unused18,kevnm,kstnm,khole,ko,ka,kt0,kt1,&
kt2,kt3,kt4,kt5,kt6,kt7,kt8,kt9,kf,kuser0,kuser1,kuser2,kcmpnm,knetwk,kdatrd,&
kinst,idata)

IF (nvhdr /= 6) THEN
  write(*,*) "ERROR - File: '", TRIM(adjustl(sacfile)), "' appears to be of non-native &
  &byte-order or is not a SAC file."
  STOP
ENDIF

!    --  FIND MAX/MIN BETWEEN T1 & T2  --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
tt = b1
Amax = MINVAL(idata(:))
Amin = MAXVAL(idata(:))
Tmax = 0.0
Tmin = 0.0
DO J=0,(npts1-1)
  IF (tt >= time1 .AND. tt <= time2) THEN

    IF (idata(J) > Amax) THEN
      Amax = idata(J)
      Tmax = tt
    ENDIF

    IF (idata(J) < Amin) THEN
      Amin = idata(J)
      Tmin = tt
    ENDIF

  ENDIF

  tt = b1 + delta1*J
ENDDO

!    -- WRITE OUTPUT TO STDOUT
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
write(*,*) "Amax,Amin,Tmax,Tmin=", Amax, Amin, Tmax, Tmin


END PROGRAM getsacmax
