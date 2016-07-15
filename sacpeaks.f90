PROGRAM peakfind
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
!  Find minima/maxima in a SAC file
!
!  michael.thorne@utah.edu
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
USE sac_i_o
IMPLICIT NONE
REAL, DIMENSION(:,:), ALLOCATABLE :: idata
REAL, DIMENSION(:), ALLOCATABLE :: sacdata
REAL, DIMENSION(2,1) :: peakpicks
REAL :: tt
INTEGER :: NN, ios, J
INTEGER :: sens                 !sensitivity
INTEGER :: minmax
CHARACTER(LEN=112) :: sacfile, input

!  READ USER INPUT
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
NN = IARGC()
IF (NN < 3) THEN
  write(*,'(a)') "usage: sacpeaks sacfile sensitivity minmax"
  write(*,'(a)') "       sacfile - input SAC file"
  write(*,'(a)') "       sensitivity - number of points (integer)"
  write(*,'(a)') "       minmax: 0=find both minima and maxima"
  write(*,'(a)') "               1=find only maxima"
  write(*,'(a)') "               2=find only minima"
  STOP
ENDIF

CALL GETARG(1,sacfile)

CALL GETARG(2,input)
READ(input,*) sens

CALL GETARG(3,input)
READ(input,*) minmax

write(*,*) "Finding peaks for file: '", TRIM(ADJUSTL(sacfile)), "'"
write(*,*) "Sensitivity set at: ", sens
IF (minmax == 0) THEN
  write(*,*) "Searching for all minima/maxima"
ELSEIF (minmax == 1) THEN
  write(*,*) "Searching only for maxima"
ELSEIF (minmax == 2) THEN
  write(*,*) "Searching only for minima"
ENDIF
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!

!    --  READ INPUT SAC FILE   --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!

!read sacfile
CALL rbsac(sacfile,delta,depmin,depmax,scale,odelta,b,e,o,a,internal1,      &
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
kinst,sacdata)

IF (nvhdr /= 6) THEN
  write(*,*) "ERROR - File: '", TRIM(adjustl(sacfile)), "' appears to be of non-native &
  &byte-order or is not a SAC file."
  STOP
ENDIF
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!



! POPULATE idata with time and amplitude from SAC file
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
ALLOCATE(idata(2,npts))
tt = b
DO J=0,(npts-1)
  idata(1,J+1) = tt
  idata(2,J+1) = sacdata(J+1)
  tt = b + delta*J
ENDDO
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!

! Loop through file finding candidate peaks
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
OPEN(UNIT=2,FILE='peaks.xy')
DO J=1,npts
  CALL findpeak(idata,npts,J,sens,peakpicks,minmax)
  IF (peakpicks(1,1) /= 0.0  .AND. peakpicks(2,1) /= 0.0) THEN
    write(2,*) peakpicks(1,1), peakpicks(2,1)
  ENDIF
ENDDO
CLOSE(2)



END PROGRAM peakfind

!----------------------------------------------------------------------------------------!
SUBROUTINE findpeak(xyvals,nr,pt,s,op,mmax)
IMPLICIT NONE
REAL, INTENT(IN), DIMENSION(2,NR) :: xyvals
REAL, INTENT(OUT), DIMENSION(2,1) :: op
INTEGER, INTENT(IN) :: pt, s, nr, mmax
INTEGER :: lwin, rwin
INTEGER :: maxsum, minsum
INTEGER :: J

IF ((pt-s) < 1) THEN
  lwin = 1
ELSE
  lwin = pt-s
ENDIF

IF ((pt+s) > nr) THEN
  rwin = nr
ELSE
  rwin = pt+s
ENDIF

maxsum = 0
minsum = 0
DO J=lwin,rwin
  IF (xyvals(2,J) >= xyvals(2,pt)) THEN
    maxsum = maxsum + 1
  ENDIF
 
  IF (xyvals(2,J) <= xyvals(2,pt)) THEN
    minsum = minsum + 1
  ENDIF
  
ENDDO

IF (mmax == 0) THEN

  IF (maxsum == 1 .OR. minsum == 1) THEN
    op(1,1) = xyvals(1,pt)
    op(2,1) = xyvals(2,pt)
  ELSE
    op(1,1) = 0.
    op(2,1) = 0.
  ENDIF

ELSEIF (mmax == 1) THEN

  IF (maxsum == 1) THEN
    op(1,1) = xyvals(1,pt)
    op(2,1) = xyvals(2,pt)
  ELSE
    op(1,1) = 0.
    op(2,1) = 0.
  ENDIF

ELSEIF (mmax == 2) THEN

  IF (minsum == 1) THEN
    op(1,1) = xyvals(1,pt)
    op(2,1) = xyvals(2,pt)
  ELSE
    op(1,1) = 0.
    op(2,1) = 0.
  ENDIF

ENDIF

END SUBROUTINE findpeak
!----------------------------------------------------------------------------------------!



