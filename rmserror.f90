PROGRAM rootmeansquare
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
! compute RMS misfit between data and synthetic traces
!
! michael.thorne@utah.edu
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
USE sac_i_o
IMPLICIT NONE
REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: yarray1, yarray2
REAL(KIND=4)       :: delta1, delta2, b1, b2, e1, e2
REAL(KIND=4) :: denom, numer, rms
INTEGER(KIND=4)    :: NN, ios, npts1, npts2, J, tpts
CHARACTER(LEN=112) :: file1, file2

!    --  READ USER INPUT  --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
NN = IARGC()
IF (NN < 2) THEN
  write(*,'(a)') "usage:  rmserror file1 file2"
  write(*,'(a)') "        file1 = observed data"
  write(*,'(a)') "        file2 = reference (synthetic) data"
  STOP
ENDIF

CALL GETARG(1,file1)
OPEN(UNIT=1,FILE=file1,STATUS='OLD',IOSTAT=ios)
IF (ios > 0) THEN
  write(*,*) "ERROR - Input file: '", TRIM(adjustl(file1)), "' does not exist ..."
  CLOSE(1)
  STOP
ENDIF
CLOSE(1) 
CALL GETARG(2,file2)
OPEN(UNIT=1,FILE=file2,STATUS='OLD',IOSTAT=ios)
IF (ios > 0) THEN
  write(*,*) "ERROR - Input file: '", TRIM(adjustl(file2)), "' does not exist ..."
  CLOSE(1)
  STOP
ENDIF
CLOSE(1)

write(*,*) "Calculating Root Mean Square error between files: '", &
& TRIM(adjustl(file1)), "' and '", TRIM(adjustl(file2)), "' ..."


!    --  READ INPUT SAC FILES  --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!

!read file1
CALL rbsac(file1,delta1,depmin,depmax,scale,odelta,b1,e1,o,a,internal1,      &
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
kinst,yarray1)

IF (nvhdr /= 6) THEN
  write(*,*) "ERROR - File: '", TRIM(adjustl(file1)), "' appears to be of non-native &
  &byte-order or is not a SAC file."
  STOP
ENDIF

!read file2
CALL rbsac(file2,delta2,depmin,depmax,scale,odelta,b2,e2,o,a,internal1,      &
t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,f,resp0,resp1,resp2,resp3,resp4,resp5,resp6,   &
resp7,resp8,resp9,stla,stlo,stel,stdp,evla,evlo,evel,evdp,mag,user0,user1,   &
user2,user3,user4,user5,user6,user7,user8,user9,dist,az,baz,gcarc,internal2, &
internal3,depmen,cmpaz,cmpinc,xminimum,xmaximum,yminimum,ymaximum,unused1,   &
unused2,unused3,unused4,unused5,unused6,unused7,nzyear,nzjday,nzhour,nzmin,  &
nzsec,nzmsec,nvhdr,norid,nevid,npts2,internal4,nwfid,nxsize,nysize,unused8,  &
iftype,idep,iztype,unused9,iinst,istreg,ievreg,ievtyp,iqual,isynth,imagtyp,  &
imagsrc,unused10,unused11,unused12,unused13,unused14,unused15,unused16,      &
unused17,leven,lpspol,lovrok,lcalda,unused18,kevnm,kstnm,khole,ko,ka,kt0,kt1,&
kt2,kt3,kt4,kt5,kt6,kt7,kt8,kt9,kf,kuser0,kuser1,kuser2,kcmpnm,knetwk,kdatrd,&
kinst,yarray2)

IF (nvhdr /= 6) THEN
  write(*,*) "ERROR - File: '", TRIM(adjustl(file2)), "' appears to be of non-native &
  &byte-order or is not a SAC file."
  STOP
ENDIF

!    --  TEST SAMPLE RATES  --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
IF (delta1 /= delta2) THEN
  write(*,*) "ERROR - Input files contain unequal sample rates ..."
  STOP
ENDIF

!    --  TEST VECTOR LENGTHS  --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
! Set number of points in computation to smaller of the two file sizes
!  in the case they are not equal

IF (npts1 < npts2) THEN
tpts = npts1
ELSE
tpts = npts2
ENDIF

!    --  COMPUTE RMS DIFFERENCE BETWEEN FILES --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
denom = 0.0_4
numer = 0.0_4
DO J=1,tpts
  numer = numer + (ABS(yarray1(J)-yarray2(J)))**2
  denom = denom + (ABS(yarray2(J)))**2 
ENDDO
rms = sqrt(numer/denom)

!    --  WRITE OUTPUT --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
! write output to stdout
write(*,*) "Root Mean Square Error: ", 100.0*rms, " (%)"

! write output (in percent) to file
OPEN(UNIT=1,FILE='rmserror.txt')
write(1,*) 100.0*rms
CLOSE(1)


END PROGRAM rootmeansquare
