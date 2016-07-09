PROGRAM sactoxy
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
! Convert SAC file to XY table
!
! michael.thorne@utah.edu
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
USE sac_i_o
IMPLICIT NONE
REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: sacfile
REAL(KIND=4)       :: delta1, b1, e1, tt
INTEGER(KIND=4)    :: NN, ios, npts1, J
CHARACTER(LEN=112) :: file1, ofile


!    --  READ USER INPUT  --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
NN = IARGC()
IF (NN < 2) THEN
  write(*,'(a)') "usage:  sac2xy sacfile ofile"
  write(*,'(a)') "        sacfile - input sac file"
  write(*,'(a)') "        ofile - output xy file"
  STOP
ENDIF

CALL GETARG(1, file1)
OPEN(UNIT=1,FILE=file1,STATUS='OLD',IOSTAT=ios)
IF (ios > 0) THEN
  write(*,*) "ERROR - Input file: '", TRIM(adjustl(file1)), "' does not exist ..."
  CLOSE(1)
  STOP
ENDIF
CLOSE(1) 

CALL GETARG(2,ofile)

write(*,*) "Writing SAC file: '", TRIM(adjustl(file1)), "' to XY file: '", &
           & TRIM(adjustl(ofile)), "' ..."


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
kinst,sacfile)

IF (nvhdr /= 6) THEN
  write(*,*) "ERROR - File: '", TRIM(adjustl(file1)), "' appears to be of non-native &
  &byte-order or is not a SAC file."
  STOP
ENDIF

!    --  WRITE OUTPUT SACFILE  --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!

OPEN(UNIT=2,FILE=ofile)
DO J=0,(npts1-1)
  tt = b1 + delta1*J
  write(2,*) tt, sacfile(J+1)
ENDDO
CLOSE(2)


END PROGRAM sactoxy
