PROGRAM xysac
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
! Convert x,y table to SAC file
!
! michael.thorne@utah.edu
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
USE sac_i_o
IMPLICIT NONE
REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: amps, times, dummy
REAL(KIND=4) :: deltaa
INTEGER(KIND=4)    :: NN, ios, J, NR
INTEGER(KIND=4), PARAMETER :: maxrecs = 10000000
CHARACTER(LEN=112) :: xytable, osacfile, ofile
CHARACTER(LEN=1) :: junk


!    --  READ USER INPUT  --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
NN = IARGC()
IF (NN < 2) THEN
  write(*,'(a)') "usage:  xy2sac xytable sacfile"
  write(*,'(a)') "        xytable - input xy file"
  write(*,'(a)') "        sacfile - name of output sac file"
  STOP
ENDIF

CALL GETARG(1, xytable)
OPEN(UNIT=1,FILE=xytable,STATUS='OLD',IOSTAT=ios)
IF (ios > 0) THEN
  write(*,*) "ERROR - Input file: '", TRIM(adjustl(xytable)), "' does not exist ..."
  CLOSE(1)
  STOP
ENDIF
CLOSE(1) 

CALL GETARG(2,osacfile)

write(*,*) "Writing XY file: '", TRIM(adjustl(xytable)), "' to SAC file: '", &
           & TRIM(adjustl(osacfile)), "' ..."


!    --  READ INPUT X-Y TABLE  --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!

!Determine total number of lines in file
NR = 0
OPEN(UNIT=1,FILE=xytable)
DO J=1,maxrecs
  READ(1,*,IOSTAT=ios) junk
  IF (ios /= 0) EXIT
  IF (J == maxrecs) THEN
    write(*,*) "Error:  Maximum number of records exceeded..."
    write(*,*) "  Exiting now!"
    STOP
  ENDIF
  NR = NR + 1
ENDDO
REWIND(1)

!Allocate data
ALLOCATE(times(NR))
ALLOCATE(amps(NR))

!Now read in data
DO J=1,NR
  READ(1,*) times(J), amps(J)
ENDDO
CLOSE(1)


!    --  WRITE OUTPUT SAC FILE  --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!

!Initialize sac file
ofile = 'temp'
CALL initsac(ofile,delta,depmin,depmax,scale,odelta,b,e,o,a,internal1,       &
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
kinst,dummy)

!find delta by simply difference the first two time steps
deltaa = times(2) - times(1)
depmin = MINVAL(amps)
depmax = MAXVAL(amps)
b = times(1)
npts = NR
kuser0 = 'xy2sac'

!now write out sac file
CALL wbsac(osacfile,deltaa,depmin,depmax,scale,odelta,b,e,o,a,internal1,     &
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
kinst,amps)




END PROGRAM xysac
