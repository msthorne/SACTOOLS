PROGRAM ampsac
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
! Convert single column of amplitude values to SAC file
!
! michael.thorne@utah.edu
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
USE sac_i_o
IMPLICIT NONE
REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: amps, dummy
REAL(KIND=4) :: deltaa
INTEGER(KIND=4)    :: NN, ios, J, NR
INTEGER(KIND=4), PARAMETER :: maxrecs = 10000000
CHARACTER(LEN=112) :: ampvals, osacfile, ofile
CHARACTER(LEN=12) :: deltastr
CHARACTER(LEN=1) :: junk


!    --  READ USER INPUT  --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
NN = IARGC()
IF (NN < 2) THEN
  write(*,'(a)') "usage:  amp2sac ampvals sacfile delta"
  write(*,'(a)') "        ampvals - input file containing amplitude values"
  write(*,'(a)') "        sacfile - name of output sac file"
  write(*,'(a)') "        delta   - sample interval (sec)"
  STOP
ENDIF

CALL GETARG(1, ampvals)
OPEN(UNIT=1,FILE=ampvals,STATUS='OLD',IOSTAT=ios)
IF (ios > 0) THEN
  write(*,*) "ERROR - Input file: '", TRIM(adjustl(ampvals)), "' does not exist ..."
  CLOSE(1)
  STOP
ENDIF
CLOSE(1) 

CALL GETARG(2,osacfile)

write(*,*) "Writing amplitude file: '", TRIM(adjustl(ampvals)), "' to SAC file: '", &
           & TRIM(adjustl(osacfile)), "' ..."

CALL GETARG(3,deltastr)
read(deltastr,*) deltaa

write(*,*) "Using delta spacing of: ", deltaa, " (sec);", 1.0/deltaa, " (Hz)"


!    --  READ INPUT TABLE  -- 
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!

!Determine total number of lines in file
NR = 0
OPEN(UNIT=1,FILE=ampvals)
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
ALLOCATE(amps(NR))

!Now read in data
DO J=1,NR
  READ(1,*) amps(J)
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

depmin = MINVAL(amps)
depmax = MAXVAL(amps)
b = 0.0
npts = NR
kuser0 = 'amp2sac'

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


END PROGRAM ampsac
