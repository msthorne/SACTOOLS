PROGRAM sacstacking
!USE F90_UNIX_ENV
!Performs simple linear stacking of SAC formatted files listed
! in the input file
USE sac_i_o
IMPLICIT NONE
REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: dinput, stacked
REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: dummy
REAL(KIND=4)       :: delta1, delta2, b1, b2, e1, e2
INTEGER(KIND=4)    :: NN, ios, npts1, npts2, J
INTEGER(KIND=4)    :: NR
INTEGER(KIND=4), PARAMETER :: maxrecs = 1000
CHARACTER(LEN=112) :: file1, junk
CHARACTER(LEN=112) :: file2, ofile

!    --  R E A D  U S E R  I N P U T  --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
NN = IARGC()
IF (NN < 1) THEN
  write(*,'(a)') "usage:  stacksac filelist"
  write(*,'(a)') "        filelist: list of files to be stacked"
  STOP
ENDIF

CALL GETARG(1,file1)
OPEN(UNIT=1,FILE=file1,STATUS='OLD',IOSTAT=ios)
IF (ios > 0) THEN
  write(*,*) "ERROR - Input file: '", TRIM(adjustl(file1)), "' does not exist ..."
  CLOSE(1)
  STOP
ENDIF

write(*,*) "Stacking SAC files in list '", TRIM(adjustl(file1)), "' ..."
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!

!    -- F I N D   T O T A L   #   O F   F I L E S
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
NR = 0
OPEN(UNIT=1,FILE=file1)
DO J=1,maxrecs
  READ(1,*,IOSTAT=ios) junk
  IF (ios == -1) EXIT
  IF (J == maxrecs) write(*,*) "Warning: Reached Maximum number of records.  Change 'maxrecs' and recompile..."
  NR = NR + 1
ENDDO
write(*,*) "Stacking ", NR, " SAC files..."
REWIND(1)
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!

!    --  R E A D   I N P U T  S A C  F I L E S  A N D  S T A C K --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
DO J=1,NR

  READ(1,*) file2

  IF ( J == 1) THEN
    CALL rbsac(file2,delta1,depmin,depmax,scale,odelta,b1,e1,o,a,internal1,      &
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
    kinst,dinput)
  ELSE
    CALL rbsac(file2,delta2,depmin,depmax,scale,odelta,b1,e1,o,a,internal1,      &
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
    kinst,dinput)
  ENDIF

  IF (nvhdr /= 6) THEN  !Check Header Version
    write(*,*) "ERROR - File: '", TRIM(adjustl(file1)), "' appears to be of non-native &
    &byte-order or is not a SAC file."
    STOP
  ENDIF

  IF (delta1 /= delta2 .AND. J > 1) THEN  !Check equal sample rates
    write(*,*) "ERROR - Input files contain unequal sample rates ..."
    STOP
  ENDIF

  IF (npts1 /= npts2 .AND. J > 1) THEN  !Check vector lengths
    write(*,*) "ERROR - Input files are not equal length ..."
    STOP
  ENDIF

  IF (J == 1) THEN
    ALLOCATE(stacked(npts1))
    stacked = dinput
  ELSE
    stacked = stacked + dinput 
  ENDIF

ENDDO
stacked = stacked/NR
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!

!    --  W R I T E   O U T   S T A C K  --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!

!Initialize sac file
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

ofile ='stacked.sac'
delta =delta1
depmin =MINVAL(stacked)
depmax =MAXVAL(stacked)
!b =-(float(npts1-1))*delta
!e =(float(npts1-1))*delta
!npts =((npts1-1)*2+1)
b = b1
e = e1
npts = npts1
kevnm ='stacksac output'
kuser0 = 'N traces'
user0 = NR

CALL wbsac(ofile,delta,depmin,depmax,scale,odelta,b,e,o,a,internal1,         &
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
kinst,stacked)
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!



END PROGRAM sacstacking
