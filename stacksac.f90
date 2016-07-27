PROGRAM sacstacking
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
! Performs simple linear stacking of SAC formatted files listed
! in the input file
!
! michael.thorne@utah.edu
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
USE sac_i_o
IMPLICIT NONE
REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: dinput, stacked
REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: dummy
REAL(KIND=4)       :: delta1, delta2, b1, b2, e1, e2
REAL(KIND=4)       :: root
INTEGER(KIND=4)    :: NN, ios, npts1, npts2, npts3, J
INTEGER(KIND=4)    :: NR
INTEGER(KIND=4), PARAMETER :: maxrecs = 10000
CHARACTER(LEN=112) :: file1, junk
CHARACTER(LEN=112) :: file2, ofile

!    --  READ USER INPUT  --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
NN = IARGC()
IF (NN < 1) THEN
  write(*,'(a)') "usage:  stacksac filelist [root]"
  write(*,'(a)') "        filelist: list of files to be stacked"
  write(*,'(a)') "        OPTIONAL: root for n-th root stacking"
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

IF (NN == 2) THEN
  CALL GETARG(2,junk)
  READ(junk,*) root
  write(*,*) "  Using root: ", root, " stacking..."
ELSE
  root = 1.0
ENDIF
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!

!    -- FIND TOTAL # OF FILES
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

!    --  READ INPUT SAC FILES AND STACK --
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
    write(*,*) "ERROR - File: '", TRIM(adjustl(file2)), "' appears to be of non-native &
    &byte-order or is not a SAC file."
    STOP
  ENDIF

  IF (delta1 /= delta2 .AND. J > 1) THEN  !Check equal sample rates
    write(*,*) "ERROR - Input files contain unequal sample rates ..."
    STOP
  ENDIF

  npts3 = npts1
  IF (npts2 > npts1 .AND. J > 1) THEN !SAC file has more samples
    write(*,*) "WARNING: Input file '", TRIM(adjustl(file2)), "' is not equal &
               &length.  File has a larger number of samples. Truncating file..." 
  ELSEIF (npts2 < npts1 .AND. J > 1) THEN
    write(*,*) "WARNING: Input file '", TRIM(adjustl(file2)), "' is not equal &
               &length.  File has a smaller number of samples. Padding end with&
               &zeros ..." 
    npts3 = npts2
  ENDIF

  IF (J == 1) THEN
    ALLOCATE(stacked(npts1))
    !stacked(1:npts1) = dinput(1:npts1)
    stacked(1:npts1) = SIGN(ABS(dinput(1:npts1))**(1./root),dinput(1:npts1))
  ELSE
    !stacked(1:npts3) = stacked(1:npts3) + dinput(1:npts3) 
    stacked(1:npts3) = stacked(1:npts3) + SIGN(ABS(dinput(1:npts3))**(1./root),dinput(1:npts3))
  ENDIF

ENDDO
stacked(1:npts3) = (SIGN(ABS(stacked(1:npts3))**(root),stacked(1:npts3)))/REAL(NR)
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!

!    --  WRITE OUT STACK  --
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
