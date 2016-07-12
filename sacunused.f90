PROGRAM writeunusedvariables
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
! Write variables to the 'unused' header spaces in SAC files
!
! michael.thorne@utah.edu
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
USE sac_i_o
IMPLICIT NONE
REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: sacdata
REAL(KIND=4) :: realinvalue
INTEGER(KIND=4)    :: NN, ios, J
CHARACTER(LEN=112) :: file1
CHARACTER(LEN=2) :: variable
CHARACTER(LEN=80) :: invalue

!    --  READ USER INPUT  --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
NN = IARGC()
IF (NN < 3) THEN
  write(*,'(a)') "usage:  sacunused sacfile variable value"
  write(*,'(a)') "        sacfile: SAC file to write header value to"
  write(*,'(a)') "        variable: which header variable"
  write(*,'(a)') "        value: what to write to header"
  write(*,'(a)') "        "
  write(*,'(a)') "        Example:"
  write(*,'(a)') "        sacunused foo.sac 1 3.0"
  write(*,'(a)') "        will put the number '3.0' in sac variable unused1"
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

CALL GETARG(2,variable)

CALL GETARG(3,invalue)
read(invalue,*) realinvalue

write(*,*) "Writing value: ", realinvalue, " to header: '", TRIM(adjustl(variable)), &
  & "' in file: '", TRIM(adjustl(file1)), "' ..." 

!    --  READ INPUT SAC FILES  --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!

!read file1
CALL rbsac(file1,delta,depmin,depmax,scale,odelta,b,e,o,a,internal1,      &
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
  write(*,*) "ERROR - File: '", TRIM(adjustl(file1)), "' appears to be of non-native &
  &byte-order or is not a SAC file."
  STOP
ENDIF

!    -- INITIALIZE UNUSED VARIABLE --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!

IF (variable == '1') THEN
  unused1 = realinvalue
ELSEIF (variable == '2') THEN
  unused2 = realinvalue
ELSEIF (variable == '3') THEN
  unused3 = realinvalue
ELSEIF (variable == '4') THEN
  unused4 = realinvalue
ELSEIF (variable == '5') THEN
  unused5 = realinvalue
ELSEIF (variable == '6') THEN
  unused6 = realinvalue
ELSEIF (variable == '7') THEN
  unused7 = realinvalue
ELSEIF (variable == '8') THEN
  unused8 = realinvalue
ELSEIF (variable == '9') THEN
  unused9 = realinvalue
ELSEIF (variable == '10') THEN
  unused10 = realinvalue
ELSEIF (variable == '11') THEN
  unused11 = realinvalue
ELSEIF (variable == '12') THEN
  unused12 = realinvalue
ELSEIF (variable == '13') THEN
  unused13 = realinvalue
ELSEIF (variable == '14') THEN
  unused14 = realinvalue
ELSEIF (variable == '15') THEN
  unused15 = realinvalue
ELSEIF (variable == '16') THEN
  unused16 = realinvalue
ELSEIF (variable == '17') THEN
  unused17 = realinvalue
ENDIF

!    --  WRITE OUT SAC FILE
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!

CALL wbsac(file1,delta,depmin,depmax,scale,odelta,b,e,o,a,internal1,         &
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
kinst,sacdata)


END PROGRAM writeunusedvariables
