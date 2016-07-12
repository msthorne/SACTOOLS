PROGRAM header
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
! Retrieve SAC header values
!
! michael.thorne@utah.edu
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
USE sac_i_o
IMPLICIT NONE
REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: sacfile
INTEGER(KIND=4)  :: NN, ios, J
CHARACTER(LEN=112) :: file1, variable


!    --  READ USER INPUT  --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
NN = IARGC()
IF (NN < 2) THEN
  write(*,'(a)') "usage:  sachead sacfile variable"
  write(*,'(a)') "        sacfile - input sac file"
  write(*,'(a)') "        variable - SAC header variable to read"
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

CALL GETARG(2,variable)
                                                                                
!    --  READ INPUT SAC FILES  --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
!read file1
CALL rbsac(file1,delta,depmin,depmax,scale,odelta,b,e,o,a,internal1,         &
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
kinst,sacfile)
                                                                                
IF (nvhdr /= 6) THEN
  write(*,*) "ERROR - File: '", TRIM(adjustl(file1)), "' appears to be of non-native &
  &byte-order or is not a SAC file."
  STOP
ENDIF

!    --  WRITE SAC VARIABLE  --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
  IF (variable == 'delta' .OR. variable == 'DELTA') THEN
    write(*,96)   "DELTA= ", delta
  ELSEIF (variable == 'depmin' .OR. variable == 'DEPMIN') THEN
    write(*,96)   "DEPMIN= ", depmin
  ELSEIF (variable == 'depmax' .OR. variable == 'DEPMAX') THEN
    write(*,96)   "DEPMAX= ", depmax
  ELSEIF (variable == 'scale' .OR. variable == 'SCALE') THEN
    write(*,96)   "SCALE= ", scale
  ELSEIF (variable == 'odelta' .OR. variable == 'ODELTA') THEN
    write(*,96)   "ODELTA= ", odelta
  ELSEIF (variable == 'b' .OR. variable == 'B') THEN
    write(*,96)   "B= ", b
  ELSEIF (variable == 'e' .OR. variable == 'E') THEN
    write(*,96)   "E= ", e
  ELSEIF (variable == 'o' .OR. variable == 'O') THEN
    write(*,96)   "O= ", o
  ELSEIF (variable == 'a' .OR. variable == 'A') THEN
    write(*,96)   "A= ", a
  ELSEIF (variable == 't0' .OR. variable == 'T0') THEN
    write(*,96)   "T0= ", t0
  ELSEIF (variable == 't1' .OR. variable == 'T1') THEN
    write(*,96)   "T1= ", t1
  ELSEIF (variable == 't2' .OR. variable == 'T2') THEN
    write(*,96)   "T2= ", t2
  ELSEIF (variable == 't3' .OR. variable == 'T3') THEN
    write(*,96)   "T3= ", t3
  ELSEIF (variable == 't4' .OR. variable == 'T4') THEN
    write(*,96)   "T4= ", t4
  ELSEIF (variable == 't5' .OR. variable == 'T5') THEN
    write(*,96)   "T5= ", t5
  ELSEIF (variable == 't6' .OR. variable == 'T6') THEN
    write(*,96)   "T6= ", t6
  ELSEIF (variable == 't7' .OR. variable == 'T7') THEN
    write(*,96)   "T7= ", t7
  ELSEIF (variable == 't8' .OR. variable == 'T8') THEN
    write(*,96)   "T8= ", t8
  ELSEIF (variable == 't9' .OR. variable == 'T9') THEN
    write(*,96)   "T9= ", t9
  ELSEIF (variable == 'f' .OR. variable == 'F') THEN
    write(*,96)   "F= ", f
  ELSEIF (variable == 'resp0' .OR. variable == 'RESP0') THEN
    write(*,96)   "RESP0= ", resp0
  ELSEIF (variable == 'resp1' .OR. variable == 'RESP1') THEN
    write(*,96)   "RESP1= ", resp1
  ELSEIF (variable == 'resp2' .OR. variable == 'RESP2') THEN
    write(*,96)   "RESP2= ", resp2
  ELSEIF (variable == 'resp3' .OR. variable == 'RESP3') THEN
    write(*,96)   "RESP3= ", resp3
  ELSEIF (variable == 'resp4' .OR. variable == 'RESP4') THEN
    write(*,96)   "RESP4= ", resp4
  ELSEIF (variable == 'resp5' .OR. variable == 'RESP5') THEN
    write(*,96)   "RESP5= ", resp5
  ELSEIF (variable == 'resp6' .OR. variable == 'RESP6') THEN
    write(*,96)   "RESP6= ", resp6
  ELSEIF (variable == 'resp7' .OR. variable == 'RESP7') THEN
    write(*,96)   "RESP7= ", resp7
  ELSEIF (variable == 'resp8' .OR. variable == 'RESP8') THEN
    write(*,96)   "RESP8= ", resp8
  ELSEIF (variable == 'resp9' .OR. variable == 'RESP9') THEN
    write(*,96)   "RESP9= ", resp9
  ELSEIF (variable == 'stla' .OR. variable == 'STLA') THEN
    write(*,96)   "STLA= ", stla
  ELSEIF (variable == 'stlo' .OR. variable == 'STLO') THEN
    write(*,96)   "STLO= ", stlo
  ELSEIF (variable == 'stel' .OR. variable == 'STEL') THEN
    write(*,96)   "STEL= ", stel
  ELSEIF (variable == 'stdp' .OR. variable == 'STDP') THEN
    write(*,96)   "STDP= ", stdp
  ELSEIF (variable == 'evla' .OR. variable == 'EVLA') THEN
    write(*,96)   "EVLA= ", evla
  ELSEIF (variable == 'evlo' .OR. variable == 'EVLO') THEN
    write(*,96)   "EVLO= ", evlo
  ELSEIF (variable == 'evel' .OR. variable == 'EVEL') THEN
    write(*,96)   "EVEL= ", evel
  ELSEIF (variable == 'evdp' .OR. variable == 'EVDP') THEN
    write(*,96)   "EVDP= ", evdp
  ELSEIF (variable == 'mag' .OR. variable == 'MAG') THEN
    write(*,96)   "MAG= ", mag
  ELSEIF (variable == 'user0' .OR. variable == 'USER0') THEN
    write(*,96)   "USER0= ", user0
  ELSEIF (variable == 'user1' .OR. variable == 'USER1') THEN
    write(*,96)   "USER1= ", user1
  ELSEIF (variable == 'user2' .OR. variable == 'USER2') THEN
    write(*,96)   "USER2= ", user2
  ELSEIF (variable == 'user3' .OR. variable == 'USER3') THEN
    write(*,96)   "USER3= ", user3
  ELSEIF (variable == 'user4' .OR. variable == 'USER4') THEN
    write(*,96)   "USER4= ", user4
  ELSEIF (variable == 'user5' .OR. variable == 'USER5') THEN
    write(*,96)   "USER5= ", user5
  ELSEIF (variable == 'user6' .OR. variable == 'USER6') THEN
    write(*,96)   "USER6= ", user6
  ELSEIF (variable == 'user7' .OR. variable == 'USER7') THEN
    write(*,96)   "USER7= ", user7
  ELSEIF (variable == 'user8' .OR. variable == 'USER8') THEN
    write(*,96)   "USER8= ", user8
  ELSEIF (variable == 'user9' .OR. variable == 'USER9') THEN
    write(*,96)   "USER9= ", user9
  ELSEIF (variable == 'dist' .OR. variable == 'DIST') THEN
    write(*,96)   "DIST= ", dist
  ELSEIF (variable == 'az' .OR. variable == 'AZ') THEN
    write(*,96)   "AZ= ", az
  ELSEIF (variable == 'baz' .OR. variable == 'BAZ') THEN
    write(*,96)   "BAZ= ", baz
  ELSEIF (variable == 'gcarc' .OR. variable == 'GCARC') THEN
    write(*,96)   "GCARC= ", gcarc
  ELSEIF (variable == 'depmen' .OR. variable == 'DEPMEN') THEN
    write(*,96)   "DEPMEN= ", depmen
  ELSEIF (variable == 'cmpaz' .OR. variable == 'CMPAZ') THEN
    write(*,96)   "CMPAZ= ", cmpaz
  ELSEIF (variable == 'cmpinc' .OR. variable == 'CMPINC') THEN
    write(*,96)   "CMPINC= ", cmpinc
  ELSEIF (variable == 'xminimum' .OR. variable == 'XMINIMUM') THEN
    write(*,96)   "XMINIMUM= ", xminimum
  ELSEIF (variable == 'xmaximum' .OR. variable == 'XMAXIMUM') THEN
    write(*,96)   "XMAXIMUM= ", xmaximum
  ELSEIF (variable == 'yminimum' .OR. variable == 'YMINIMUM') THEN
    write(*,96)   "YMINIMUM= ", yminimum
  ELSEIF (variable == 'ymaximum' .OR. variable == 'YMAXIMUM') THEN
    write(*,96)   "YMAXIMUM= ", ymaximum
  ELSEIF (variable == 'unused1' .OR. variable == 'UNUSED1') THEN
    write(*,96)   "UNUSED1= ", unused1
  ELSEIF (variable == 'unused2' .OR. variable == 'UNUSED2') THEN
    write(*,96)   "UNUSED2= ", unused2
  ELSEIF (variable == 'unused3' .OR. variable == 'UNUSED3') THEN
    write(*,96)   "UNUSED3= ", unused3
  ELSEIF (variable == 'unused4' .OR. variable == 'UNUSED4') THEN
    write(*,96)   "UNUSED4= ", unused4
  ELSEIF (variable == 'unused5' .OR. variable == 'UNUSED5') THEN
    write(*,96)   "UNUSED5= ", unused5
  ELSEIF (variable == 'unused6' .OR. variable == 'UNUSED6') THEN
    write(*,96)   "UNUSED6= ", unused6
  ELSEIF (variable == 'unused7' .OR. variable == 'UNUSED7') THEN
    write(*,96)   "UNUSED7= ", unused7
  ELSEIF (variable == 'unused8' .OR. variable == 'UNUSED8') THEN
    write(*,96)   "UNUSED8= ", unused8
  ELSEIF (variable == 'unused9' .OR. variable == 'UNUSED9') THEN
    write(*,96)   "UNUSED9= ", unused9
  ELSEIF (variable == 'unused10' .OR. variable == 'UNUSED10') THEN
    write(*,96)   "UNUSED10= ", unused10
  ELSEIF (variable == 'unused11' .OR. variable == 'UNUSED11') THEN
    write(*,96)   "UNUSED11= ", unused11
  ELSEIF (variable == 'unused12' .OR. variable == 'UNUSED12') THEN
    write(*,96)   "UNUSED12= ", unused12
  ELSEIF (variable == 'unused13' .OR. variable == 'UNUSED13') THEN
    write(*,96)   "UNUSED13= ", unused13
  ELSEIF (variable == 'unused14' .OR. variable == 'UNUSED14') THEN
    write(*,96)   "UNUSED14= ", unused14
  ELSEIF (variable == 'unused15' .OR. variable == 'UNUSED15') THEN
    write(*,96)   "UNUSED15= ", unused15
  ELSEIF (variable == 'unused16' .OR. variable == 'UNUSED16') THEN
    write(*,96)   "UNUSED16= ", unused16
  ELSEIF (variable == 'unused17' .OR. variable == 'UNUSED17') THEN
    write(*,96)   "UNUSED17= ", unused17
  ELSEIF (variable == 'nzyear' .OR. variable == 'NZYEAR') THEN
    write(*,97)   "NZYEAR= ", nzyear
  ELSEIF (variable == 'nzjday' .OR. variable == 'NZJDAY') THEN
    write(*,97)   "NZJDAY= ", nzjday
  ELSEIF (variable == 'nzhour' .OR. variable == 'NZHOUR') THEN
    write(*,97)   "NZHOUR= ", nzhour
  ELSEIF (variable == 'nzmin' .OR. variable == 'NZMIN') THEN
    write(*,97)   "NZMIN= ", nzmin
  ELSEIF (variable == 'nzsec' .OR. variable == 'NZSEC') THEN
    write(*,97)   "NZSEC= ", nzsec
  ELSEIF (variable == 'nzmsec' .OR. variable == 'NZMSEC') THEN
    write(*,97)   "NZMSEC= ", nzmsec
  ELSEIF (variable == 'nvhdr' .OR. variable == 'NVHDR') THEN
    write(*,97)   "NVHDR= ", nvhdr
  ELSEIF (variable == 'norid' .OR. variable == 'NORID') THEN
    write(*,97)   "NORID= ", norid
  ELSEIF (variable == 'nevid' .OR. variable == 'NEVID') THEN
    write(*,97)   "NEVID= ", nevid
  ELSEIF (variable == 'npts' .OR. variable == 'NPTS') THEN
    write(*,97)   "NPTS= ", npts
  ELSEIF (variable == 'nwfid' .OR. variable == 'NWFID') THEN
    write(*,97)   "NWFID= ", nwfid
  ELSEIF (variable == 'nxsize' .OR. variable == 'NXSIZE') THEN
    write(*,97)   "NXSIZE= ", nxsize
  ELSEIF (variable == 'nysize' .OR. variable == 'NYSIZE') THEN
    write(*,97)   "NYSIZE= ", nysize
  ELSEIF (variable == 'iftype' .OR. variable == 'IFTYPE') THEN
    write(*,97)   "IFTYPE= ", iftype
  ELSEIF (variable == 'iinst' .OR. variable == 'IINST') THEN
    write(*,97)   "IINST= ", iinst
  ELSEIF (variable == 'istreg' .OR. variable == 'ISTREG') THEN
    write(*,97)   "ISTREG= ", istreg
  ELSEIF (variable == 'ievreg' .OR. variable == 'IEVREG') THEN
    write(*,97)   "IEVREG= ", ievreg
  ELSEIF (variable == 'ievtyp' .OR. variable == 'IEVTYP') THEN
    write(*,97)   "IEVTYP= ", ievtyp
  ELSEIF (variable == 'iqual' .OR. variable == 'IQUAL') THEN
    write(*,97)   "IQUAL= ", iqual
  ELSEIF (variable == 'isynth' .OR. variable == 'ISYNTH') THEN
    write(*,97)   "ISYNTH= ", isynth
  ELSEIF (variable == 'imagtyp' .OR. variable == 'IMAGTYP') THEN
    write(*,97)   "IMAGTYP= ", imagtyp
  ELSEIF (variable == 'imagsrc' .OR. variable == 'IMAGSRC') THEN
    write(*,97)   "IMAGSRC= ", imagsrc
  ELSEIF (variable == 'leven' .OR. variable == 'LEVEN') THEN
    write(*,97)   "LEVEN= ", leven
  ELSEIF (variable == 'lpspol' .OR. variable == 'LPSPOL') THEN
    write(*,97)   "LPSPOL= ", lpspol
  ELSEIF (variable == 'lovrok' .OR. variable == 'LOVROK') THEN
    write(*,97)   "LOVROK= ", lovrok
  ELSEIF (variable == 'lcalda' .OR. variable == 'LCALDA') THEN
    write(*,97)   "LCALDA= ", lcalda
  ELSEIF (variable == 'kevnm' .OR. variable == 'KEVNM') THEN
    write(*,99)   "KEVNM= ", kevnm
  ELSEIF (variable == 'kstnm' .OR. variable == 'KSTNM') THEN
    write(*,98)   "KSTNM= ", kstnm
  ELSEIF (variable == 'khole' .OR. variable == 'KHOLE') THEN
    write(*,98)   "KHOLE= ", khole
  ELSEIF (variable == 'ko' .OR. variable == 'KO') THEN
    write(*,98)   "KO= ", ko
  ELSEIF (variable == 'ka' .OR. variable == 'KA') THEN
    write(*,98)   "KA= ", ka
  ELSEIF (variable == 'kt0' .OR. variable == 'KT0') THEN
    write(*,98)   "KT0= ", kt0
  ELSEIF (variable == 'kt1' .OR. variable == 'KT1') THEN
    write(*,98)   "KT1= ", kt1
  ELSEIF (variable == 'kt2' .OR. variable == 'KT2') THEN
    write(*,98)   "KT2= ", kt2
  ELSEIF (variable == 'kt3' .OR. variable == 'KT3') THEN
    write(*,98)   "KT3= ", kt3
  ELSEIF (variable == 'kt4' .OR. variable == 'KT4') THEN
    write(*,98)   "KT4= ", kt4
  ELSEIF (variable == 'kt5' .OR. variable == 'KT5') THEN
    write(*,98)   "KT5= ", kt5
  ELSEIF (variable == 'kt6' .OR. variable == 'KT6') THEN
    write(*,98)   "KT6= ", kt6
  ELSEIF (variable == 'kt7' .OR. variable == 'KT7') THEN
    write(*,98)   "KT7= ", kt7
  ELSEIF (variable == 'kt8' .OR. variable == 'KT8') THEN
    write(*,98)   "KT8= ", kt8
  ELSEIF (variable == 'kt9' .OR. variable == 'KT9') THEN
    write(*,98)   "KT9= ", kt9
  ELSEIF (variable == 'kf' .OR. variable == 'KF') THEN
    write(*,98)   "KF= ", kf
  ELSEIF (variable == 'kuser0' .OR. variable == 'KUSER0') THEN
    write(*,98)   "KUSER0= ", kuser0
  ELSEIF (variable == 'kuser1' .OR. variable == 'KUSER1') THEN
    write(*,98)   "KUSER1= ", kuser1
  ELSEIF (variable == 'kuser2' .OR. variable == 'KUSER2') THEN
    write(*,98)   "KUSER2= ", kuser2
  ELSEIF (variable == 'kcmpnm' .OR. variable == 'KCMPNM') THEN
    write(*,98)   "KCMPNM= ", kcmpnm
  ELSEIF (variable == 'knetwk' .OR. variable == 'KNETWK') THEN
    write(*,98)   "KNETWK= ", knetwk
  ELSEIF (variable == 'kdatrd' .OR. variable == 'KDATRD') THEN
    write(*,98)   "KDATRD= ", kdatrd
  ELSEIF (variable == 'kinst' .OR. variable == 'KINST') THEN
    write(*,98)   "KINST= ", kinst
  ELSE
    write(*,*) "ERROR - Variable: '",TRIM(adjustl(variable)), "' does not exist ..."
  ENDIF

96 FORMAT(A,F15.7)
97 FORMAT(A,5I10)
98 FORMAT(A,A8)
99 FORMAT(A,A16)

END PROGRAM header
