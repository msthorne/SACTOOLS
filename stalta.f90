PROGRAM sacstalta
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
! Compute Short term, Long term averages on a SAC file
!  note: this is simple LTA, not recursive formula
!
! michael.thorne@utah.edu
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
USE sac_i_o
IMPLICIT NONE
REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: dinput, sta, lta, snr
REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: dummy
REAL(KIND=4)       :: delta1, b1, e1
REAL(KIND=4)       :: L_sta, L_lta
INTEGER(KIND=4)    :: NN, ios, npts1, J, KK
INTEGER(KIND=4)    :: I_sta, I_lta
INTEGER(KIND=4)    :: NR
INTEGER(KIND=4), PARAMETER :: maxrecs = 10000
CHARACTER(LEN=112) :: file1, junk
CHARACTER(LEN=112) :: file2, ofile


!L_sta = 5.0    !seconds
!L_lta = 100.0    !seconds

!    --  READ USER INPUT  --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
NN = IARGC()
IF (NN < 3) THEN
  write(*,'(a)') "usage:  stalta infile STA LTA"
  write(*,'(a)') "        infile: input sac file"
  write(*,'(a)') "        STA: time (seconds) of Short Term Average"
  write(*,'(a)') "        LTA: time (seconds) of Long  Term Average"
  STOP
ENDIF

CALL GETARG(1,file1)
CALL GETARG(2,junk)
READ(junk,*) L_sta 
CALL GETARG(3,junk)
READ(junk,*) L_lta 

write(*,*) "Computing STA, LTA, and SNR on file '", TRIM(adjustl(file1)), "' ..."
write(*,*) "STA: ", L_sta, " (s)"
write(*,*) "LTA: ", L_lta, " (s)"
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!

!    --  READ INPUT SAC FILES  --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
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
kinst,dinput)


IF (nvhdr /= 6) THEN  !Check Header Version
  write(*,*) "ERROR - File: '", TRIM(adjustl(file1)), "' appears to be of non-native &
  &byte-order or is not a SAC file."
  STOP
ENDIF

ALLOCATE(sta(npts1))
ALLOCATE(lta(npts1))
ALLOCATE(snr(npts1))
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!

!   -- CALCULATE STA --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
sta(:) = 0.0
I_sta = NINT(L_sta/delta1)
DO J=(I_sta+1),npts1
  DO KK=0,(I_sta-1)
    sta(J) = sta(J) + ABS(dinput(J-KK))
  ENDDO
  sta(J) = sta(J)/REAL(I_sta)
ENDDO
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!

!   -- CALCULATE LTA --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
lta(:) = 0.0
I_lta = NINT(L_lta/delta1)
DO J=(I_lta+1),npts1
  DO KK=0,(I_lta-1)
    lta(J) = lta(J) + ABS(dinput(J-KK))
  ENDDO
  lta(J) = lta(J)/REAL(I_lta)
ENDDO
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!

!   -- CALCULATE SNR --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
snr(:) = 0.0
DO J=1,npts1
 IF (lta(J) /= 0.0) THEN
   snr(J) = sta(J)/lta(J)
 ELSE
   snr(J) = 0.0
 ENDIF
ENDDO
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!


!    --  WRITE  OUT SAC FILE   --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
ofile ='sta.sac'
delta =delta1
depmin =MINVAL(sta)
depmax =MAXVAL(sta)
kevnm ='STA'

CALL wbsac(ofile,delta,depmin,depmax,scale,odelta,b1,e1,o,a,internal1,         &
t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,f,resp0,resp1,resp2,resp3,resp4,resp5,resp6,   &
resp7,resp8,resp9,stla,stlo,stel,stdp,evla,evlo,evel,evdp,mag,user0,user1,   &
user2,user3,user4,user5,user6,user7,user8,user9,dist,az,baz,gcarc,internal2, &
internal3,depmen,cmpaz,cmpinc,xminimum,xmaximum,yminimum,ymaximum,unused1,   &
unused2,unused3,unused4,unused5,unused6,unused7,nzyear,nzjday,nzhour,nzmin,  &
nzsec,nzmsec,nvhdr,norid,nevid,npts1,internal4,nwfid,nxsize,nysize,unused8,   &
iftype,idep,iztype,unused9,iinst,istreg,ievreg,ievtyp,iqual,isynth,imagtyp,  &
imagsrc,unused10,unused11,unused12,unused13,unused14,unused15,unused16,      &
unused17,leven,lpspol,lovrok,lcalda,unused18,kevnm,kstnm,khole,ko,ka,kt0,kt1,&
kt2,kt3,kt4,kt5,kt6,kt7,kt8,kt9,kf,kuser0,kuser1,kuser2,kcmpnm,knetwk,kdatrd,&
kinst,sta)

ofile ='lta.sac'
delta =delta1
depmin =MINVAL(lta)
depmax =MAXVAL(lta)
kevnm ='LTA'

CALL wbsac(ofile,delta,depmin,depmax,scale,odelta,b1,e1,o,a,internal1,         &
t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,f,resp0,resp1,resp2,resp3,resp4,resp5,resp6,   &
resp7,resp8,resp9,stla,stlo,stel,stdp,evla,evlo,evel,evdp,mag,user0,user1,   &
user2,user3,user4,user5,user6,user7,user8,user9,dist,az,baz,gcarc,internal2, &
internal3,depmen,cmpaz,cmpinc,xminimum,xmaximum,yminimum,ymaximum,unused1,   &
unused2,unused3,unused4,unused5,unused6,unused7,nzyear,nzjday,nzhour,nzmin,  &
nzsec,nzmsec,nvhdr,norid,nevid,npts1,internal4,nwfid,nxsize,nysize,unused8,   &
iftype,idep,iztype,unused9,iinst,istreg,ievreg,ievtyp,iqual,isynth,imagtyp,  &
imagsrc,unused10,unused11,unused12,unused13,unused14,unused15,unused16,      &
unused17,leven,lpspol,lovrok,lcalda,unused18,kevnm,kstnm,khole,ko,ka,kt0,kt1,&
kt2,kt3,kt4,kt5,kt6,kt7,kt8,kt9,kf,kuser0,kuser1,kuser2,kcmpnm,knetwk,kdatrd,&
kinst,lta)

ofile ='snr.sac'
delta =delta1
depmin =MINVAL(snr)
depmax =MAXVAL(snr)
kevnm ='SNR'

CALL wbsac(ofile,delta,depmin,depmax,scale,odelta,b1,e1,o,a,internal1,         &
t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,f,resp0,resp1,resp2,resp3,resp4,resp5,resp6,   &
resp7,resp8,resp9,stla,stlo,stel,stdp,evla,evlo,evel,evdp,mag,user0,user1,   &
user2,user3,user4,user5,user6,user7,user8,user9,dist,az,baz,gcarc,internal2, &
internal3,depmen,cmpaz,cmpinc,xminimum,xmaximum,yminimum,ymaximum,unused1,   &
unused2,unused3,unused4,unused5,unused6,unused7,nzyear,nzjday,nzhour,nzmin,  &
nzsec,nzmsec,nvhdr,norid,nevid,npts1,internal4,nwfid,nxsize,nysize,unused8,   &
iftype,idep,iztype,unused9,iinst,istreg,ievreg,ievtyp,iqual,isynth,imagtyp,  &
imagsrc,unused10,unused11,unused12,unused13,unused14,unused15,unused16,      &
unused17,leven,lpspol,lovrok,lcalda,unused18,kevnm,kstnm,khole,ko,ka,kt0,kt1,&
kt2,kt3,kt4,kt5,kt6,kt7,kt8,kt9,kf,kuser0,kuser1,kuser2,kcmpnm,knetwk,kdatrd,&
kinst,snr)


!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!


END PROGRAM sacstalta
