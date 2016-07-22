PROGRAM noisy
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
!
!
! michael.thorne@utah.edu
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
USE sac_i_o
IMPLICIT NONE
REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: sacdata
REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: xgrid, rand, auto, fdata
REAL(KIND=4)    :: rms, corner, arg
REAL(KIND=4)    :: x, aa, bb, phi
REAL(KIND=4)    :: ave, adev, sdev, var, skew, curt
REAL(KIND=4), PARAMETER :: pi = 3.141592654
INTEGER(KIND=4), DIMENSION(3)               :: dvec   !FFT dimension vector
INTEGER(KIND=4) :: NN, ios, NR, J, Kseed, JK
INTEGER(KIND=4) :: nx, counter
INTEGER(KIND=4), PARAMETER :: maxrecs = 10000
CHARACTER(LEN=112) :: flist, junk, file1, ofile

!    -- READ USER INPUT --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
NN = IARGC()

IF (NN < 3) THEN
  write(*,'(a)') "usage:  addnoise filelist RMS corner"
  write(*,'(a)') "        filelist: list of SAC files to add noise to"
  write(*,'(a)') "        RMS: root-mean square amplitude of noise"
  write(*,'(a)') "        Corner: corner period (s)"
  STOP
ENDIF

! read list of SAC files
CALL GETARG(1,flist)
OPEN(UNIT=1,FILE=flist,STATUS='OLD',IOSTAT=ios)
IF (ios > 0) THEN
  write(*,*) "ERROR - Input file: '", TRIM(adjustl(flist)), "' does not exist ..."
  CLOSE(1)
  STOP
ENDIF

! read RMS amplitude
CALL GETARG(2,junk)
READ(junk,*) rms

! read corner period
CALL GETARG(3,junk)
READ(junk,*) corner

write(*,*) "Adding noise to SAC files in list '", TRIM(ADJUSTL(flist)), "' ..."
write(*,*) "   RMS Amplitude variations: ", rms
write(*,*) "   Corner Period: ", corner, " (s)"
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!

!    -- FIND TOTAL # OF FILES
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
NR = 0
OPEN(UNIT=1,FILE=flist)
DO J=1,maxrecs
  READ(1,*,IOSTAT=ios) junk
  IF (ios == -1) EXIT
  IF (J == maxrecs) write(*,*) "Warning: Reached Maximum number of records.  Change 'maxrecs' and recompile..."
  NR = NR + 1
ENDDO
REWIND(1)
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!

! -- READ INPUT SAC FILES
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
DO JK=1,NR
  READ(1,*) file1

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
    kinst,sacdata)

  IF (nvhdr /= 6) THEN  !Check Header Version
    write(*,*) "ERROR - File: '", TRIM(adjustl(file1)), "' appears to be of non-native &
    &byte-order or is not a SAC file."
    STOP
  ENDIF


  !determine length of noise series
  nx = npts
  arg = npts
  DO WHILE (arg > 1)
    IF (mod(arg,2.0) /= 0.0) THEN
      nx = CEILING(LOG(REAL(nx))/LOG(2.0))
      nx = 2**nx
      EXIT
    ENDIF
    arg = arg/2.0
  ENDDO

  !Create grid
  ALLOCATE(xgrid(nx))
  x = -(nx*delta-delta)/2.0
  FORALL (J = 0:nx-1) xgrid(J+1) = x + J*delta

  !Create random matrix
  ALLOCATE(rand(nx))
  Kseed = 1
  CALL RANDOM_SEED (SIZE=Kseed)
  CALL RANDOM_NUMBER(rand(:))
  rand(:) = rand(:)*2.0*pi

  !Create Gaussian auto-correlation function
  ALLOCATE(auto(nx))
  IF (JK == 1) THEN
    corner = corner*corner
  ENDIF
  FORALL (J=1:nx)
    auto(J)=exp(-(xgrid(J)**2/corner))
  END FORALL

  !Write out autocorrelation function to an ascii file
  IF (JK == 1) THEN

   OPEN(UNIT=33,FILE='auto_gauss.xy')
   DO J=1,nx
     write(33,*) xgrid(J), auto(J)
   ENDDO
   CLOSE(33)

  ENDIF

  !FFT autocorrelation function
  ALLOCATE(fdata(2*nx))
  counter = 1
  DO J=1,nx
    fdata(counter) = auto(J)
    counter = counter + 1
    fdata(counter) = 0.0
    counter = counter + 1
  ENDDO 

  dvec(1) = nx; dvec(2) = 1; dvec(3) = 1
  CALL cool(fdata,2*nx,dvec,1,1)

  !Convolve autocorrelation function and random numbers
  counter = 1
  DO J=1,nx
    phi = rand(J)
    aa = fdata(counter)
    bb = fdata(counter+1)
    fdata(counter) = aa*cos(phi) - bb*sin(phi)
    fdata(counter+1) = aa*sin(phi) + bb*cos(phi)
    counter = counter + 2
  ENDDO

  !Do inverse FFT
  CALL cool(fdata,2*nx,dvec,1,-1)

  !unpack FFT vector
  counter = 1
  DO J=1,nx
    auto(J) = fdata(counter) !Just keep the real part
    counter = counter + 2
  ENDDO

  !scale
  CALL moment(auto,nx,ave,adev,sdev,var,skew,curt)
  auto = auto - ave
  auto = (auto/sdev)*rms

  !Write out noise file for first instance for checking
  IF (JK == 1) THEN
    ofile = 'noise.sac'

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
    kinst,auto)
  ENDIF

  !Add to SAC file
  sacdata(1:npts) = sacdata(1:npts) + auto(1:npts)

  !Write out sacfile
  ofile = 'noise_'//TRIM(ADJUSTL(file1))

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
  kinst,sacdata)


  !Deallocate for next file
  DEALLOCATE(xgrid)
  DEALLOCATE(rand)
  DEALLOCATE(auto)
  DEALLOCATE(fdata)
ENDDO
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!


END PROGRAM noisy

!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
! SUBROUTINES
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!

!COOL
!*----*----*----*----*----*----*----*----*----*----*----*----*----*----*----*
!Subroutine modifed from Numerical Recipes in Fortran
SUBROUTINE cool(data,nsize,nn,ndim,isign)
INTEGER(KIND=4), INTENT(IN) :: isign, ndim, nsize
INTEGER(KIND=4), DIMENSION(ndim), INTENT(IN) :: nn
REAL(KIND=4), DIMENSION(nsize), INTENT(INOUT) :: data
INTEGER(KIND=4) :: i1,i2,i2rev,i3,i3rev,ibit,idim,ifp1,ifp2,ip1,ip2,ip3,k1, &
                   k2,n,nprev,nrem,ntot
REAL(KIND=4) :: tempi,tempr
REAL(KIND=8) :: theta,wi,wpi,wpr,wr,wtemp
ntot=1
DO idim=1,ndim
  ntot=ntot*nn(idim)
ENDDO
nprev=1
DO idim=1,ndim
  n=nn(idim)
  nrem=ntot/(n*nprev)
  ip1=2*nprev
  ip2=ip1*n
  ip3=ip2*nrem
  i2rev=1
  DO i2=1,ip2,ip1
    IF (i2 < i2rev) THEN
      DO i1=i2,i2+ip1-2,2
        DO i3=i1,ip3,ip2
          i3rev=i2rev+i3-i2
          tempr=data(i3)
          tempi=data(i3+1)
          data(i3)=data(i3rev)
          data(i3+1)=data(i3rev+1)
          data(i3rev)=tempr
          data(i3rev+1)=tempi
        ENDDO
      ENDDO
    ENDIF
    ibit=ip2/2
    DO WHILE ((ibit >= ip1).AND.(i2rev > ibit))
      i2rev=i2rev-ibit
      ibit=ibit/2
    ENDDO
    i2rev=i2rev+ibit
  ENDDO
  ifp1=ip1
  DO WHILE (ifp1 < ip2)
    ifp2=2*ifp1
    theta=isign*6.28318530717959_8/(ifp2/ip1)
    wpr=-2.0_8*sin(0.5_8*theta)**2
    wpi=sin(theta)
    wr=1.0_8
    wi=0.0_8
    DO i3=1,ifp1,ip1
      DO i1=i3,i3+ip1-2,2
        DO i2=i1,ip3,ifp2
          k1=i2
          k2=k1+ifp1
          tempr=sngl(wr)*data(k2)-sngl(wi)*data(k2+1)
          tempi=sngl(wr)*data(k2+1)+sngl(wi)*data(k2)
          data(k2)=data(k1)-tempr
          data(k2+1)=data(k1+1)-tempi
          data(k1)=data(k1)+tempr
          data(k1+1)=data(k1+1)+tempi
        ENDDO
      ENDDO
      wtemp=wr
      wr=wr*wpr-wi*wpi+wr
      wi=wi*wpr+wtemp*wpi+wi
    ENDDO
    ifp1=ifp2
  ENDDO
  nprev=n*nprev
ENDDO

END SUBROUTINE cool
!*----*----*----*----*----*----*----*----*----*----*----*----*----*----*----*

!MOMENT
!*----*----*----*----*----*----*----*----*----*----*----*----*----*----*----*
!Subroutine modifed from Numerical Recipes in Fortran

SUBROUTINE moment(data,n,ave,adev,sdev,var,skew,curt)
INTEGER(KIND=4), INTENT(IN) :: n
REAL(KIND=4), DIMENSION(n), INTENT(IN) :: data
REAL(KIND=4), INTENT(OUT) :: adev,ave,curt,sdev,skew,var
REAL(KIND=4) :: p, s, ep
INTEGER(KIND=4) :: j

s=0.
DO j=1,n
  s=s+data(j)
ENDDO
ave=s/n
adev=0.
var=0.
skew=0.
curt=0.
ep=0.
DO j=1,n
  s=data(j)-ave
  ep=ep+s
  adev=adev+abs(s)
  p=s*s
  var=var+p
  p=p*s
  skew=skew+p
  p=p*s
  curt=curt+p
ENDDO
adev=adev/n
var=(var-ep**2/n)/(n-1)
sdev=sqrt(var)
IF (var.ne.0.) THEN
  skew=skew/(n*sdev**3)
  curt=curt/(n*var**2)-3.
ELSE
!      pause 'no skew or kurtosis when zero variance in moment'
ENDIF

END SUBROUTINE moment
!*----*----*----*----*----*----*----*----*----*----*----*----*----*----*----*

