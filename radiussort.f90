PROGRAM radiussorting
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
! Search for SAC files within a specified radius (binsize) of a center
! latitude and longitude 
!
!
! michael.thorne@utah.edu
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
USE sac_i_o
IMPLICIT NONE
REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: dinput
REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: latlons
REAL(KIND=4)       :: binsize, clat, clon
REAL(KIND=4)       :: slat, slon
REAL(KIND=4)       :: x1, y1, z1, x2, y2, z2, ddprod, alpha, s
REAL(KIND=4), PARAMETER :: Rad=6371.0_4
INTEGER(KIND=4)    :: NN, ios, J, NR
INTEGER(KIND=4), PARAMETER :: maxrecs = 10000
CHARACTER(LEN=112) :: file1, junk
CHARACTER(LEN=112) :: file2, cmd
CHARACTER(LEN=112), DIMENSION(:), ALLOCATABLE :: fnames

!    --  READ USER INPUT  --
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
NN = IARGC()
IF (NN < 4) THEN
  write(*,'(a)') "usage:  radiussort filelist clat clon binsize"
  write(*,'(a)') "        filelist: list of files to be stacked"
  write(*,'(a)') "        clat: center latitude"
  write(*,'(a)') "        clon: center longitude"
  write(*,'(a)') "        binsize: radius of geographic bins (deg)"
  STOP
ENDIF

CALL GETARG(1,file1)
OPEN(UNIT=1,FILE=file1,STATUS='OLD',IOSTAT=ios)
IF (ios > 0) THEN
  write(*,*) "ERROR - Input file: '", TRIM(adjustl(file1)), "' does not exist ..."
  CLOSE(1)
  STOP
ENDIF

CALL GETARG(2,junk)
READ(junk,*) clat

CALL GETARG(3,junk)
READ(junk,*) clon

CALL GETARG(4,junk)
READ(junk,*) binsize

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
write(*,*) "Sorting ", NR, " SAC files..."
REWIND(1)
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!

!    --  READ INPUT SAC FILES AND STORE STLA, STLO, and filename
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
ALLOCATE(latlons(2,NR))
ALLOCATE(fnames(NR))
DO J=1,NR

  READ(1,*) file2

    CALL rbsac(file2,delta,depmin,depmax,scale,odelta,b,e,o,a,internal1,      &
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
    kinst,dinput)

  IF (nvhdr /= 6) THEN  !Check Header Version
    write(*,*) "ERROR - File: '", TRIM(adjustl(file2)), "' appears to be of non-native &
    &byte-order or is not a SAC file."
    STOP
  ENDIF

  latlons(1,J) = stla
  latlons(2,J) = stlo
  fnames(J) = TRIM(ADJUSTL(file2))

ENDDO
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!


! Loop through data and cp records into sorted bin
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!
cmd = '/bin/mkdir SORTED'
CALL SYSTEM(cmd)
CALL cartesian(clat, clon, Rad, x1, y1, z1)

DO J=1,NR
   slat = latlons(1,J)
   slon = latlons(2,J)
   CALL cartesian(slat, slon, Rad, x2, y2, z2)
   CALL dot(x1,y1,z1,x2,y2,z2,ddprod,alpha,s)
   IF (alpha <= binsize) THEN
     cmd = '/bin/cp '//TRIM(ADJUSTL(fnames(J)))//' SORTED'
     CALL SYSTEM(cmd)
   ENDIF
ENDDO
!:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:=====:!

END PROGRAM radiussorting

SUBROUTINE cartesian(Lat, Lon, Rad, x, y, z)
! Takes input Latitude, Longitude, and Radius
! Returns x, y, z coordinates
IMPLICIT NONE
INTEGER, PARAMETER :: k=4
REAL(k), PARAMETER :: pi=3.141592653589793_k
REAL(k), INTENT(IN) :: Lat, Lon, Rad
REAL(k), INTENT(OUT) :: x, y, z
REAL(k) :: rho, phi, theta, dtr

dtr = pi/180_k

rho = Rad; theta = Lon*dtr
Phi = (90_k - Lat)*dtr
        
x = rho*SIN(Phi)*COS(theta)
y = rho*SIN(Phi)*SIN(theta)
z = rho*COS(Phi)
        
END SUBROUTINE cartesian

SUBROUTINE dot(v1, v2, v3, w1, w2, w3, ddprod, alpha, s)
! For vectors v and w, where
! v = (v1, v2, v3)
! w = (w1, w2, w3)
! the dot product output is the scalar variable ddprod
! the angle between the vectors is given as alpha, where
! alpha = arccos(v*w/|v||w|)
! s is the distance along the arc between the two endpoints
! of vectors v and w, where s=r*theta.  alpha and s are
! valid for angles <= 180 deg
IMPLICIT NONE
INTEGER, PARAMETER :: k=4
REAL(k), PARAMETER :: pi=3.141592653589793_k
REAL(k), INTENT(IN) :: v1, v2, v3, w1, w2, w3
REAL(k), INTENT(OUT) :: ddprod, alpha, s
REAL(k) :: rtd, magW, magV, arg, theta

rtd = 180_k/pi

ddprod = v1*w1 + v2*w2 + v3*w3

magV = SQRT(v1**2 + v2**2 + v3**2)
magW = SQRT(w1**2 + w2**2 + w3**2)

arg = ddprod/(magV*magW)
theta = ACOS(arg)
s = magV*theta
alpha = (ACOS(arg))*rtd

END SUBROUTINE dot

