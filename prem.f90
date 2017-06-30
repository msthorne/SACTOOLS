!***********************************************************************!
!
!PROGRAM prem
!
!This is a utility that allows you to enter a depth in km, and 
!	additionally perturbations in % for Vs, Vp, and Density
!	The output will be the PREM predictions.
!
!	To compile this program you must use the subroutine:
!	premsub.f90
!
!	To compile:
!
!	>>  f90 -c premsub.f90
!	>>  f90 prem.f90 premsub.o -o prem
!
!	To run, simply type prem.
!
!	Author:  Michael Thorne, contact mthorne@asu.edu
!	Jun 27, 2002
!
!***********************************************************************!
PROGRAM grabprem
USE getprem
IMPLICIT NONE
!******************************************************************!
!
REAL(KIND=8) :: depth, delS, delP, delrho
REAL(KIND=8) :: Vs, Vp, rho, r
REAL(KIND=8) :: Vs1, Vs2, Vp1, Vp2, rho1, rho2
REAL(KIND=8), PARAMETER :: rad=6371_8
INTEGER(KIND=4) :: disc, fo
CHARACTER(LEN=60) :: comment
CHARACTER(LEN=1) :: continue
!
!******************************************************************!
!
DO
PRINT *, " "
PRINT *, " "
PRINT *, "Enter Depth, Delta S, Delta P, Delta Rho"
PRINT *, " "
READ *, depth, delS, delP, delrho
PRINT *, " "
PRINT *, " "
!
!******************************************************************!
!
r = rad - depth
!
!******************************************************************!
!
IF (r == 1221.5_8) THEN
    fo = 1
    comment = "Inner-Core/Outer-Core Boundary"
    disc = 1
    Call gprem(depth, delS, delP, delrho, Vs, Vp, rho, disc)
    Vs1 = Vs; Vp1 = Vp; rho1 = rho
    disc = 2
    Call gprem(depth, delS, delP, delrho, Vs, Vp, rho, disc)	
    Vs2 = Vs; Vp2 = Vp; rho2 = rho
ELSEIF (r == 3480_8) THEN
    comment = "Core Mantle Boundary"
    fo = 1
    disc = 1
    Call gprem(depth, delS, delP, delrho, Vs, Vp, rho, disc)
    Vs1 = Vs; Vp1 = Vp; rho1 = rho
    disc = 2
    Call gprem(depth, delS, delP, delrho, Vs, Vp, rho, disc)
    Vs2 = Vs; Vp2 = Vp; rho2 = rho
ELSEIF (r == 3630_8) THEN
    fo = 1
    disc = 1
    Call gprem(depth, delS, delP, delrho, Vs, Vp, rho, disc)
    Vs1 = Vs; Vp1 = Vp; rho1 = rho
    disc = 2
    Call gprem(depth, delS, delP, delrho, Vs, Vp, rho, disc)
    Vs2 = Vs; Vp2 = Vp; rho2 = rho
ELSEIF (r == 5600_8) THEN
    fo = 1
    disc = 1
    Call gprem(depth, delS, delP, delrho, Vs, Vp, rho, disc)
    Vs1 = Vs; Vp1 = Vp; rho1 = rho
    disc = 2
    Call gprem(depth, delS, delP, delrho, Vs, Vp, rho, disc)
    Vs2 = Vs; Vp2 = Vp; rho2 = rho
ELSEIF (r == 5701_8) THEN
    fo = 1
    disc = 1
    Call gprem(depth, delS, delP, delrho, Vs, Vp, rho, disc)
    Vs1 = Vs; Vp1 = Vp; rho1 = rho
    disc = 2
    Call gprem(depth, delS, delP, delrho, Vs, Vp, rho, disc)
    Vs2 = Vs; Vp2 = Vp; rho2 = rho
ELSEIF (r == 5771_8) THEN
    fo = 1
    disc = 1
    Call gprem(depth, delS, delP, delrho, Vs, Vp, rho, disc)
    Vs1 = Vs; Vp1 = Vp; rho1 = rho
    disc = 2
    Call gprem(depth, delS, delP, delrho, Vs, Vp, rho, disc)
    Vs2 = Vs; Vp2 = Vp; rho2 = rho
ELSEIF (r == 5971_8) THEN
    fo = 1
    disc = 1
    Call gprem(depth, delS, delP, delrho, Vs, Vp, rho, disc)
    Vs1 = Vs; Vp1 = Vp; rho1 = rho
    disc = 2
    Call gprem(depth, delS, delP, delrho, Vs, Vp, rho, disc)
    Vs2 = Vs; Vp2 = Vp; rho2 = rho
ELSEIF (r == 6151_8) THEN
    fo = 1
    disc = 1
    Call gprem(depth, delS, delP, delrho, Vs, Vp, rho, disc)
    Vs1 = Vs; Vp1 = Vp; rho1 = rho
    disc = 2
    Call gprem(depth, delS, delP, delrho, Vs, Vp, rho, disc)
    Vs2 = Vs; Vp2 = Vp; rho2 = rho
ELSEIF (r == 6291_8) THEN
    fo = 1
    disc = 1
    Call gprem(depth, delS, delP, delrho, Vs, Vp, rho, disc)
    Vs1 = Vs; Vp1 = Vp; rho1 = rho
    disc = 2
    Call gprem(depth, delS, delP, delrho, Vs, Vp, rho, disc)
    Vs2 = Vs; Vp2 = Vp; rho2 = rho
ELSEIF (r == 6346.6_8) THEN
    fo = 1
    disc = 1
    Call gprem(depth, delS, delP, delrho, Vs, Vp, rho, disc)
    Vs1 = Vs; Vp1 = Vp; rho1 = rho
    disc = 2
    Call gprem(depth, delS, delP, delrho, Vs, Vp, rho, disc)
    Vs2 = Vs; Vp2 = Vp; rho2 = rho
ELSEIF (r == 6356_8) THEN
    fo = 1
    disc = 1
    Call gprem(depth, delS, delP, delrho, Vs, Vp, rho, disc)
    Vs1 = Vs; Vp1 = Vp; rho1 = rho
    disc = 2
    Call gprem(depth, delS, delP, delrho, Vs, Vp, rho, disc)
    Vs2 = Vs; Vp2 = Vp; rho2 = rho
ELSEIF (r == 6368_8) THEN
    comment = "Note:  0-3 km depth PREM Ocean Layer"
    fo = 1
    disc = 1
    Call gprem(depth, delS, delP, delrho, Vs, Vp, rho, disc)
    Vs1 = Vs; Vp1 = Vp; rho1 = rho
    disc = 2
    Call gprem(depth, delS, delP, delrho, Vs, Vp, rho, disc)
    Vs2 = Vs; Vp2 = Vp; rho2 = rho
ELSEIF ((r >= 0_8).AND.(r < 1221.5_8)) THEN
    fo = 2
    comment = "Inner Core"
    disc = 1
    Call gprem(depth, delS, delP, delrho, Vs, Vp, rho, disc)
    Vs1 = Vs; Vp1 = Vp; rho1 = rho
ELSEIF ((r > 1221.5_8).AND.(r < 3480_8)) THEN
    fo = 2
    comment = "Outer Core"
    disc = 1
    Call gprem(depth, delS, delP, delrho, Vs, Vp, rho, disc)
    Vs1 = Vs; Vp1 = Vp; rho1 = rho
ELSEIF ((r > 3480_8).AND.(r < 5701_8)) THEN
    fo = 2
    comment = "Lower Mantle"
    disc = 1
    Call gprem(depth, delS, delP, delrho, Vs, Vp, rho, disc)
    Vs1 = Vs; Vp1 = Vp; rho1 = rho
ELSEIF ((r > 5701_8).AND.(r < 6151_8)) THEN
    fo = 2
    comment = "Transition Zone"
    disc = 1
    Call gprem(depth, delS, delP, delrho, Vs, Vp, rho, disc)
    Vs1 = Vs; Vp1 = Vp; rho1 = rho
ELSEIF ((r > 6151_8).AND.(r < 6346.6_8)) THEN
    fo = 2
    comment = "Upper Mantle: &
    & Effective Isotropic Velocities Approximated"
    disc = 1
    Call gprem(depth, delS, delP, delrho, Vs, Vp, rho, disc)
    Vs1 = Vs; Vp1 = Vp; rho1 = rho
ELSEIF ((r > 6346.6_8).AND.(r < 6368_8)) THEN
    fo = 2
    comment = "Crust"
    disc = 1
    Call gprem(depth, delS, delP, delrho, Vs, Vp, rho, disc)
    Vs1 = Vs; Vp1 = Vp; rho1 = rho
ELSEIF ((r > 6368_8).AND.(r <= 6371_8)) THEN
    fo = 2
    comment = "PREM Ocean Layer"
    disc = 1
    Call gprem(depth, delS, delP, delrho, Vs, Vp, rho, disc)
    Vs1 = Vs; Vp1 = Vp; rho1 = rho
ENDIF


IF (fo == 1) THEN
    PRINT *, "******************************************************************"
    PRINT *, " "
    PRINT *, "First-Order Discontinuity in PREM"
    PRINT *, comment
    PRINT *, " "
    PRINT *, "                del Vp (%)      del Vs (%)        del Rho (%)"
    PRINT *, "                __________      __________        ___________"
    PRINT *, " "
    PRINT 102, delP, delS, delrho
    PRINT *, " "
    PRINT *, " "
    PRINT *, "  Depth (km) 	Vp (km/sec)	Vs (km/sec)	Density (g/cm^3)"
    PRINT *, "  __________	___________	___________	________________"
    PRINT *, " "
    PRINT 101, depth, Vp2, Vs2, rho2
    PRINT 101, depth, Vp1, Vs1, rho1
    PRINT *, " "
    PRINT *, "******************************************************************"
ELSEIF (fo == 2) THEN
    PRINT *, "******************************************************************"
    PRINT *, " "
    PRINT *, comment
    PRINT *, " "
    PRINT *, "                del Vp (%)      del Vs (%)        del Rho (%)"
    PRINT *, "                __________      __________        ___________"
    PRINT *, " "
    PRINT 102, delP, delS, delrho
    PRINT *, " "
    PRINT *, " "
    PRINT *, "  Depth (km)	Vp (km/sec)	Vs (km/sec)	Density (g/cm^3)"
    PRINT *, "  __________	___________	___________	________________"
    PRINT *, " "
    PRINT 101, depth, Vp1, Vs1, rho1
    PRINT *, " "
    PRINT *, "******************************************************************"
ENDIF
101 FORMAT (4X,F8.3,2X,F13.8,3X,F13.8,5X,F13.8)
102 FORMAT (19X,F6.2,10X,F6.2,12X,F6.2)


PRINT *, " "
PRINT *, "continue or quit? (c/q)"
READ *, continue
IF (continue == 'q') EXIT
comment = " "
fo = 5
disc = 5
ENDDO
END PROGRAM grabprem
