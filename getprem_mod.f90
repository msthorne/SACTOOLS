!***********************************************************************!
!
!Subroutine gprem
!	
!This subroutine returns the PREM model at a given depth
!	It also gives variations from PREM with the input
!	delS, delP, and delrho non-zero.  delS, delP, and
!	delrho, are entered into the subroutine in percentages
!	(range 0 - 100%)  
!
!	the variable disc can be entered as either a 1 or 2
!	if the depth is not conincident with a first-order
!	discontinuity in prem, then the variable disc can
!	take on any value.  If the depth entered into the
!	subroutine is conicident with a discontinuity then
!	if the varivable disc = 1: the subroutine will return
!	PREM for the deeper layer.  If the varibale disc = 2
!	the subroutine will return PREM for the more shallow
!	layer in depth.
!
!Calling Subroutine:
!
!Call gprem(depth, delS, delP, delrho, Vs, Vp, rho, disc)
!
!Where: depth = depth in the earth in (km)    [input]
!       delS = S-wave velocity perturbation from PREM in (%)  [input]
!       delP = P-wave velocity perturbation from PREM in (%)  [input]
!       delrho = Density perturbation from PREM in (%)  [input]
!       Vs = S-wave velocity (km/sec) [output]
!       Vp = P-wave velocity (km/sec) [output]
!       rho = Density (g/cm^3)  [output]
!       disc = top or bottom of discontinuity as above [input]
!
!Potential Problems:  I declared all real variable as KIND TYPE = 8
!This provides excellent precision on the processor I am running,
!however not all KIND TYPES are equal among different processors.
!	 
!Author:  Michael Thorne; contact: michael.thorne@utah.edu
!Jun 27, 2002
!	 
!
!***********************************************************************!
MODULE getprem
CONTAINS
SUBROUTINE gprem(depth, delS, delP, delrho, Vs, Vp, rho, disc)
IMPLICIT NONE
REAL(KIND=8), INTENT(IN) :: depth, delS, delP, delrho
REAL(KIND=8), INTENT(OUT) :: Vs, Vp, rho
REAL(KIND=8), PARAMETER :: a=6371_8
REAL(KIND=8) :: r, x
INTEGER(KIND=4), INTENT(IN) :: disc

r = a - depth
x = r/a

!***********************************************************************!
!
IF ((r >= 0_8).AND.(r < 1221.5_8)) THEN

    rho = 13.0885_8 - (8.8381_8)*(x**2)
    Vp = 11.2622_8 - (6.3640_8)*(x**2)
    Vs = 3.6678_8 - (4.4475_8)*(x**2)

ELSEIF ((r == 1221.5_8).AND.(disc == 1)) THEN

    rho = 13.0885_8 - (8.8381_8)*(x**2)
    Vp = 11.2622_8 - (6.3640_8)*(x**2)
    Vs = 3.6678_8 - (4.4475_8)*(x**2)

ELSEIF ((r == 1221.5_8).AND.(disc == 2)) THEN

    rho = 12.5815_8 - (1.2638_8)*x - (3.6426_8)*(x**2) - (5.5281_8)*(x**3)
    Vp = 11.0487_8 - (4.0362_8)*x + (4.8023_8)*(x**2) - (13.5732_8)*(x**3)
    Vs = 0_8

ELSEIF ((r > 1221.5_8).AND.(r < 3480_8)) THEN

    rho = 12.5815_8 - (1.2638_8)*x - (3.6426_8)*(x**2) - (5.5281_8)*(x**3)
    Vp = 11.0487_8 - (4.0362_8)*x + (4.8023_8)*(x**2) - (13.5732_8)*(x**3) 
    Vs = 0_8  

ELSEIF ((r == 3480_8).AND.(disc == 1)) THEN

    rho = 12.5815_8 - (1.2638_8)*x - (3.6426_8)*(x**2) - (5.5281_8)*(x**3)
    Vp = 11.0487_8 - (4.0362_8)*x + (4.8023_8)*(x**2) - (13.5732_8)*(x**3) 
    Vs = 0_8

ELSEIF ((r == 3480_8).AND.(disc == 2)) THEN

    rho = 7.9565_8 - (6.4761_8)*x + (5.5283_8)*(x**2) - (3.0807_8)*(x**3)
    Vp = 15.3891_8 - (5.3181_8)*x + (5.5242_8)*(x**2) - (2.5514_8)*(x**3)
    Vs = 6.9254_8 + (1.4672_8)*x - (2.0834_8)*(x**2) + (0.9783_8)*(x**3)

ELSEIF ((r > 3480_8).AND.(r < 3630_8)) THEN

    rho = 7.9565_8 - (6.4761_8)*x + (5.5283_8)*(x**2) - (3.0807_8)*(x**3)
    Vp = 15.3891_8 - (5.3181_8)*x + (5.5242_8)*(x**2) - (2.5514_8)*(x**3)
    Vs = 6.9254_8 + (1.4672_8)*x - (2.0834_8)*(x**2) + (0.9783_8)*(x**3)

ELSEIF ((r == 3630_8).AND.(disc == 1)) THEN

    rho = 7.9565_8 - (6.4761_8)*x + (5.5283_8)*(x**2) - (3.0807_8)*(x**3)
    Vp = 15.3891_8 - (5.3181_8)*x + (5.5242_8)*(x**2) - (2.5514_8)*(x**3)
    Vs = 6.9254_8 + (1.4672_8)*x - (2.0834_8)*(x**2) + (0.9783_8)*(x**3)

ELSEIF ((r == 3630_8).AND.(disc == 2)) THEN

    rho = 7.9565_8 - (6.4761_8)*x + (5.5283_8)*(x**2) - (3.0807_8)*(x**3)
    Vp = 24.9520_8 - (40.4673_8)*x + (51.4832_8)*(x**2) - (26.6419_8)*(x**3)
    Vs = 11.1671_8 - (13.7818_8)*x + (17.4575_8)*(x**2) - (9.2777_8)*(x**3)

ELSEIF ((r > 3630_8).AND.(r < 5600_8)) THEN

    rho = 7.9565_8 - (6.4761_8)*x + (5.5283_8)*(x**2) - (3.0807_8)*(x**3)
    Vp = 24.9520_8 - (40.4673_8)*x + (51.4832_8)*(x**2) - (26.6419_8)*(x**3)
    Vs = 11.1671_8 - (13.7818_8)*x + (17.4575_8)*(x**2) - (9.2777_8)*(x**3)

ELSEIF ((r == 5600_8).AND.(disc == 1)) THEN

    rho = 7.9565_8 - (6.4761_8)*x + (5.5283_8)*(x**2) - (3.0807_8)*(x**3)
    Vp = 24.9520_8 - (40.4673_8)*x + (51.4832_8)*(x**2) - (26.6419_8)*(x**3)
    Vs = 11.1671_8 - (13.7818_8)*x + (17.4575_8)*(x**2) - (9.2777_8)*(x**3)

ELSEIF ((r == 5600_8).AND.(disc == 2)) THEN

    rho = 7.9565_8 - (6.4761_8)*x + (5.5283_8)*(x**2) - (3.0807_8)*(x**3)
    Vp = 29.2766_8 - (23.6027_8)*x + (5.5242_8)*(x**2) - (2.5514_8)*(x**3)
    Vs = 22.3459_8 - (17.2473_8)*x - (2.0834_8)*(x**2) + (0.9783_8)*(x**3)

ELSEIF ((r > 5600_8).AND.(r < 5701_8)) THEN

    rho = 7.9565_8 - (6.4761_8)*x + (5.5283_8)*(x**2) - (3.0807_8)*(x**3)
    Vp = 29.2766_8 - (23.6027_8)*x + (5.5242_8)*(x**2) - (2.5514_8)*(x**3)
    Vs = 22.3459_8 - (17.2473_8)*x - (2.0834_8)*(x**2) + (0.9783_8)*(x**3)

ELSEIF ((r == 5701_8).AND.(disc == 1)) THEN

    rho = 7.9565_8 - (6.4761_8)*x + (5.5283_8)*(x**2) - (3.0807_8)*(x**3)
    Vp = 29.2766_8 - (23.6027_8)*x + (5.5242_8)*(x**2) - (2.5514_8)*(x**3)
    Vs = 22.3459_8 - (17.2473_8)*x - (2.0834_8)*(x**2) + (0.9783_8)*(x**3)

ELSEIF ((r == 5701_8).AND.(disc == 2)) THEN

    rho = 5.3197_8 - (1.4836_8)*x
    Vp = 19.0957_8 - (9.8672_8)*x
    Vs = 9.9839_8 - (4.9324_8)*x

ELSEIF ((r > 5701_8).AND.(r < 5771_8)) THEN

    rho = 5.3197_8 - (1.4836_8)*x
    Vp = 19.0957_8 - (9.8672_8)*x
    Vs = 9.9839_8 - (4.9324_8)*x

ELSEIF ((r == 5771_8).AND.(disc == 1)) THEN

    rho = 5.3197_8 - (1.4836_8)*x
    Vp = 19.0957_8 - (9.8672_8)*x
    Vs = 9.9839_8 - (4.9324_8)*x

ELSEIF ((r == 5771_8).AND.(disc == 2)) THEN

    rho = 11.2494_8 - (8.0298_8)*x
    Vp = 39.7027_8 - (32.6166_8)*x
    Vs = 22.3512_8 - (18.5856_8)*x

ELSEIF ((r > 5771_8).AND.(r < 5971_8)) THEN

    rho = 11.2494_8 - (8.0298_8)*x
    Vp = 39.7027_8 - (32.6166_8)*x
    Vs = 22.3512_8 - (18.5856_8)*x

ELSEIF ((r == 5971_8).AND.(disc == 1)) THEN

    rho = 11.2494_8 - (8.0298_8)*x
    Vp = 39.7027_8 - (32.6166_8)*x
    Vs = 22.3512_8 - (18.5856_8)*x

ELSEIF ((r == 5971_8).AND.(disc == 2)) THEN

    rho = 7.1089_8 - (3.8045_8)*x
    Vp = 20.3926_8 - (12.2569_8)*x
    Vs = 8.9496_8 - (4.4597_8)*x

ELSEIF ((r > 5971_8).AND.(r < 6151_8)) THEN

    rho = 7.1089_8 - (3.8045_8)*x
    Vp = 20.3926_8 - (12.2569_8)*x
    Vs = 8.9496_8 - (4.4597_8)*x

ELSEIF ((r == 6151_8).AND.(disc == 1)) THEN

    rho = 7.1089_8 - (3.8045_8)*x
    Vp = 20.3926_8 - (12.2569_8)*x
    Vs = 8.9496_8 - (4.4597_8)*x

ELSEIF ((r == 6151_8).AND.(disc == 2)) THEN

    rho = 2.6910_8 + (0.6924_8)*x
    Vp = 4.1875_8 + (3.9382_8)*x 
    Vs = 2.1519_8 + (2.3481_8)*x

ELSEIF ((r > 6151_8).AND.(r < 6291_8)) THEN

    rho = 2.6910_8 + (0.6924_8)*x
    Vp = 4.1875_8 + (3.9382_8)*x 
    Vs = 2.1519_8 + (2.3481_8)*x

ELSEIF ((r == 6291_8).AND.(disc == 1)) THEN

    rho = 2.6910_8 + (0.6924_8)*x
    Vp = 4.1875_8 + (3.9382_8)*x 
    Vs = 2.1519_8 + (2.3481_8)*x

ELSEIF ((r == 6291_8).AND.(disc == 2)) THEN

    rho = 2.6910_8 + (0.6924_8)*x
    Vp = 4.1875_8 + (3.9382_8)*x 
    Vs = 2.1519_8 + (2.3481_8)*x

ELSEIF ((r > 6291_8).AND.(r < 6346.6_8)) THEN

    rho = 2.6910_8 + (0.6924_8)*x
    Vp = 4.1875_8 + (3.9382_8)*x 
    Vs = 2.1519_8 + (2.3481_8)*x

ELSEIF ((r == 6346.6_8).AND.(disc == 1)) THEN

    rho = 2.6910_8 + (0.6924_8)*x
    Vp = 4.1875_8 + (3.9382_8)*x 
    Vs = 2.1519_8 + (2.3481_8)*x

ELSEIF ((r == 6346.6_8).AND.(disc == 2)) THEN

    rho = 2.9_8
    Vp = 6.8_8
    Vs = 3.9_8

ELSEIF ((r > 6346.6_8).AND.(r < 6356_8)) THEN

    rho = 2.9_8
    Vp = 6.8_8
    Vs = 3.9_8

ELSEIF ((r == 6356_8).AND.(disc == 1)) THEN

    rho = 2.9_8
    Vp = 6.8_8
    Vs = 3.9_8

ELSEIF ((r == 6356_8).AND.(disc == 2)) THEN

    rho = 2.6_8
    Vp = 5.8_8
    Vs = 3.2_8

ELSEIF ((r > 6356_8).AND.(r < 6368_8)) THEN

    rho = 2.6_8
    Vp = 5.8_8
    Vs = 3.2_8

ELSEIF ((r == 6368_8).AND.(disc == 1)) THEN

    rho = 2.6_8
    Vp = 5.8_8
    Vs = 3.2_8

ELSEIF ((r == 6368_8).AND.(disc == 2)) THEN

    rho = 1.02_8
    Vp = 1.45_8
    Vs = 0_8

ELSEIF ((r > 6368_8).AND.(r <= 6371_8)) THEN

    rho = 1.02_8
    Vp = 1.45_8
    Vs = 0_8    

ENDIF


rho = rho + (rho*delrho*.01_8)
Vp = Vp + (Vp*delP*.01_8)
Vs = Vs + (Vs*delS*.01_8)

END SUBROUTINE gprem
END MODULE getprem
