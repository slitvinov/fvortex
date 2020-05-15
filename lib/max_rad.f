c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        SUBROUTINE MAX_RAD (Np,rmax)

c  This subroutine finds the distance of the particle farthest from (0,0).

        implicit none

        include 'main_dim.h'
        include 'part.h'

        integer np
        real rmax

        integer i
        real x,y,r
c----------------------------------------------------------------------

        x = XP(1)
        y = YP(1)
        rmax = SQRT( x*x + y*y )
        DO 11 i=2,Np
          x=XP(i)
          y=YP(i)
          r = SQRT( x*x + y*y )
          rmax = AMAX1(rmax,r)
11      CONTINUE

        RETURN
        END
