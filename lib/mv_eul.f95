!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    SUBROUTINE  MV_EUL

!  Advance the particle positions using an Euler step
!  based on velocities calculated in CONDIFF and VEL_EXT.

    implicit none

    include 'main_dim.h'

    include 'part.h'

    integer :: n
    real :: time,dt,slip_frac
    COMMON/PARAMS/n,Time,dt,slip_frac

    integer :: np
    real :: s2,ovrlp,gnu
    COMMON/PART/Np,s2,ovrlp,gnu

    integer :: in,i
    real :: pi,const,xpi,ypi,theta,u_vel,v_vel,vel_tan
!--------------------------------------------------------------------
            
    in = 0
    pi = 4.*ATAN(1.)
    const = gnu*ovrlp**2/(pi*s2)

    DO 1 i =1,Np
        xp(i) = XN(i) + dt*uu(i)
        yp(i) = YN(i) + dt*VV(i)
        gp(i) = gn(i) + dt*const*gdiff(i)
    1 END DO

    RETURN
    END SUBROUTINE 
