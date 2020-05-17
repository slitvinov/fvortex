!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    SUBROUTINE  VEL_EXT(tm)

!  This routine adds the irrotational components of the velocity field
!  (which are not represented by the vorticity field) such as a free stream.

    implicit none

    include 'main_dim.h'

    include 'part.h'

    integer :: n
    real :: time,dt,slip_frac
    COMMON/PARAMS/n,Time,dt,slip_frac

    integer :: np
    real :: s2,ovrlp,gnu
    COMMON/PART/Np,s2,ovrlp,gnu

    real :: tm
            
    integer :: i
    real :: alpha,x_vel,y_vel,xni,yni,arn2,uext,vext

    real :: velocity_x,velocity_y,omega
!----------------------------------------

    alpha = omega(tm)
    x_vel = velocity_x(tm)
    y_vel = velocity_y(tm)

    DO 1 i=1,Np
        xni = XN(i)
        yni = YN(i)
        Arn2 = alpha /(xni*xni + yni*yni)   ! from rotation
        Uext = -yni * Arn2 + x_vel
        Vext =  xni * Arn2 + y_vel
        UU(i) = UU(i) + Uext
        VV(i) = VV(i) + Vext
    1 END DO

    RETURN
    END SUBROUTINE 
