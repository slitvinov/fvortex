C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        SUBROUTINE  MV_AB(irk)

C  Advance the particle positions using an Adams Bashforth scheme
C  based on velocities calculated in CONDIFF and VEL_EXT.

        implicit none

        include 'main_dim.h'

        include 'part.h'

        integer n
        real time,dt,slip_frac
        COMMON/PARAMS/n,Time,dt,slip_frac

        integer np
        real s2,ovrlp,gnu
        COMMON/PART/Np,s2,ovrlp,gnu

        integer irk

        integer in,i
        real pi,const,xpi,ypi,theta,u_vel,v_vel,vel_tan,c1,c2
C--------------------------------------------------------------------
        
        in = 0
        pi = 4.*ATAN(1.)
        const = gnu*ovrlp**2/(pi*s2)

        if (irk.eq.0) then
          c1=1.5
          c2=0.5
        else
          c1=2.
          c2=1.
        endif
        DO i =1,Np
          xp(i) = XN(i) + dt*( c1*UU(i) - c2*Uold(i) )
          yp(i) = YN(i) + dt*( c1*VV(i) - c2*Vold(i) )

          gp(i) = gn(i) + dt*const*( c1*gdiff(i) - c2*gdold(i) ) 
        ENDDO

        RETURN
        END
