c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        subroutine velocity (x,y,u,v,tme)

c   This subroutine finds the velocity at point x,y.

        implicit none

	include 'main_dim.h'

        integer np
        real s2,ovrlp,gnu
        COMMON/PART/Np,s2,ovrlp,gnu

        real x,y,u,v,tme

        real pi,twopiinv,alpha,x_vel,y_vel,ui,vi,r_2

        real velocity_x,velocity_y,omega
c------------------------------------------------------------------------

        pi = 4.0 * ATAN(1.)
        twopiinv = 0.5/pi
        alpha=OMEGA(Tme)
        x_vel = velocity_x(tme)
        y_vel = velocity_y(tme)

	    call vel_tree(x,y,ui,vi)

        r_2 = x**2 + y**2
        u = ui*twopiinv + x_vel - y*alpha/r_2
        v = vi*twopiinv + y_vel + x*alpha/r_2

        return
        end
