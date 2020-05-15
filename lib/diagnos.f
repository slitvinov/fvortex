C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        SUBROUTINE  DIAGNOS

C  Calculates the linear/angular impulse and circulation of the flow.
c  Differentiation of the impulse will give drag and lift.

        implicit none

        include 'main_dim.h'

        include 'part.h'

        integer n
        real Time,dt,slip_frac
        COMMON/PARAMS/n,Time,dt,slip_frac

        integer np
        real s2,ovrlp,gnu
        COMMON/PART/Np,s2,ovrlp,gnu

        integer i
        real pi,circ,xmom,ymom,g,x_acc,y_acc,w_acc,ang,x,y
C-----------------------------------------------------

          pi = 4.*atan(1.)
      circ = 0.
      xmom = 0.
          ymom = 0.
          ang = 0.
      DO 2 i = 1, Np
            x = xp(i)
            y = yp(i)
            g = gp(i)
            circ = circ + g
            xmom = xmom - g*y
            ymom = ymom + g*x
            ang = ang + 0.5*(x*x+y*y+s2)*g
        write(21,*)x,y
2     CONTINUE
       CLOSE(21)

           WRITE(10,100)Time,xmom
           WRITE(11,100)Time,ymom
           WRITE(12,100)Time,Circ

100    FORMAT(f10.4,2x,e13.6)

        RETURN
        END
