C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE vel_part(xtest,ytest,upart,vpart,kpart)

C  This  subroutine calculates the velocities induced on the
C  particle located at *xtest*,*ytest*, by the particles in its
C  interaction list *XT,YT,GT*.

      implicit none

      include 'tree_tmp.h'

      integer np
      real s2,ovrlp,gnu
      COMMON/PART/Np,s2,ovrlp,gnu

      integer kpart
      real xtest,ytest,upart,vpart

      integer m
      real eps,xx,yy,r2,fc,s2inv2
c------------------------------------------------------------------------------

c Compute now the direct particle-particle interactions

      Upart = 0.0
      Vpart = 0.0
      eps = 0.000001
      s2inv2 = 0.5/s2
      
      DO 4 m = 1,kpart
         xx = xtest - XT(m)
         yy = YT(m) - ytest
         r2 = xx*xx + yy*yy
         fc = GT(m) * (1.-exp(-r2*s2inv2))/(r2+eps)
         Upart = Upart + yy*fc
         Vpart = Vpart + xx*fc
 4    CONTINUE
      
      RETURN
      END
