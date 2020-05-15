

                                        
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE int_part2 (gtest,xtest,ytest,upart,vpart,gpart,kpart)

C  This  subroutine calculates the velocities induced on the
C  particle located at *xtest*,*ytest*, by the particles in its
C  interaction list *XT,YT,GT*; it also does the reverse influence of
c  xtest,ytest on the particles XT,YT. Thus it is only used when xtest,
c  ytest is in a higher level box than XT,YT to keep things straight.
  
      implicit none

      include 'main_dim.h'
      include 'tree_tmp.h'
      include 'part.h'

      integer np
      real s2,ovrlp,gnu
      COMMON/PART/Np,s2,ovrlp,gnu

      real gdelta,gauss(ngauss)
      COMMON/GAUSS_TABLE/gdelta,gauss

      real visc_cutoff
      common/cutoff/visc_cutoff

      integer kpart
      real gtest,xtest,ytest,upart,vpart,gpart

      integer m,i,n
      real s2inv2,gg,xx,yy,r2,arg,C,svl,fm,fn
      real rad1,rad2,A,dyopiinv
c------------------------------------------------------------------------

      dyopiinv = 1./(8.*atan(1.))
      s2inv2 = 0.5/s2
      Upart = 0.0
      Vpart = 0.0
      gg = gtest*dyopiinv
      gpart = 0.0
      
C     FPP$PERMUTATION(IT)
      DO 4 m = 1,kpart
         xx = xtest - XT(m)
         yy = YT(m) - ytest
         r2 = xx*xx + yy*yy
         arg = r2*s2inv2
         IF(arg.LT.gauss_cut) then
            i=arg/gdelta +1
            C = gauss(i)*(1 + i*gdelta - arg) ! includes first error term
         else
            C = 0.
         endif
         SVL = (1.-C)/r2
         fm = GT(m) * SVL
          Upart = Upart + yy*fm  ! velocity on particle LP
          Vpart = Vpart + xx*fm
                                ! Calculation is made symmetric here
          n = IT(m)
          fn = gg * SVL
          UU(n) = UU(n) - yy*fn
          VV(n) = VV(n) - xx*fn  
C*          rad1 = xtest*xtest + ytest*ytest
C*          rad2 = xt(m)*xt(m) + yt(m)*yt(m)
C*          if((rad1.lt.visc_cutoff).and.(rad2.lt.visc_cutoff))then
             A = (GT(m) - gtest) * C
             gpart = gpart + A  
             Gdiff(n) = Gdiff(n) - A ! Strength of n
C*         endif
 4     CONTINUE
       
       RETURN
       END                                         
