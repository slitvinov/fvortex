      subroutine int_part2(gtest, xtest, ytest, upart, vpart, gpart,
     $     kpart)

C This  subroutine calculates the velocities induced on the
C particle located at *xtest*,*ytest*, by the particles in its
C interaction list *XT,YT,GT*; it also does the reverse influence of
C xtest,ytest on the particles XT,YT. Thus it is only used when xtest,
C ytest is in a higher level box than XT,YT to keep things straight.

      include 'main_dim.h'
      include 'tree_tmp.h'
      include 'part.h'

      integer np
      real s2
      real ovrlp
      real gnu
      common/part/Np, s2, ovrlp, gnu

      real gdelta
      real gauss(ngauss)
      common/gauss_table/gdelta, gauss

      real visc_cutoff
      common/cutoff/visc_cutoff

      integer kpart
      real gtest
      real xtest
      real ytest
      real upart
      real vpart
      real gpart

      integer m
      integer i
      integer n
      real s2inv2
      real gg
      real xx
      real yy
      real r2
      real arg
      real c
      real svl
      real fm
      real fn
      real a
      real dyopiinv

      dyopiinv = 1./(8.*atan(1.))
      s2inv2 = 0.5/s2
      Upart = 0.0
      Vpart = 0.0
      gg = gtest*dyopiinv
      gpart = 0.0

C FPP$PERMUTATION(IT)
      do 4 m = 1, kpart
         xx = xtest - xt(m)
         yy = yt(m) - ytest
         r2 = xx*xx + yy*yy
         arg = r2*s2inv2
         if (arg < gauss_cut) then
            i = int(arg/gdelta + 1)
            c = gauss(i)*(1 + i*gdelta - arg)

         else
            c = 0.
         endif
         svl = (1.-c)/r2
         fm = gt(m)*svl
         Upart = Upart + yy*fm
         Vpart = Vpart + xx*fm
C Calculation is made symmetric here
         n = it(m)
         fn = gg*svl
         uu(n) = uu(n) - yy*fn
         vv(n) = vv(n) - xx*fn
C           rad1 = xtest*xtest + ytest*ytest
C           rad2 = xt(m)*xt(m) + yt(m)*yt(m)
C           if((rad1.lt.visc_cutoff).and.(rad2.lt.visc_cutoff))then
         a = (gt(m) - gtest)*c
         gpart = gpart + a
         Gdiff(n) = Gdiff(n) - a
C          endif
    4 continue
      end
