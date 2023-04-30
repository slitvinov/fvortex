      program main

      include 'main_dim.h'
      include 'part.h'

      integer i
      integer iframe
      integer np
      real dt
      real gnu
      real ovrlp
      real s2
      real time

      common/params/n, time, dt
      common/part/np, s2, ovrlp, gnu

      iframe = 0
      time = 0.0
      s2 = 1e-4
      np = 10
      do 10 i = 1, np
         xp(i) = real(i)
         yp(i) = 10 * real(i)
         gp(i) = 100 * real(i)
 10   continue
      call diagnos(iframe)

      end
