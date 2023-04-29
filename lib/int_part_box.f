      subroutine int_part_box(xtest, ytest, ubox, vbox, kbox)

C This  subroutine calculates the velocities induced by boxes
C on individual particles.

      include 'tree_tmp.h'

      integer kbox
      real xtest
      real ytest
      real ubox
      real vbox

      integer ks
      integer level
      real xx
      real yy
      real r2inv
      real velr
      real veli
      real prl
      real pil
      real r1
      real f1
      real r2
      real f2
      real r3
      real f3
      real r4
      real f4
      real r5
      real f5
      real r6
      real f6
      real r7
      real f7
      real r8
      real f8
      real c0r
      real c0i
      real c1r
      real c1i
      real c2r
      real c2i
      real c3r
      real c3i
      real c4r
      real c4i
      real c5r
      real c5i
      real c6r
      real c6i
      real c7r
      real c7i
      ubox = 0.
      vbox = 0.
      do 2 ks = 1, kbox

         level = 0
         xx = Xtest - Xbox(ks)
         yy = Ytest - Ybox(ks)
         r2inv = 1.0/(xx*xx + yy*yy)
         r1 = xx*r2inv
         f1 = yy*r2inv
         c0r = f1*Prbox(ks, level)
         c0i = r1*Prbox(ks, level)

         level = 1
         prl = Prbox(ks, level)
         pil = Pibox(ks, level)
         r2 = r1*r1 - f1*f1
         f2 = r1*f1 + f1*r1
         c1r = f2*prl + r2*pil
         c1i = r2*prl - f2*pil

         level = 2
         prl = Prbox(ks, level)
         pil = Pibox(ks, level)
         r3 = r2*r1 - f2*f1
         f3 = r2*f1 + f2*r1
         c2r = f3*prl + r3*pil
         c2i = r3*prl - f3*pil

         level = 3
         prl = Prbox(ks, level)
         pil = Pibox(ks, level)
         r4 = r3*r1 - f3*f1
         f4 = r3*f1 + f3*r1
         c3r = f4*prl + r4*pil
         c3i = r4*prl - f4*pil

         level = 4
         prl = Prbox(ks, level)
         pil = Pibox(ks, level)
         r5 = r4*r1 - f4*f1
         f5 = r4*f1 + f4*r1
         c4r = f5*prl + r5*pil
         c4i = r5*prl - f5*pil

         level = 5
         prl = Prbox(ks, level)
         pil = Pibox(ks, level)
         r6 = r5*r1 - f5*f1
         f6 = r5*f1 + f5*r1
         c5r = f6*prl + r6*pil
         c5i = r6*prl - f6*pil

         level = 6
         prl = Prbox(ks, level)
         pil = Pibox(ks, level)
         r7 = r6*r1 - f6*f1
         f7 = r6*f1 + f6*r1
         c6r = f7*prl + r7*pil
         c6i = r7*prl - f7*pil

         level = 7
         prl = Prbox(ks, level)
         pil = Pibox(ks, level)
         r8 = r7*r1 - f7*f1
         f8 = r7*f1 + f7*r1
         c7r = f8*prl + r8*pil
         c7i = r8*prl - f8*pil

C Sum all the terms in the series
         velr = c0r + c1r + c2r + c3r + c4r + c5r + c6r + c7r
         veli = c0i + c1i + c2i + c3i + c4i + c5i + c6i + c7i

C Calculate the velocity induced by the group "k" on particle "i"
         ubox = ubox - velr
         vbox = vbox + veli
    2 continue
      end
