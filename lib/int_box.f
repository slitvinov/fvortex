      subroutine int_box(Nmax, id, xtest, ytest, kbb, Br, Bi)

C     This subroutine computes BOX-BOX interactions.


      include 'tree_tmp.h'

      integer nmax, id, kbb
      real xtest, ytest
      real Br(nmax, 7), Bi(Nmax, 7)

      integer ni, level
      real br0, bi0, br1, bi1, br2, bi2, br3, bi3, br4, bi4
      real br5, bi5, br6, bi6, br7, bi7
      real s1, t1, s2, t2, s3, t3, s4, t4
      real s5, t5, s6, t6, s7, t7
      real prl, pil
      real cr0, ci0, cr1, ci1, cr2, ci2, cr3, ci3, cr4, ci4
      real cr5, ci5, cr6, ci6, cr7, ci7
      real a, b, xx, yy, r2inv
C-----------------------------------------------------------

      br0 = 0.
      bi0 = 0.
      br1 = 0.
      bi1 = 0.
      br2 = 0.
      bi2 = 0.
      br3 = 0.
      bi3 = 0.
      br4 = 0.
      bi4 = 0.
      br5 = 0.
      bi5 = 0.
      br6 = 0.
      bi6 = 0.
      br7 = 0.
      bi7 = 0.

      do 2 ni = 1, kbb
         level = 0
         xx = Xtest - Xbox(ni)
         yy = Ytest - Ybox(ni)
         r2inv = 1.0/(xx*xx + yy*yy)
         s1 = xx*r2inv
         t1 = yy*r2inv
         prl = Prbox(ni, 0)
         pil = Pibox(ni, 0)
         cr0 = prl
         ci0 = pil

         level = 1
         prl = Prbox(ni, level)
         pil = Pibox(ni, level)
         cr1 = s1*prl - t1*pil
         ci1 = s1*pil + t1*prl

         level = 2
         prl = Prbox(ni, level)
         pil = Pibox(ni, level)
         s2 = s1*s1 - t1*t1
         t2 = s1*t1 + t1*s1
         cr2 = s2*prl - t2*pil
         ci2 = s2*pil + t2*prl

         level = 3
         prl = Prbox(ni, level)
         pil = Pibox(ni, level)
         s3 = s2*s1 - t2*t1
         t3 = s2*t1 + t2*s1
         cr3 = s3*prl - t3*pil
         ci3 = s3*pil + t3*prl

         level = 4
         prl = Prbox(ni, level)
         pil = Pibox(ni, level)
         s4 = s3*s1 - t3*t1
         t4 = s3*t1 + t3*s1
         cr4 = s4*prl - t4*pil
         ci4 = s4*pil + t4*prl

         level = 5
         prl = Prbox(ni, level)
         pil = Pibox(ni, level)
         s5 = s4*s1 - t4*t1
         t5 = s4*t1 + t4*s1
         cr5 = s5*prl - t5*pil
         ci5 = s5*pil + t5*prl

         level = 6
         prl = Prbox(ni, level)
         pil = Pibox(ni, level)
         s6 = s5*s1 - t5*t1
         t6 = s5*t1 + t5*s1
         cr6 = s6*prl - t6*pil
         ci6 = s6*pil + t6*prl

         level = 7
         prl = Prbox(ni, level)
         pil = Pibox(ni, level)
         s7 = s6*s1 - t6*t1
         t7 = s6*t1 + t6*s1
         cr7 = s7*prl - t7*pil
         ci7 = s7*pil + t7*prl

         a = cr0 + cr1 + cr2 + cr3 + cr4 + cr5 + cr6 + cr7
         b = ci0 + ci1 + ci2 + ci3 + ci4 + ci5 + ci6 + ci7
         br1 = br1 + s1*a - t1*b
         bi1 = bi1 + s1*b + t1*a

         a = cr0 + 2.*cr1 + 3.*cr2 + 4.*cr3 + 5.*cr4 + 6.*cr5
     $     + 7.*cr6 + 8.*cr7
         b = ci0 + 2.*ci1 + 3.*ci2 + 4.*ci3 + 5.*ci4 + 6.*ci5
     $     + 7.*ci6 + 8.*ci7
         br2 = br2 + s2*a - t2*b
         bi2 = bi2 + s2*b + t2*a

         a = cr0 + 3.*cr1 + 6.*cr2 + 10.*cr3 + 15.*cr4 + 21.*cr5
     $     + 28.*cr6 + 36.*cr7
         b = ci0 + 3.*ci1 + 6.*ci2 + 10.*ci3 + 15.*ci4 + 21.*ci5
     $     + 28.*ci6 + 36.*ci7
         br3 = br3 + s3*a - t3*b
         bi3 = bi3 + s3*b + t3*a

         a = cr0 + 4.*cr1 + 10.*cr2 + 20.*cr3 + 35.*cr4 + 56.*cr5
     $     + 84.*cr6 + 120.*cr7
         b = ci0 + 4.*ci1 + 10.*ci2 + 20.*ci3 + 35.*ci4 + 56.*ci5
     $     + 84.*ci6 + 120.*ci7
         br4 = br4 + s4*a - t4*b
         bi4 = bi4 + s4*b + t4*a

         a = cr0 + 5.*cr1 + 15.*cr2 + 35.*cr3 + 70.*cr4 + 126.*cr5
     $     + 210.*cr6 + 330.*cr7
         b = ci0 + 5.*ci1 + 15.*ci2 + 35.*ci3 + 70.*ci4 + 126.*ci5
     $     + 210.*ci6 + 330.*ci7
         br5 = br5 + s5*a - t5*b
         bi5 = bi5 + s5*b + t5*a

         a = cr0 + 6.*cr1 + 21.*cr2 + 56.*cr3 + 126.*cr4 + 252.*cr5
     $     + 462.*cr6 + 792.*cr7
         b = ci0 + 6.*ci1 + 21.*ci2 + 56.*ci3 + 126.*ci4 + 252.*ci5
     $     + 462.*ci6 + 792.*ci7
         br6 = br6 + s6*a - t6*b
         bi6 = bi6 + s6*b + t6*a

         a = cr0 + 7.*cr1 + 28.*cr2 + 84.*cr3 + 210.*cr4 + 462.*cr5
     $     + 924.*cr6 + 1716.*cr7
         b = ci0 + 7.*ci1 + 28.*ci2 + 84.*ci3 + 210.*ci4 + 462.*ci5
     $     + 924.*ci6 + 1716.*ci7
         br7 = br7 + s7*a - t7*b
         bi7 = bi7 + s7*b + t7*a
    2 end do

      Br(id, 1) = Br(id, 1) + br1
      Bi(id, 1) = Bi(id, 1) + bi1
      Br(id, 2) = Br(id, 2) - br2
      Bi(id, 2) = Bi(id, 2) - bi2
      Br(id, 3) = Br(id, 3) + br3
      Bi(id, 3) = Bi(id, 3) + bi3
      Br(id, 4) = Br(id, 4) - br4
      Bi(id, 4) = Bi(id, 4) - bi4
      Br(id, 5) = Br(id, 5) + br5
      Bi(id, 5) = Bi(id, 5) + bi5
      Br(id, 6) = Br(id, 6) - br6
      Bi(id, 6) = Bi(id, 6) - bi6
      Br(id, 7) = Br(id, 7) + br7
      Bi(id, 7) = Bi(id, 7) + bi7

      return
      end
