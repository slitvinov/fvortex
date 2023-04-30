      subroutine near_far(Nmax, ib, jb, r, ic, jc, kexam, listexam,
     $  kfar, Listfar, kclose, Listclose)

C This subroutine finds all the far & close boxes at a certain level
C associated with a certain particle.

      include 'tree_tmp.h'

      integer ib
      integer ic(nmax)
      integer jb
      integer jc(nmax)
      integer k
      integer kclose
      integer kexam
      integer kfar
      integer ks
      integer Listclose(Nhlp)
      integer listexam(Nhlp)
      integer Listfar(Nhlp)
      integer nmax
      real cr
      real fi
      real fj
      real r
      real si
      real sj

      if (kexam > nhlp) write (*, *) 'error in near_far,', kexam

      fi = r*(ib - .5) + .5
      fj = r*(jb - .5) + .5
      cr = .500001*(1.+r)
      kfar = 0
      kclose = 0

      do 2 k = 1, kexam
         ks = listexam(k)
         si = ic(ks)
         sj = jc(ks)
         if ((abs(fi - si) < cr) .and. (abs(fj - sj) < cr)) then
            kclose = kclose + 1
            Listclose(kclose) = ks
         else
            kfar = kfar + 1
            Listfar(kfar) = ks
         endif
    2 continue
      end
