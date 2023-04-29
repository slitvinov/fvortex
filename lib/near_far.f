      subroutine near_far(Nmax, ib, jb, r, ic, jc, kexam, listexam,
     $  kfar, Listfar, kclose, Listclose)

C This  subroutine finds all the far & close boxes at a certain
C level associated with a certain particle.

      include 'tree_tmp.h'

      integer nmax
      integer ib
      integer jb
      integer ic(nmax)
      integer jc(nmax)
      integer kexam
      integer kfar
      integer kclose
      integer Listclose(Nhlp)
      integer listexam(Nhlp)
      integer Listfar(Nhlp)
      real r

      integer k
      integer ks
      real fi
      real fj
      real cr
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
            Listclose(kclose) = ks ! close
         else
            kfar = kfar + 1
            Listfar(kfar) = ks  ! far away
         endif
    2 continue
      end
