      subroutine check_box(Nmax1, kclose, Listclose, kexam, listexam,
     $     kpart, Listpart, ipar1Ch2, imark1)

C This  subroutine examines the nearby boxes of a certain particle.
C If they are parents then their children are placed in the list
C *listexam* so that they are eaxmined at the lower level.
C If they are childless then their particles are placed in list
C *Listpart* so that they interact directly with the particle.

      include 'tree_tmp.h'

      integer nmax1
      integer kclose
      integer kexam
      integer kpart
      integer Listclose(Nhlp)
      integer listexam(Nhlp)
      integer Listpart(Nhlp)
      integer ipar1Ch2(Nmax1,4)
      integer imark1(Nmax1)

      integer k
      integer ks
      integer i
      integer m
      integer kcheck
      integer icheck4(Nhlp)

      if (kclose > nhlp) write (*, *) 'error in check_box,', kclose
      kexam = 0
      kpart = 0
      kcheck = 0

      do 2 k = 1, kclose
         ks = Listclose(k)
         if (imark1(ks) == 0) then ! childless
            kpart = kpart + 1
            Listpart(kpart) = ks
         else
            kcheck = kcheck + 1
            icheck4(kcheck) = ks
         endif
    2 continue

      do 3 i = 1, 4
         do 30 k = 1, Kcheck
            ks = icheck4(k)
            m = ipar1Ch2(ks, i)
            if (m /= 0) then    ! box not empty, examine at lower
               kexam = kexam + 1
               listexam(kexam) = m
            endif
   30    continue
    3 continue
      end
