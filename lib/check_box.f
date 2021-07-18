      subroutine check_box(Nmax1, kclose, Listclose, kexam, listexam,
     &     kpart, Listpart, ipar1Ch2, imark1)

!     This  subroutine examines the nearby boxes of a certain particle.
!     If they are parents then their children are placed in the list
!     *listexam* so that they are eaxmined at the lower level.
!     If they are childless then their particles are placed in list
!     *Listpart* so that they interact directly with the particle.

      implicit none

      include 'tree_tmp.h'

      integer :: nmax1, kclose, kexam, kpart
      integer :: Listclose(Nhlp), listexam(Nhlp), Listpart(Nhlp)
      integer :: ipar1Ch2(Nmax1, 4), imark1(Nmax1)

      integer :: k, ks, i, m, kcheck
      integer :: icheck4(Nhlp)
!------------------------------------------------------

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
 2    end do

      do 3 i = 1, 4
         do 30 k = 1, Kcheck
            ks = icheck4(k)
            m = ipar1Ch2(ks, i)
            if (m /= 0) then    ! box not empty, examine at lower
               kexam = kexam + 1
               listexam(kexam) = m
            endif
 30      end do
 3    end do

      return
      end subroutine check_box
