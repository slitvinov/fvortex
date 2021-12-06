      subroutine wheneq(npart, ibox, itoss, imatch, idummy, nmatch)

!     This routine finds elements of an array which match some key and saves
!     their indicies in a new array.

      implicit none

      include 'main_dim.h'

      integer npart, itoss, imatch, nmatch
      integer ibox(nvort), idummy(nvort)

      integer i
!----------------------------------------------------------

      nmatch = 0

      do 10 i = 1, npart
         if (ibox(i) == imatch) then
            nmatch = nmatch + 1
            idummy(nmatch) = i
         endif
   10 end do

      return
      end
