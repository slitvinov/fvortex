      subroutine wheneq(npart, ibox, itoss, imatch, idummy, nmatch)

C This routine finds elements of an array which match some key and saves
C their indicies in a new array.

      include 'main_dim.h'

      integer npart
      integer itoss
      integer imatch
      integer nmatch
      integer ibox(nvort)
      integer idummy(nvort)

      integer i

      nmatch = 0

      do 10 i = 1, npart
         if (ibox(i) == imatch) then
            nmatch = nmatch + 1
            idummy(nmatch) = i
         endif
   10 continue
      end
