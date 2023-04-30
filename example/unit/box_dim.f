      program main
      include 'main_dim.h'
      integer npart
      integer stat
      real x
      real xmax
      real xmin
      real xp(nvort)
      real y
      real ymax
      real ymin
      real yp(nvort)
      common/vort1/xp, yp

      npart = 0
      do 1
         read(5, *, iostat = stat) x, y
         if (stat /= 0) exit
         npart = npart + 1
         xp(npart) = x
         yp(npart) = y
 1    continue
      call box_dim(npart, xmin, xmax, ymin, ymax)
      print *, xmin, ymin
      print *, xmax, ymin
      print *, xmax, ymax
      print *, xmin, ymax
      print *, xmin, ymin
      end program main
