      program main
      include 'main_dim.h'
      common/vort1/xp, yp
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

      npart = 0
      do
         read(5, *, iostat = stat) x, y
         if (stat /= 0) exit
         npart = npart + 1
         xp(npart) = x
         yp(npart) = y
      continue
      call box_dim(npart, xmin, xmax, ymin, ymax)
      print *, xmin, ymin
      print *, xmax, ymin
      print *, xmax, ymax
      print *, xmin, ymax
      print *, xmin, ymin
      end program main
