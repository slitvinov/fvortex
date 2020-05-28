program main
   implicit none
   interface
      include 'petros.h'
   end interface
   include 'main_dim.h'
   include 'tree9.h'
   common/vort1/xp, yp
   common/vort2/xn, yn
   common/geom/x0, y0
   integer :: i
   integer :: j
   integer :: npart
   integer :: stat
   real :: s0
   real :: x
   real :: x0
   real :: xmax
   real :: xmin
   real :: xn(Nvort)
   real :: xp(Nvort)
   real :: y
   real :: y0
   real :: ymax
   real :: ymin
   real :: yn(Nvort)
   real :: yp(Nvort)
   integer :: stderr
   integer :: stdin

   stderr = 0
   stdin = 5
   npart = 0
   do
      read(stdin, *, iostat = stat) x, y
      if (stat /= 0) exit
      npart = npart + 1
      xp(npart) = x
      yp(npart) = y
   end do
   call box_dim(npart, xmin, xmax, ymin, ymax)
   s0 = max(abs(Xmax - Xmin), abs(Ymax - Ymin))
   x0 = xmin - 0.01*s0 ! Coords. of lower
   y0 = ymin - 0.01*s0 ! left corner of square (origin)
   s0 = 1.02*s0

   call box_1(npart, s0, xc1, yc1, ic1, jc1, npb1, ds1, kp1, Liststart)
   write (stderr, *) 'xc = ', xc1
   write (stderr, *) 'yc = ', yc1
   write (stderr, *) 'ic = ', ic1
   write (stderr, *) 'jc = ', jc1
   write (stderr, *) 'Liststart = ', Liststart
   write (stderr, *) 'npb1(:, 1) = ', npb1(:, 1)
   write (stderr, *) 'npb1(:, 2) = ', npb1(:, 2)
   write (stderr, *) 'ds1 = ', ds1
   write (stderr, *) 'kp1 = ', kp1
   do j = 1, kp1
      do i = npb1(j, 1), npb1(j, 2)
         print *, xn(i), yn(i), j
      end do
   end do

end program main
