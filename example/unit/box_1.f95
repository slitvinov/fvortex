program main
   implicit none
   interface
      include 'petros.h'
   end interface
   include 'main_dim.h'
   include 'tree9.h'
   common/vort1/xp, yp
   common/vort2/xn, yn
   integer :: i
   integer :: npart
   integer :: stat
   real :: s0
   real :: x
   real :: xmax
   real :: xmin
   real :: xn(Nvort)
   real :: xp(Nvort)
   real :: y
   real :: ymax
   real :: ymin
   real :: yn(Nvort)
   real :: yp(Nvort)

   npart = 0
   do
      read(5, *, iostat = stat) x, y
      if (stat /= 0) exit
      npart = npart + 1
      xp(npart) = x
      yp(npart) = y
   end do
   call box_dim(npart, xmin, xmax, ymin, ymax)
   s0 = max(abs(Xmax - Xmin), abs(Ymax - Ymin))

   call box_1(npart, s0, xc1, yc1, ic1, jc1, npb1, ds1, kp1, Liststart)
!   print *, xc1
!   print *, yc1
!   print *, ic1
!   print *, jc1
!   print *, npb1
!   print *, ds1
!   print *, kp1
!   print *, Liststart

   do i = 1, npart
      print *, xn(i), yn(i)
   end do

end program main
