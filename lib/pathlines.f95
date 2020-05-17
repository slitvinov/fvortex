subroutine pathlines(npath)

!  This subroutine advances a set of points used for pathlines in the flow.

   implicit none

   integer :: nppts
   parameter(nppts=2048)

   integer :: np
   real :: s2, ovrlp, gnu
   COMMON/PART/Np, s2, ovrlp, gnu

   integer :: n
   real :: time, dt, slip_frac
   COMMON/PARAMS/n, Time, dt, slip_frac

   integer :: ipoints
   real :: xpath(nppts), ypath(nppts), upath(nppts), vpath(nppts)
   common/pathline/ipoints, xpath, ypath, upath, vpath

   integer :: npath

   integer :: k
   real :: eps, x, y, uo, vo, u, v
   character file
!-------------------------------------------------------------------------

   file = char(npath + 49)
   eps = s2/1000.

   if (npath == 0) then
      open (unit=1, file='path.dat', status='old')
      read (1, *) ipoints
      do 1 k = 1, ipoints              ! Euler step first step
         read (1, *) x, y
         call velocity(x, y, u, v, time)
         upath(k) = u
         vpath(k) = v
         xpath(k) = x + u*dt
         ypath(k) = y + v*dt
1     END DO
      close (1)
   else
      do 2 k = 1, ipoints              ! Adams Bashforth for rest
         x = xpath(k)
         y = ypath(k)
         uo = upath(k)
         vo = vpath(k)
         call velocity(x, y, u, v, time)
         upath(k) = u
         vpath(k) = v
         xpath(k) = x + 0.5*dt*(3.*u - uo)
         ypath(k) = y + 0.5*dt*(3.*v - vo)
2     END DO
   endif

   open (1, file='path'//file//'.dat', status='new')
   write (1, 101) time, ipoints
   do 3 k = 1, ipoints
      write (1, 100) xpath(k), ypath(k)
3  END DO
   close (1)

100 format(f10.6, 2x, f10.6)
101 format(f8.4, 2x, i6)

   return
end subroutine pathlines
