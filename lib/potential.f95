subroutine potential(Xbc, Ybc, potp, potb)

!  This subroutine finds the value of the velocity potential at
!  (xbc,ybc) induced by the vorticity field and returns the contribution
!  from the particles (potp) and boxes (potb).

   implicit none

   include 'tree_tmp.h'

   real :: xbc, ybc, potp, potb

   integer :: nn, kfp
!--------------------------------------------------------

   call build_tree(1, xbc, ybc, nn, kfp)

   call pot_part(xbc, ybc, nn, potp)
   if (nn > np_max) then
      write (*, *) 'error in pot_part, nn=', nn
      stop
   endif

   call pot_box(xbc, ybc, kfp, potb)
   if (kfp > nbox_max) then
      write (*, *) 'error in pot_box, kfp=', kfp
      stop
   endif

900 return
end subroutine potential
