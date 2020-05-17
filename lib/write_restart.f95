subroutine write_restart(ivalue, time, np, s2, ovrlp, &
                         nvort, xp, yp, gp)

!  Write the files needed to restart the simulation.
!  Values for u and v are freestream velocity, thus (-) body vel.

   implicit none

   integer :: ivalue, np, nvort
   real :: time, s2, ovrlp, xp(nvort), yp(nvort), gp(nvort)
   character value
!----------------------------------------------------------------------

   value = char(ivalue + 48)

   open (1, file='params'//value//'.cont', status='replace')
   write (1, *) time
   write (1, *) np
   write (1, *) s2
   write (1, *) ovrlp
   close (1)

   open (1, file='x'//value//'.cont', status='new', form='unformatted')
   write (1) xp
   close (1)

   open (1, file='y'//value//'.cont', status='new', form='unformatted')
   write (1) yp
   close (1)

   open (1, file='g'//value//'.cont', status='new', form='unformatted')
   write (1) gp
   close (1)

   return
end subroutine write_restart

