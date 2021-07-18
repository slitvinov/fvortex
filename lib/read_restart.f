      subroutine read_restart(time, np, s2, ovrlp, nvort, xp, yp, gp)

!     Reads the files for restarting the simulation from a saved state.
!     Values for u and v are freestream velocity, thus (-) body vel.

      implicit none

      integer np, nvort
      real time, s2, ovrlp, xp(nvort), yp(nvort), gp(nvort)
!----------------------------------------------------------------------

      open (1, file='params.cont', status='old')
      read (1, *) time
      read (1, *) np
      read (1, *) s2
      read (1, *) ovrlp
      close (1)

      open (1, file='x.cont', status='old', form='unformatted')
      read (1) xp
      close (1)

      open (1, file='y.cont', status='old', form='unformatted')
      read (1) yp
      close (1)

      open (1, file='g.cont', status='old', form='unformatted')
      read (1) gp
      close (1)

      return
      end

