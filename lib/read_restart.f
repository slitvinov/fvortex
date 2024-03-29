      subroutine read_restart(time, np, s2, ovrlp, nvort, xp, yp, gp)

C Reads the files for restarting the simulation from a saved state.
C Values for u and v are freestream velocity, thus (-) body vel.

      integer np
      integer nvort
      real time
      real s2
      real ovrlp
      real xp(nvort)
      real yp(nvort)
      real gp(nvort)

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
      end

