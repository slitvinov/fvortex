      subroutine write_restart(time, np, s2, ovrlp, nvort, xp, yp, gp)

C     Write the files needed to restart the simulation.
C     Values for u and v are freestream velocity, thus (-) body vel.

      integer  np, nvort
      real time, s2, ovrlp, xp(nvort), yp(nvort), gp(nvort)
      open (1, file='params.cont', status='replace')
      write (1, *) time
      write (1, *) np
      write (1, *) s2
      write (1, *) ovrlp
      close (1)

      open (1, file='x.cont', status='replace', form='unformatted')
      write (1) xp
      close (1)

      open (1, file='y.cont', status='replace', form='unformatted')
      write (1) yp
      close (1)

      open (1, file='g.cont', status='replace', form='unformatted')
      write (1) gp
      close (1)

      return
      end

