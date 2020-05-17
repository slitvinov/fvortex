program go
   implicit none

   interface
      include 'petros.h'
   end interface
   include 'main_dim.h'
   include 'part.h'

   integer :: np
   real :: s2, ovrlp, gnu
   common/part/Np, s2, ovrlp, gnu

   integer :: n
   real :: time, dt, slip_frac
   common/params/n, Time, dt, slip_frac

   real :: vortlim, t1, t2
   common/rems/vortlim

   integer :: irk, npath, ivalue, istepping
   integer :: icase, ipath, idiags
   integer :: Nsteps, Nrem, Nrestart
   integer :: Nvf, Nvel, Ntree
   integer :: i_time_avg, n_avg_start, n_avg_times, n_avg_interval
   real :: Rmax, gamma_0, ell_x, ell_y, time_0, visc_rmax
   logical ::  lremesh

!---------------------------------------------------------------------------

   irk = 0
   npath = -1
   lremesh = .false.

   call input(icase, ipath, idiags, istepping, &
              Nsteps, Nrem, Nrestart, &
              Nvf, Nvel, Ntree, &
              Rmax, gamma_0, ell_x, ell_y, time_0, visc_rmax, &
              i_time_avg, n_avg_start, n_avg_times, n_avg_interval)

!---  tabulate the gaussian for use as diffusion kernel

   call gaussian

!---  old data for continuation run
   if (icase == 0) then
      call read_restart(time, np, s2, ovrlp, nvort, xp, yp, gp)

      !     -- NEW run
   else
      call initial(Rmax, gamma_0, ell_x, ell_y, time_0)
      Time = time_0
      irk = 0
      call diagnos           ! get initial impulse and circulation
   endif
   ivalue = 0
   call vort_field(ivalue)

   call condiff(Np, 0, 9999., 0) ! rebuild the interaction tree

!     call vel_error  ! absolute error in a vel. profile relative to exact

   do 1 n = 1, Nsteps

      !--   compute vortex interactions with the FAST MULTIPOLE METHOD
      call cpu_time(t1)

      if (mod(n, Ntree) == 0) then
         call condiff(Np, 1, visc_rmax, 1)
      else
         call condiff(np, 1, visc_rmax, 0)
      endif

      call cpu_time(t2)
      write (*, 103) Np, t2 - t1
      call vel_ext(time)     ! Add irrotational velocities

      !--   do pathlines if desired

      if (ipath == 0) then
         npath = npath + 1
         call pathlines(npath)
      endif

      !---  Move the particles

      if ((n == 1) .or. (lremesh)) then ! first step or first after remesh
         lremesh = .false.
         if (istepping == 2) then
            call mv_rk(visc_rmax)
            irk = 1
         else
            call mv_eul
            irk = 0
         endif
      else
         call mv_ab(irk)
         irk = 0
      endif

      Time = Time + dt
      write (*, 102) n, Time
      !--   remesh every few steps to regularize particle locations

      if (mod(n, Nrem) == 0) then
         call remesh
         lremesh = .true.
      endif

      if (idiags == 1) then
         call diagnos        ! flow momentum and circulation
      endif

      !--   save data for restart, if desired

      if (mod(n, Nrestart) == 0) then
         ivalue = n/Nrestart
         call write_restart(time, np, s2, ovrlp, nvort, xp, yp, gp)
      endif

      !--   take measurements if desired

      call condiff(Np, 0, 9999., 0) ! rebuild the interaction tree

      !     call vel_error  ! absolute error in a vel. profile relative to exact

      if (mod(n, Nvf) == 0) then
         ivalue = n/Nvf
         call vort_field(ivalue)
      endif

      !--   end of loop

1  end do

102 format(10x, ' Time Step :', i5, 6x, 'Time :', f8.4)
103 format(10x, ' Particles :', i5, 6x, 'Time :', f8.4)

   stop
end program
