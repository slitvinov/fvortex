c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        PROGRAM  GO     

C       VORTICITY IN UNBOUNDED DOMAIN
C  This program solves the 2D incompressible viscous vorticity equations
C  using Lagrangian vortex particles 

        implicit none

        include 'main_dim.h'
        include 'part.h'

        integer np
        real s2,ovrlp,gnu
        COMMON/PART/Np,s2,ovrlp,gnu

        integer n
        real time,dt,slip_frac
        COMMON/PARAMS/n,Time,dt,slip_frac

        real vortlim, t1,t2
        COMMON/REMS/vortlim
		
        integer irk,npath,i,ivalue,istepping
        integer icase,ipath,idiags  
		integer Nsteps,Nrem,Nfilter,Nrestart
        integer Nvf,Nvel,Ntree
        integer i_time_avg,n_avg_start,n_avg_times,n_avg_interval
        real Rmax,gamma_0,ell_x,ell_y,time_0,visc_rmax
        LOGICAL  LREMESH

c---------------------------------------------------------------------------

        irk=0
        npath = -1
        LREMESH = .FALSE.

c--- get computational parameters

        CALL INPUT(icase,ipath,idiags,istepping,
     &     Nsteps,Nrem,Nrestart,
     &     Nvf,Nvel,Ntree,
     &     Rmax,gamma_0,ell_x,ell_y,time_0,visc_rmax,
     &     i_time_avg,n_avg_start,n_avg_times,n_avg_interval)

c--- tabulate the gaussian for use as diffusion kernel

        CALL GAUSSIAN

c--- old data for continuation run
        IF(icase.EQ.0)THEN
           CALL READ_RESTART(time,np,s2,ovrlp,nvort,xp,yp,gp)

c -- NEW run
        ELSE
           CALL INITIAL(Rmax,gamma_0,ell_x,ell_y,time_0)    
           Time = time_0
           irk = 0
           CALL DIAGNOS         ! get initial impulse and circulation
        ENDIF
        ivalue = 0
	    call VORT_FIELD(ivalue)

	    CALL CONDIFF(Np,0,9999.,0)  ! rebuild the interaction tree

C   call vel_error  ! absolute error in a vel. profile relative to exact

c***************************************************************************
c                       MAIN LOOP
c***************************************************************************

        DO 1 n=1,Nsteps 

c-- compute vortex interactions with the FAST MULTIPOLE METHOD
		  CALL CPU_TIME(t1)
		  
          IF (MOD(n,Ntree).EQ.0) THEN
            CALL CONDIFF(Np,1,visc_rmax,1)
          else
            call condiff(np,1,visc_rmax,0)
          endif
		  
	      CALL CPU_TIME(t2)
		  WRITE(*,103)Np,t2-t1
          CALL VEL_EXT(time)     ! Add irrotational velocities

c--     do pathlines if desired

          if(ipath.eq.0)then 
            npath = npath + 1
            call pathlines(npath)
          endif

c--- Move the particles

          IF((n.EQ.1).OR.(LREMESH))THEN    ! first step or first after remesh
            LREMESH = .FALSE.
            if(istepping.eq.2) then
              CALL MV_RK(visc_rmax)
              irk=1
            else
              call mv_eul
              irk=0
            endif      
          ELSE                 
            CALL MV_AB(irk)
            irk=0
          ENDIF


          Time=Time+dt
          WRITE(*,102)n,Time
c-- remesh every few steps to regularize particle locations

          IF (MOD(n,Nrem).EQ.0) THEN
            CALL REMESH
            LREMESH = .TRUE.
         ENDIF

          if (idiags.EQ.1) then
             CALL DIAGNOS               ! flow momentum and circulation
          endif

c-- save data for restart, if desired

          if(mod(n,Nrestart).eq.0) then
             ivalue=n/Nrestart
             call write_restart(ivalue,time,np,s2,ovrlp,nvort,xp,yp,gp)
          endif

c-- take measurements if desired

	  CALL CONDIFF(Np,0,9999.,0)  ! rebuild the interaction tree

c	  call vel_error  ! absolute error in a vel. profile relative to exact

          if(mod(n,Nvf).eq.0) then
             ivalue=n/Nvf
             call VORT_FIELD(ivalue)
          endif

c-- end of loop

1	CONTINUE

c***************************************************************************
c			END OF LOOP
c***************************************************************************

101     FORMAT(f8.4,5(2x,f8.4))
102     FORMAT(1H+,10x,' Time Step :',I5,6x,'Time :',F8.4)
103     FORMAT(1H+,10x,' Particles :',I5,6x,'Time :',F8.4)


	STOP
	END