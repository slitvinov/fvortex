c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        SUBROUTINE  REMESH

C   This subroutine remeshes the vortex field onto a uniform Cartesian
c   grid.

        implicit none

	    include 'remesh_cartesian.h'
	    include 'main_dim.h'
	    include 'part.h'

        integer np
        real s2,ovrlp,gnu
        COMMON/PART/Np,s2,ovrlp,gnu

        integer n
        real time,dt,slip_frac
	    COMMON/PARAMS/n,Time,dt,slip_frac

        real vortlim
        COMMON/REMS/vortlim

        integer nx_r,nx_l,ny_t,ny_b,ig,ix,iy,i,in,ifar,iback
        integer ix0,ix1,ix2,iy0,iy1,iy2,nmesh
        integer indx(nx_min:nx_max,ny_min:ny_max)
        integer k00,k10,k20,k01,k11,k21,k02,k12,k22
        real pi,twopi,twopiinv,dh,circ
        real xmax,xmin,ymax,ymin,xr,xl,yt,yb,dhhaf,xx,yy,cold,cx
        real cutoff,cut_far,dhinv,g,x,y,ndist,sdist
        real u,v,fy0,fy1,fy2,fx0,fx1,fx2,ag,cnew
        real xg(ngrid),yg(ngrid),gg(ngrid)

c----------------------------------------------------------------

        pi = 4.0*ATAN(1.0)
        twopi = 2.*pi
        twopiinv = 1./twopi
        dh = 2. * ovrlp * sqrt(s2/pi)
        dhinv = 1./dh

c--- FIRST ESTABLISH THE NEW GRID TO BE MAPPED INTO

c--- Find the edge of the grid of particles

        call box_dim(np,xmin,xmax,ymin,ymax)
        xr = xmax + 5.*dh
        xl = xmin - 5.*dh
        yt = ymax + 5.*dh
        yb = ymin - 5.*dh
        Nx_r = NINT(Xr / dh)
        Nx_l = NINT(Xl / dh)
        Ny_t = NINT(Yt / dh)
        Ny_b = NINT(Yb / dh) - 1

        WRITE(*,*)'Nx_l,Nx_r', Nx_l,Nx_r
        write(*,*)'Ny_t,Ny_b',ny_t,ny_b
        IF(Nx_r.GT.NX_max) THEN
		  write(*,*) 'PROBLEM :Nx_right =',Nx_r, ' Nx_max = ', Nx_max
		END IF 
        IF(Nx_l.LT.NX_min) THEN
		   write(*,*) 'PROBLEM :Nx_left =', Nx_l,' Nx_min=',Nx_min
		END IF
        IF(Ny_t.GT.Ny_max)THEN
		   write(*,*)'PROBLEM :Ny_top =',Ny_t, 'Ny_max=',Ny_max
		END IF
        IF(Ny_b.lT.Ny_min)THEN
		   write(*,*)'PROBLEM :Ny_bottom =', Ny_b, 'Ny_min=',Ny_min
		END IF

        IF((Nx_r.GT.NX_max).OR.(Nx_l.LT.NX_min).OR. 
     &     (Ny_t.GT.Ny_max).OR.(Ny_b.lT.Ny_min)) THEN
	      STOP
	    END IF

c--- Establish the new grid for the remeshed field

         dhhaf = 0.5 * dh
         ig = 0
         DO 10 ix = Nx_l,Nx_r,1
           xx = dhhaf + ix * dh
           DO 11 iy = Ny_b,Ny_t,1
              yy =  dhhaf + iy * dh
              ig = ig + 1
              XG(ig) = xx
              YG(ig) = yy
              GG(ig) = 0.0
              INDX(ix,iy) = ig          ! We can avoid this big array
11         CONTINUE
10       CONTINUE

         Nmesh = ig

         WRITE(*,*)'Nmesh = ', Nmesh
         IF(nmesh.gt.ngrid) then ! overran array dimensions
            write(*,*)'nmesh too large in remesh, stopping'
            stop
         endif

c--- check diagnostics, pre-remesh - they should be conserved through remesh

        cold = 0.0
        cx = 0.0
        DO 71 i =1,Np
         cold = cold + GP(i)		! total circulation
         cx  = cx + GP(i)*YP(i)	! x-impulse
71      CONTINUE
        WRITE(*,*)'pre-remesh, circulation:',cold,'  x-impulse: ',cx

c-- set cutoff values to throw out particles

        cutoff=vortlim*s2*ovrlp**2
        cut_far = 10. * cutoff
c--- NEW GRID IN PLACE, REMESH OLD FIELD NOW

        in = 0
        ifar = 0
        DO 40 i=1,Np   ! loop through, mapping particles to mesh
          g = GP(i)
          x = XP(i)
          y = YP(i)
          ndist = y
          sdist = x
          ix = NINT(sdist * dhinv - 0.5)
          iy = NINT(ndist * dhinv - 0.5)

c ---- Do not remesh the particles outside the established grid

          IF ((ix.GT.Nx_r).or.(ix.lt.nx_l).or.(iy.gt.ny_t).or.
     &        (iy.lt.ny_b)) THEN
             if(abs(g).ge.cut_far) then
                   ifar = ifar + 1
                   XP(ifar) = x
                   YP(ifar) = y
                   GP(ifar) = g
             endif

c-- all other particles are in the inner grid
          ELSE  IF((ix.eq.nx_r).or.(ix.eq.nx_l).or.
     &      (iy.eq.ny_t).or.(iy.eq.ny_b)) then
c----------------------------------------------------------------
c- Category 0_1 : PARTICLES at the FAR interface (NGP remeshing)
c----------------------------------------------------------------
           in = in + 1
           ig = INDX(ix,iy)
           GG(ig) = GG(ig) + g  ! NGP   Interpolation
         
         ELSE
c----------------------------------------------------------------
c- Category 0_1 : ALL other PARTICLES in the domain
c----------------------------------------------------------------
            in = in + 1
            ix0 = ix
            ix1 = ix - 1
            ix2 = ix + 1
            iy0 = iy
            iy1 = iy - 1
            iy2 = iy + 1
            k00 = INDX(ix0,iy0)
            k10 = INDX(ix1,iy0)
            k20 = INDX(ix2,iy0)
            k01 = INDX(ix0,iy1)
            k11 = INDX(ix1,iy1)
            k21 = INDX(ix2,iy1)
            k02 = INDX(ix0,iy2)
            k12 = INDX(ix1,iy2)
            k22 = INDX(ix2,iy2)
            u = (sdist - XG(k00)) * dhinv
            v = (ndist - YG(k00)) * dhinv
            Fy0 = 1.0 - v*v
            Fy1 = 0.5 * v * (v-1.)
            Fy2 = 0.5 * v * (v+1.)
            Fx0 = g*(1. - u*u)
            Fx1 = g*(0.5 * u * (u-1.))
            Fx2 = g*(0.5 * u * (u+1.))
            GG(k00) = GG(k00) + Fx0 * Fy0
            GG(k01) = GG(k01) + Fx0 * Fy1
            GG(k02) = GG(k02) + Fx0 * Fy2
            GG(k10) = GG(k10) + Fx1 * Fy0
            GG(k11) = GG(k11) + Fx1 * Fy1
            GG(k12) = GG(k12) + Fx1 * Fy2
            GG(k20) = GG(k20) + Fx2 * Fy0
            GG(k21) = GG(k21) + Fx2 * Fy1
            GG(k22) = GG(k22) + Fx2 * Fy2
          ENDIF
40      CONTINUE

c--- put remeshed particles into arrays, using cutoffs determined earlier

        iback=ifar
        DO 29 i=1,Nmesh
          g = gg(i)
          ag = ABS(g)
c-- only cutoff if below threshold AND away from domain center
          IF((ag.GT.cutoff).OR.((abs(yg(i))*abs(yg(i)) +
     &       abs(xg(i))*abs(xg(i))).lt.1.) ) then
             iback = iback + 1
             XP(iback) = xg(i)
             YP(iback) = yg(i)
             GP(iback) = g
          endif
29      CONTINUE

c---  check diagnostics

	    Np = iback
        cnew = 0.0
        cx = 0.0
        DO 72 i = 1,Np
	     circ = gp(i)
         cnew = cnew + circ
         cx  = cx + circ*YP(i)
72      CONTINUE
        WRITE(*,*)'post-remesh, circulation:',cnew,'  x-impulse: ',cx

        WRITE(*,89)Np,iback-ifar,ifar

89     FORMAT(3x,'New Total :',I8,3x,'INSIDE :',I8,2x,'OUTSIDE :',I8,2x)

        RETURN
        END



