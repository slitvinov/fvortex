      SUBROUTINE  VORT_FIELD(iframe)

C     outputs the vorticity field on a grid.

      implicit none

      include 'main_dim.h'
      include 'part.h'
      include 'measure.h'

      integer Nx,Ny
      real xmax,xmin,ymax,ymin
      COMMON/VORT_AVG/Nx,Ny,xmax,xmin,ymax,ymin
      integer n
      real time,dt,slip_frac
      COMMON/PARAMS/n,Time,dt,slip_frac
      integer np
      real s2,ovrlp,gnu
      COMMON/PART/Np,s2,ovrlp,gnu
      integer iframe
      integer in,ix,iy,ngrid,i,j,m0,m
      real pi,twopiinv,dx,dy,x0,y0,xx,s2inv2,s2piinv,dxinv,dyinv
      real g,x,y,r2s,c
      real xg(Nx*Ny),yg(Nx*Ny),gg(Nx*Ny)
      character(len=256) :: value,vortoutfile
c----------------------------------------------------------------
      pi = 4.0*ATAN(1.0)
      twopiinv = 0.5/pi

      dx = ABS((Xmax-Xmin)/Nx)
      dy = ABS((Ymax-Ymin)/Ny)
      X0 = Xmin
      Y0 = Ymin

      in = 0
      do 1 ix=1,Nx
        do 2 iy=1,Ny
           in=in+1
           gg(in)=0.
2       continue
1     continue

      in=0
      DO 10 ix = 1,Nx
         xx = X0 + (ix-1)*dx
         DO 11 iy = 1,Ny
            in = in + 1
            xg(in) = xx
            yg(in) = Y0 + (iy-1)*dy
 11      CONTINUE
 10   CONTINUE

      Ngrid = Nx*Ny

c     Determination of  the  circulation  of each particle

      s2inv2 = 0.5/s2
      s2piinv = twopiinv/s2
      dxinv = 1./dx
      dyinv = 1./dy
      DO 20 j = 1,Np
         g = GP(j) * s2piinv
         x = XP(j)
         y = YP(j)
         ix = nint ((x-X0)*dxinv) + 1
         iy = nint ((y-Y0)*dyinv) + 1
         if((ix.gt.-3).and.(iy.gt.-3).and.(ix.lt.(nx+3)).and.
     &      (iy.lt.(ny+3))) then

c--   loop over grid in expnaded code in order to vectorize
C     ---- Points on column IX
         m0 = (ix -1)*Ny + iy
         m = m0
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 - 3
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 - 2
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 - 1
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 + 1
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 + 2
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 + 3
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif

C     ---- Points on column IX - 3
         m0 = (ix - 4)*Ny + iy
         m = m0
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 - 3
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 - 2
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 - 1
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 + 1
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 + 2
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 + 3
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif

C     ---- Points on column IX - 2
         m0 = (ix - 3)*Ny + iy
         m = m0
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 - 3
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 - 2
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 - 1
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 + 1
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 + 2
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 + 3
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif

C     ---- Points on column IX - 1
         m0 = (ix - 2)*Ny + iy
         m = m0
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 - 3
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 - 2
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 - 1
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 + 1
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 + 2
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 + 3
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif

C     ---- Points on column IX + 1
         m0 = (ix)*Ny + iy
         m = m0
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 - 3
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 - 2
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 - 1
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 + 1
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 + 2
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 + 3
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif

C     ---- Points on column IX + 2
         m0 = (ix + 1)*Ny + iy
         m = m0
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 - 3
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 - 2
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 - 1
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 + 1
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 + 2
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 + 3
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif

C     ---- Points on column IX + 3
         m0 = (ix + 2)*Ny + iy
         m = m0
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 - 3
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 - 2
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 - 1
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 + 1
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 + 2
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
         m = m0 + 3
         if( (m.gt.0).and.(m.lt.(ngrid+1)) ) then
         r2s = ((xg(m) - x)**2 + (yg(m) - y)**2) * s2inv2
         c = EXP(-r2s)
         gg(m) = gg(m) + g * c
         endif
       endif
 20   CONTINUE
      WRITE(vortoutfile,'(A,I0.5,A)') 'w.', iframe, '.dat'
      open(1,file=vortoutfile,status='new')
      WRITE(1,'(A)') 'variables=x,y,w'
      WRITE(1,'(A,I8,A,I8)') 'zone i=',Nx,', j=',Ny
      in = 0
      DO j=1,Nx
         DO i=1,Ny
            in = in + 1
            if(abs(gg(in)).lt.1.e-20) gg(in)=0.
            if(abs(gg(in)).gt.1.e20) gg(in)=0.
            WRITE(1,'(3E17.9)') xg(in), yg(in), gg(in)
         END DO
      END DO
      CLOSE(1)
      RETURN
      END
