c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        SUBROUTINE BOX_1(Npart,s0,XC1,YC1,IC1,JC1,NPB1,ds1,kp1
     &                    ,Liststart)

c  This subroutine sorts the particles into four boxes and provides the
c  necessary identification arrays for this top level of the interaction tree.

        implicit none

        include 'main_dim.h'
        include 'part.h'

        integer limpar
        real x0,y0
        COMMON/GEOM/X0,Y0,limpar

        integer BF_marker(Nvort)
        real dragBF,liftBF,momBF,xsumBF,ysumBF,rsumBF
        COMMON/BF/dragBF,liftBF,momBF,xsumBF,ysumBF,rsumBF,BF_marker

        integer npart,kp1,BF_marker_temp(Nvort)
        integer ic1(4),jc1(4),npb1(4,2),Liststart(4)
        real s0,ds1
        real xc1(4),yc1(4)

        integer i,n,np,ibox,jbox,inew,ix
        integer nbx,nb1,nb2,nb3,nb4
        real xst,yst,ds1inv,lx,ly

        integer IXY(Nvort),IDUMMY(Nvort)
c------------------------------------------------------------------------

        do i=1,nvort
           IXY(i) = 0
           IDUMMY(i) = 0
           BF_marker_temp(i)=BF_marker(i)
        enddo
        ds1 = 0.5 * S0
        Xst = X0 - 0.5*ds1
        Yst = Y0 - 0.5*ds1
        ds1inv = 1.0/ds1

c-- Identify each particle with one of the boxes
        DO 1 n = 1,Npart
          lx = (Xp(n)-X0)*ds1inv
          ly = (Yp(n)-Y0)*ds1inv
          ibox = lx + 1                ! indices of the box where the particles
          jbox = ly + 1                ! reside
             IXY(n) = 1                                  ! IXY(n) has as value
          IF ( (ibox.EQ.1).AND.(jbox.EQ.2) ) THEN         ! the index (1,2,3,4)
              IXY(n) = 2                                  ! of the box that 
          ELSE IF ( (ibox.EQ.2).AND.(jbox.EQ.1) ) THEN    ! particle n is in
              IXY(n) = 3
          ELSE IF ( (ibox.EQ.2).AND.(jbox.EQ.2) ) THEN
              IXY(n) = 4
          END IF
1       CONTINUE

C   Find  how  many  particles  are  in  each subbox
C  and store  the  particles  in their new sorted  locations 

        CALL WHENEQ(Npart,IXY,1,1,IDUMMY,nb1)
          DO 211 i = 1,nb1
            inew = IDUMMY(i)
            XN(i) = XP(inew)
            YN(i) = YP(inew)
            GN(i) = GP(inew)
            Uold(i) = UU(inew)
            Vold(i) = VV(inew)
            Gdold(i) = Gdiff(inew)
            BF_marker(i)=BF_marker_temp(inew)
211        CONTINUE

        CALL WHENEQ(Npart,IXY,1,2,IDUMMY,nb2)
          nbx = nb1
          DO 212 i = 1,nb2
            ix = i + nbx
            inew = IDUMMY(i)
            XN(ix) = XP(inew)
            YN(ix) = YP(inew)
            GN(ix) = GP(inew)
            Uold(ix) = UU(inew)
            Vold(ix) = VV(inew)
            Gdold(ix) = Gdiff(inew)
            BF_marker(ix)=BF_marker_temp(inew)
212        CONTINUE

        CALL WHENEQ(Npart,IXY,1,3,IDUMMY,nb3)
          nbx = nbx + nb2
          DO 213 i = 1,nb3
            ix = i + nbx
            inew = IDUMMY(i)
            XN(ix) = XP(inew)
            YN(ix) = YP(inew)
            GN(ix) = GP(inew)
            Uold(ix) = UU(inew)
            Vold(ix) = VV(inew)
            Gdold(ix) = Gdiff(inew)
            BF_marker(ix)=BF_marker_temp(inew)
213        CONTINUE

        CALL WHENEQ(Npart,IXY,1,4,IDUMMY,nb4)
          nbx = nbx + nb3
          DO 214 i = 1,nb4
            ix = i + nbx
            inew = IDUMMY(i)
            XN(ix) = XP(inew)
            YN(ix) = YP(inew)
            GN(ix) = GP(inew)
            Uold(ix) = UU(inew)
            Vold(ix) = VV(inew)
            Gdold(ix) = Gdiff(inew)
            BF_marker(ix)=BF_marker_temp(inew)
214        CONTINUE

C  NPBk(kp1,1)  -> index of first particle in box kp1 at level k
C  NPBk(kp1,2)  -> index of last particle in box kp1 at level k

C  kpi is the number of boxes that contain particles at level i

c  Now make necessary identification arrays for the boxes
C Box 1
        kp1 = 0
        do i=1,4
           Liststart(i) = 0
        enddo
        np = 0
        IF (nb1.GT.0) THEN
          kp1 = kp1 + 1
          Liststart(kp1) = kp1
          NPB1(kp1,1) = np+1
          np = nb1
          NPB1(kp1,2) = np
          IC1(kp1) = 1
          JC1(kp1) = 1
          XC1(kp1) = Xst + ds1
          YC1(kp1) = Yst + ds1
        END IF

C Box 2
        IF (nb2.GT.0) THEN
          kp1 = kp1 + 1
          Liststart(kp1) = kp1
          NPB1(kp1,1) = np + 1
          np = np + nb2
          NPB1(kp1,2) = np 
          IC1(kp1) = 1
          JC1(kp1) = 2
          XC1(kp1) = Xst + ds1
          YC1(kp1) = Yst + 2.*ds1
        END IF

C Box 3
        IF (nb3.GT.0) THEN
          kp1 = kp1 + 1
          Liststart(kp1) = kp1
          NPB1(kp1,1) = np + 1
          np = np + nb3
          NPB1(kp1,2) = np 
          IC1(kp1) = 2
          JC1(kp1) = 1
          XC1(kp1) = Xst + 2.*ds1
          YC1(kp1) = Yst + ds1
        END IF

C Box 4
        IF (nb4.GT.0) THEN
          kp1 = kp1 + 1
          Liststart(kp1) = kp1
          NPB1(kp1,1) = np + 1
          np = np + nb4
          NPB1(kp1,2) = np 
          IC1(kp1) = 2
          JC1(kp1) = 2
          XC1(kp1) = Xst + 2.*ds1
          YC1(kp1) = Yst + 2.*ds1
        END IF
        
      RETURN
      END
