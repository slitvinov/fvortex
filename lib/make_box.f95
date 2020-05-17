


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    SUBROUTINE MAKE_BOX(nmax,ds1,ds2,kp1,kp2,kparent1,kchildless1, &
    IC1,JC1,NPB1,Iparent1,Imark1,Ipar1ch2, &
    Ich2par1,NPB2,IC2,JC2,XC2,YC2,Ichildless1)

!  This subroutine takes each box on the previous level and splits it into
!  four boxes, creating all the necessary indentification arrays to relate
!  the levels.  It mostly parallels the BOX_1 subroutine.
            
    implicit none

    include 'main_dim.h'

    include 'part.h'

    integer :: limpar
    real :: x0,y0
    COMMON/GEOM/X0,Y0,Limpar

    integer :: BF_marker(Nvort)
    real :: dragBF,liftBF,momBF,xsumBF,ysumBF,rsumBF
    COMMON/BF/dragBF,liftBF,momBF,xsumBF,ysumBF,rsumBF,BF_marker

    integer :: nmax,kp1,kp2,kparent1,kchildless1
    integer :: ic1(nmax/4),jc1(nmax/4),npb1(nmax/4,2)
    integer :: iparent1(nmax/4),imark1(nmax/4)
    integer :: ipar1ch2(nmax/4,4),npb2(nmax,2),ich2par1(nmax)
    integer :: ic2(nmax),jc2(nmax),ichildless1(nmax/4)
    real :: ds1,ds2
    real :: xc2(nmax),yc2(nmax)

    integer :: i,np,kbox,ip,jp,n1,n2,npbox,ipar,jpar,n
    integer :: ich1,jch1,ich2,jch2,ich3,jch3,ich4,jch4
    integer :: lx,ly,ibox,jbox,jn,n1m1,nbx,ix,inew
    integer :: nb1,nb2,nb3,nb4
    integer :: IXY(Nvort),IDUMMY(Nvort),BF_marker_temp(Nvort)
    real :: ds2inv,xst,yst
    real :: si1d,sj1d,si2d,sj2d,si3d,sj3d,si4d,sj4d
!---------------------------------------------------------------------

    do i=1,nvort
        IXY(i) = 0
        IDUMMY(i) = 0
        BF_marker_temp(i)=BF_marker(i)
    enddo

!  Find childless & parent boxes. Subdivide parent boxes in 4 squares.
!  (Parent boxes  are those  that contain more than LIMPAR particles )
    ds2 = 0.5 * ds1
    ds2inv = 1.0/ds2
    Xst = X0 - 0.5*ds2
    Yst = Y0 - 0.5*ds2
    np = 0
    kp2 = 0
    kparent1 = 0
    kchildless1 = 0

    DO 2 kbox = 1,kp1
        ip = IC1(kbox)
        jp = JC1(kbox)
        n1 = NPB1(kbox,1)
        n2 = NPB1(kbox,2)
        Npbox = n2-n1+1
        IF ( Npbox > LIMPAR ) THEN  ! *  Parent Box
            kparent1 = kparent1 + 1
            Iparent1(kparent1) = kbox           ! index of parent box
            Imark1(kbox) = 1
            ipar = (ip-1)*2
            jpar = (jp-1)*2
            ich1 = 1 + ipar          ! 1st subbox
            jch1 = 1 + jpar
            ich2 = ich1              ! 2nd subbox
            jch2 = 2 + jpar
            ich3 = 2 + ipar          ! 3rd  subbox
            jch3 = jch1
            ich4 = ich3              ! 4th  subbox
            jch4 = jch2
            si1d = ich1*ds2
            sj1d = jch1*ds2
            si2d = si1d
            sj2d = jch2*ds2
            si3d = ich3*ds2
            sj3d = sj1d
            si4d = si3d
            sj4d = sj2d
                     
            JN = 0
            DO 20 n = n1,n2
                lx = ( XN(n) - X0 )*ds2inv
                ly = ( YN(n) - Y0 )*ds2inv
                ibox = lx + 1
                jbox = ly + 1
                JN = JN + 1
                IXY(JN) = 1
                IF ( (ibox == ich2) .AND. (jbox == jch2) ) THEN
                    IXY(JN) = 2
                ELSE IF ( (ibox == ich3) .AND. (jbox == jch3) ) THEN
                    IXY(JN) = 3
                ELSE IF ( (ibox == ich4) .AND. (jbox == jch4) ) THEN
                    IXY(JN) = 4
                END IF
            20 END DO

        !   Find  how  many  particles  are  in  each subbox
        !  and store  the  particles  in their new sorted  locations
            n1m1 = n1 - 1
            nbx = n1m1

            CALL WHENEQ(Npbox,IXY,1,1,IDUMMY,nb1)
            DO 211 i = 1,nb1
                ix = i + nbx
                inew = IDUMMY(i) + n1m1
                XP(ix) = XN(inew)
                YP(ix) = YN(inew)
                GP(ix) = GN(inew)
                UU(ix) = Uold(inew)
                VV(ix) = Vold(inew)
                Gdiff(ix) = Gdold(inew)
                BF_marker(ix) = BF_marker_temp(inew)
            211 END DO

            CALL WHENEQ(Npbox,IXY,1,2,IDUMMY,nb2)
            nbx = nbx + nb1
            DO 212 i = 1,nb2
                ix = i + nbx
                inew = IDUMMY(i) + n1m1
                XP(ix) = XN(inew)
                YP(ix) = YN(inew)
                GP(ix) = GN(inew)
                UU(ix) = Uold(inew)
                VV(ix) = Vold(inew)
                Gdiff(ix) = Gdold(inew)
                BF_marker(ix) = BF_marker_temp(inew)
            212 END DO

            CALL WHENEQ(Npbox,IXY,1,3,IDUMMY,nb3)
            nbx = nbx + nb2
            DO 213 i = 1,nb3
                ix = i + nbx
                inew = IDUMMY(i) + n1m1
                XP(ix) = XN(inew)
                YP(ix) = YN(inew)
                GP(ix) = GN(inew)
                UU(ix) = Uold(inew)
                VV(ix) = Vold(inew)
                Gdiff(ix) = Gdold(inew)
                BF_marker(ix) = BF_marker_temp(inew)
            213 END DO

            CALL WHENEQ(Npbox,IXY,1,4,IDUMMY,nb4)
            nbx = nbx + nb3
            DO 214 i = 1,nb4
                ix = i + nbx
                inew = IDUMMY(i) + n1m1
                XP(ix) = XN(inew)
                YP(ix) = YN(inew)
                GP(ix) = GN(inew)
                UU(ix) = Uold(inew)
                VV(ix) = Vold(inew)
                Gdiff(ix) = Gdold(inew)
                BF_marker(ix) = BF_marker_temp(inew)
            214 END DO

        ! Box 1
            np = n1-1
            IF (NB1 > 0) THEN
                kp2 = kp2 + 1
                IPAR1CH2(kbox,1) = kp2
                Ich2par1(kp2) = kbox
                NPB2(kp2,1) = np+1
                np = np + nb1
                NPB2(kp2,2) = np
                IC2(kp2) = ich1
                JC2(kp2) = jch1
                XC2(kp2) = Xst + si1d
                YC2(kp2) = Yst + sj1d
            ELSE
                IPAR1Ch2(kbox,1) = 0
            END IF

        ! Box 2
            IF (nb2 > 0) THEN
                kp2 = kp2 + 1
                IPAR1CH2(kbox,2) = kp2
                Ich2par1(kp2) = kbox
                NPB2(kp2,1) = np + 1
                np = np + nb2
                NPB2(kp2,2) = np
                IC2(kp2) = ich2
                JC2(kp2) = jch2
                XC2(kp2) = Xst + si2d
                YC2(kp2) = Yst + sj2d
            ELSE
                IPAR1Ch2(kbox,2) = 0
            END IF

        ! Box 3
            IF (nb3 > 0) THEN
                kp2 = kp2 + 1
                IPAR1CH2(kbox,3) = kp2
                Ich2par1(kp2) = kbox
                NPB2(kp2,1) = np + 1
                np = np + nb3
                NPB2(kp2,2) = np
                IC2(kp2) = ich3
                JC2(kp2) = jch3
                XC2(kp2) = Xst + si3d
                YC2(kp2) = Yst + sj3d
            ELSE
                IPAR1Ch2(kbox,3) = 0
            END IF

        ! Box 4
            IF (nb4 > 0) THEN
                kp2 = kp2 + 1
                IPAR1CH2(kbox,4) = kp2
                Ich2par1(kp2) = kbox
                NPB2(kp2,1) = np + 1
                np = np + nb4
                NPB2(kp2,2) = np
                IC2(kp2) = ich4
                JC2(kp2) = jch4
                XC2(kp2) = Xst + si4d
                YC2(kp2) = Yst + sj4d
            ELSE
                IPAR1Ch2(kbox,4) = 0
            END IF

        ELSE                          !  * Box is childless
            kchildless1 = Kchildless1 + 1
            Ichildless1(kchildless1) = kbox
            Imark1(kbox) = 0

            DO 22 i = n1,n2
                XP(i) = XN(i)
                YP(i) = YN(i)
                GP(i) = GN(i)
                UU(i) = Uold(i)
                VV(i) = Vold(i)
                Gdiff(i) = Gdold(i)
                BF_marker(i) = BF_marker_temp(i)
            22 END DO
             
        END IF
         
    2 END DO

    do i=1,nvort
        XN(i) = XP(i)
        YN(i) = YP(i)
        GN(i) = GP(i)
        Uold(i) = UU(i)
        Vold(i) = VV(i)
        Gdold(i) = Gdiff(i)
    enddo

            
    RETURN
    END SUBROUTINE MAKE_BOX
