!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    SUBROUTINE  INT_CHLESS7(kp6,kchildless7)

!  Same idea as in int_chless2 but now for level 7 childless boxes.
!  For descriptive comments, go back to int_chless1&2

    implicit none

    include 'tree_tmp.h'
    include 'main_dim.h'
    include 'part.h'
    include 'tree9.h'

    integer :: limpar
    real :: x0,y0
    COMMON/GEOM/X0,Y0,Limpar

    integer :: kp6,kchildless7

    integer :: Listfar(Nhlp),Listclose(Nhlp),Listexam(Nhlp)
    integer :: Listpart(Nhlp),Lclg(10),nns,ipar,jpar,kc,j,m,ks,km
    integer :: kexam,kfar,kclose,i,kh,kb,ib,jb
    integer :: nb1,nb2,k,id,n1,n2,np,level,kfp,nn,kpart,n
    real :: r77,r78,r79,xnn,ynn,gnn,dyopiinv
    real :: up1,vp1,gp1,up2,vp2,gp2,ubox,vbox
!----------------------------------------------------------------------------
            
    dyopiinv=1./(8.*atan(1.))
            
    r77 = 1.0
    r78 = 2.0
    r79 = 4.0

    DO 20 kh = 1,Kchildless7
        nns = 0   ! List 1 (same level)
        nn = 0    ! List 1 (finer levels)
        kfp = 0   ! List 3
        kb = Ichildless7(kh)           ! box b index
        ib = IC7(kb)
        jb = JC7(kb)
        nb1 = NPB7(kb,1)
        nb2 = NPB7(kb,2)
        ipar = (XC7(kb)-X0)/ds6 + 1
        jpar = (YC7(kb)-Y0)/ds6 + 1
        kc = 0
        DO 21 k =1, Kp6 ! Loop over boxes in parents level.
            i = IC6(k)
            j = JC6(k)
            IF( (IABS(i-ipar) > 1) .OR. (IABS(j-jpar) > 1) ) GOTO 21
            kc = kc + 1
            Lclg(kc) = k
        21 END DO

        kexam = 0
        DO 22 m = 1,4
            DO 23 k = 1,kc
                ks = Lclg(k)
                km = Ipar6Ch7(ks,m)
                IF(km == 0)GOTO 23
                kexam = kexam + 1
                Listexam(kexam) = km
            23 END DO
        22 END DO

        CALL near_far(Nmax7,ib,jb,r77,IC7,JC7,kexam,Listexam &
        ,kfar,Listfar,Kclose,Listclose)

        CALL check_box(Nmax7,kclose,Listclose,kexam,Listexam,kpart &
        ,Listpart,Ipar7Ch8,Imark7)

        DO 25 k=1,kpart
            id = Listpart(k)
            n1 = NPB7(id,1)
            n2 = NPB7(id,2)
            DO 250 np = n1,n2
                nns = nns + 1
                XT(nns) = XN(np)
                YT(nns) = YN(np)    ! childless boxes same level
                GT(nns) = GN(np)
            250 END DO
        25 END DO

        IF(nns > np_max) WRITE(*,*)'error in int_chless7',nns
        DO 251 n = nb1,nb2
            CALL int_part1(XN(n),YN(n),GN(n),up1,vp1,gp1,nns)
            UU(n) = UU(n) + up1*dyopiinv
            VV(n) = VV(n) + vp1*dyopiinv
            gdiff(n) = gdiff(n) + gp1
        251 END DO

    ! ____________________
        LEVEL = 8
        IF(kexam == 0)GOTO 201

        CALL near_far(Nmax8,ib,jb,r78,IC8,JC8,kexam,Listexam, &
        kfar,Listfar,kclose,Listclose)

        DO 34 k =1,kfar
            kfp = kfp + 1
            id = Listfar(k)
            Xbox(kfp) = XC8(id)
            Ybox(kfp) = YC8(id)
            Prbox(kfp,0) = Pr8(id,0)
            Pibox(kfp,0) = Pi8(id,0)
            Prbox(kfp,1) = Pr8(id,1)
            Pibox(kfp,1) = Pi8(id,1)
            Prbox(kfp,2) = Pr8(id,2)
            Pibox(kfp,2) = Pi8(id,2)
            Prbox(kfp,3) = Pr8(id,3)
            Pibox(kfp,3) = Pi8(id,3)
            Prbox(kfp,4) = Pr8(id,4)
            Pibox(kfp,4) = Pi8(id,4)
            Prbox(kfp,5) = Pr8(id,5)
            Pibox(kfp,5) = Pi8(id,5)
            Prbox(kfp,6) = Pr8(id,6)
            Pibox(kfp,6) = Pi8(id,6)
            Prbox(kfp,7) = Pr8(id,7)
            Pibox(kfp,7) = Pi8(id,7)
        34 END DO

        CALL check_box(Nmax8,kclose,Listclose,kexam,Listexam,kpart &
        ,Listpart,Ipar8Ch9,Imark8)

        DO 37 k = 1,kpart
            id = Listpart(k)
            n1 = NPB8(id,1)
            n2 = NPB8(id,2)
            DO 370 np = n1,n2
                nn = nn + 1
                XT(nn) = XN(np)
                YT(nn) = YN(np)
                GT(nn) = GN(np)
                IT(nn) = np
            370 END DO
        37 END DO

    ! ____________________
        LEVEL = 9
        IF(kexam == 0)GOTO 201

        CALL near_far(Nmax9,ib,jb,r79,IC9,JC9,kexam,Listexam, &
        kfar,Listfar,kclose,Listclose)

        DO k =1,kfar
            kfp = kfp + 1
            id = Listfar(k)
            Xbox(kfp) = XC9(id)
            Ybox(kfp) = YC9(id)
            Prbox(kfp,0) = Pr9(id,0)
            Pibox(kfp,0) = Pi9(id,0)
            Prbox(kfp,1) = Pr9(id,1)
            Pibox(kfp,1) = Pi9(id,1)
            Prbox(kfp,2) = Pr9(id,2)
            Pibox(kfp,2) = Pi9(id,2)
            Prbox(kfp,3) = Pr9(id,3)
            Pibox(kfp,3) = Pi9(id,3)
            Prbox(kfp,4) = Pr9(id,4)
            Pibox(kfp,4) = Pi9(id,4)
            Prbox(kfp,5) = Pr9(id,5)
            Pibox(kfp,5) = Pi9(id,5)
            Prbox(kfp,6) = Pr9(id,6)
            Pibox(kfp,6) = Pi9(id,6)
            Prbox(kfp,7) = Pr9(id,7)
            Pibox(kfp,7) = Pi9(id,7)
        enddo

        DO k = 1,kclose   ! All close boxes are now childless
            id = Listclose(k)
            n1 = NPB9(id,1)
            n2 = NPB9(id,2)
            DO np = n1,n2
                nn = nn + 1
                XT(nn) = XN(np)
                YT(nn) = YN(np)
                GT(nn) = GN(np)
                IT(nn) = np
            enddo
        enddo


        IF(nn > np_max)WRITE(*,*)'error in int_chless6p',nn
        IF(kfp > nbox_max)WRITE(*,*)'error in int_chless6b',kfp
        201 DO 351 n = nb1,nb2
            xnn = XN(n)
            ynn = YN(n)
            gnn = GN(n)
            CALL int_part2(gnn,xnn,ynn,up2,vp2,gp2,nn)
            CALL int_part_box(xnn,ynn,ubox,vbox,kfp)
            UU(n) = UU(n) + (up2 + ubox)*dyopiinv
            VV(n) = VV(n) + (vp2 + vbox)*dyopiinv
            gdiff(n) = gdiff(n) + gp2
        351 END DO

    20 END DO

    RETURN
    END SUBROUTINE 
