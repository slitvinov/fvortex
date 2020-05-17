!  These subroutines determine the hierarchy of interactions and call for the
!  interactions. Particle interactions are assigned in the int_chless routines
!  as they only happen for childless boxes (otherwise you go to the next level
!  in hopes of geting some box interactions). Box interactions are taken care
!  of by the int_rest routines. Particle-box interactions occur in both, with
!  the particle end of these interactions in int_rest and box in int_chless.
!-------------------------------------------------------------------------

    SUBROUTINE INT_CHLESS1(kchildless1)

!  This subroutine figures out the interactions for childless boxes at
!  level 1 (the highest level of the tree).  They can interact as
!  particle-particle with nearby childless boxes and particle-box with
!  finer boxes which are far enough away (bigger box must be considered as
!  particles for the smaller box in this case but smaller box as a box to
!  bigger box). Box-box interactions cannot happen for a level 1 box and are
!  handled by the int_rest arrays rather than int_chless.

    implicit none

    include 'tree_tmp.h'
    include 'main_dim.h'
    include 'part.h'
    include 'tree9.h'

    integer :: kchildless1

    integer :: Listfar(Nhlp),Listclose(Nhlp),Listexam(Nhlp)
    integer :: Listpart(Nhlp),kexam,kfar,kclose,i,kh,kb,ib,jb
    integer :: nb1,nb2,nns,k,id,n1,n2,np,level,kfp,nn,kpart,n
    real :: r12,r13,r14,r15,r16,r17,r18,r19,xnn,ynn,gnn,dyopiinv
    real :: up1,vp1,gp1,up2,vp2,gp2,ubox,vbox
!-------------------------------------------------------------------------
            
    dyopiinv=1./(8.*atan(1.))
    r12 = 2.0
    r13 = 4.0
    r14 = 8.0
    r15 = 16.0
    r16 = 32.0
    r17 = 64.0
    r18 = 128.0
    r19 = 256.0
    kexam = 0
    kfar = 0
    kclose = 0
    do i=1,nhlp
        Listexam(i) = 0
        Listfar(i) = 0
        Listclose(i) = 0
    enddo

    DO 10 kh = 1,Kchildless1
        kb = Ichildless1(kh)
        Ib = IC1(kb)
        Jb = JC1(kb)
        nb1 = NPB1(kb,1)
        nb2 = NPB1(kb,2)
        do 1 i=1,kp1
            kclose=kclose+1
            Listclose(i)=Liststart(i)
        1 END DO

    !  Construct the interaction list with particles and boxes that belong
    !  to finer levels than the ** 1st **.
    !  First find all childless boxes on level 1.
    !  Note that the box will find and interact with itself (as particles)

        CALL check_box(Nmax1,kclose,Listclose,kexam,Listexam,kpart, &
        Listpart,Ipar1Ch2,Imark1)

        nns = 0
        DO 11 k = 1,Kpart
            id = Listpart(k)
            n1 = NPB1(id,1)
            n2 = NPB1(id,2)
            DO 111 np = n1,n2
                nns = nns + 1
                XT(nns) = XN(np)
                YT(nns) = YN(np)
                GT(nns) = GN(np)
            111 END DO
        11 END DO

        IF(nns > np_max) WRITE(*,*)'error in int_chless1',nns
        DO 251 n = nb1,nb2
            CALL int_part1(XN(n),YN(n),GN(n),up1,vp1,gp1,nns)
            UU(n) = UU(n) + up1*dyopiinv
            VV(n) = VV(n) + vp1*dyopiinv
            gdiff(n) = gdiff(n) + gp1
        251 END DO

    ! ______________________________
        LEVEL = 2

    !  Find which level 2 boxes are far enough away to interact as a box with
    !  level 1 particles (level 2 boxes are the 4 subdivisions of a level 1 box).
         
        CALL near_far(Nmax2,ib,jb,r12,IC2,JC2,kexam,Listexam, &
        kfar,Listfar,kclose,Listclose)

        kfp = 0
        DO 12 k =1,Kfar
            kfp = kfp + 1
            id = Listfar(k)
            Xbox(kfp) = XC2(id)
            Ybox(kfp) = YC2(id)
            Prbox(kfp,0) = Pr2(id,0)
            Pibox(kfp,0) = Pi2(id,0)
            Prbox(kfp,1) = Pr2(id,1)
            Pibox(kfp,1) = Pi2(id,1)
            Prbox(kfp,2) = Pr2(id,2)
            Pibox(kfp,2) = Pi2(id,2)
            Prbox(kfp,3) = Pr2(id,3)
            Pibox(kfp,3) = Pi2(id,3)
            Prbox(kfp,4) = Pr2(id,4)
            Pibox(kfp,4) = Pi2(id,4)
            Prbox(kfp,5) = Pr2(id,5)
            Pibox(kfp,5) = Pi2(id,5)
            Prbox(kfp,6) = Pr2(id,6)
            Pibox(kfp,6) = Pi2(id,6)
            Prbox(kfp,7) = Pr2(id,7)
            Pibox(kfp,7) = Pi2(id,7)
        12 END DO

    !  Check the remaining level 2 boxes for childless boxes. Since they didn't
    !  interact as a box above and further subdivisions don't exist for the
    !  box, it must now interact as particles.

        CALL check_box(Nmax2,kclose,Listclose,kexam,Listexam,kpart, &
        Listpart,Ipar2Ch3,Imark2)

        nn = 0
        DO 25 k=1,kpart
            id = Listpart(k)
            n1 = NPB2(id,1)
            n2 = NPB2(id,2)
            DO 250 np = n1,n2
                nn = nn + 1
                XT(nn) = XN(np)
                YT(nn) = YN(np)
                GT(nn) = GN(np)
                IT(nn) = np
            250 END DO
        25 END DO

    ! All remaining boxes (those which have not yet interacted in some way) are
    ! parents, thus go to their level 3 children. Process of level 2 repeats
    ! for all subsequent levels.

    ! _____________________________________
        LEVEL = 3

        CALL near_far(Nmax3,ib,jb,r13,IC3,JC3,kexam,Listexam, &
        kfar,Listfar,kclose,Listclose)

        DO 26 k =1,kfar
            kfp = kfp + 1
            id = Listfar(k)
            Xbox(kfp) = XC3(id)
            Ybox(kfp) = YC3(id)
            Prbox(kfp,0) = Pr3(id,0)
            Pibox(kfp,0) = Pi3(id,0)
            Prbox(kfp,1) = Pr3(id,1)
            Pibox(kfp,1) = Pi3(id,1)
            Prbox(kfp,2) = Pr3(id,2)
            Pibox(kfp,2) = Pi3(id,2)
            Prbox(kfp,3) = Pr3(id,3)
            Pibox(kfp,3) = Pi3(id,3)
            Prbox(kfp,4) = Pr3(id,4)
            Pibox(kfp,4) = Pi3(id,4)
            Prbox(kfp,5) = Pr3(id,5)
            Pibox(kfp,5) = Pi3(id,5)
            Prbox(kfp,6) = Pr3(id,6)
            Pibox(kfp,6) = Pi3(id,6)
            Prbox(kfp,7) = Pr3(id,7)
            Pibox(kfp,7) = Pi3(id,7)

        26 END DO

        CALL check_box(Nmax3,Kclose,Listclose,kexam,Listexam,Kpart &
        ,Listpart,Ipar3Ch4,Imark3)

        DO 27 k = 1,kpart
            id = Listpart(k)
            n1 = NPB3(id,1)
            n2 = NPB3(id,2)
            DO 270 np = n1,n2
                nn = nn + 1
                XT(nn) = XN(np)
                YT(nn) = YN(np)
                GT(nn) = GN(np)
                IT(nn) = np
            270 END DO
        27 END DO

    ! ____________________
        LEVEL = 4

        CALL near_far(Nmax4,ib,jb,r14,IC4,JC4,kexam,Listexam, &
        kfar,Listfar,kclose,Listclose)

        DO 28 k =1,kfar
            kfp = kfp + 1
            id = Listfar(k)
            Xbox(kfp) = XC4(id)
            Ybox(kfp) = YC4(id)
            Prbox(kfp,0) = Pr4(id,0)
            Pibox(kfp,0) = Pi4(id,0)
            Prbox(kfp,1) = Pr4(id,1)
            Pibox(kfp,1) = Pi4(id,1)
            Prbox(kfp,2) = Pr4(id,2)
            Pibox(kfp,2) = Pi4(id,2)
            Prbox(kfp,3) = Pr4(id,3)
            Pibox(kfp,3) = Pi4(id,3)
            Prbox(kfp,4) = Pr4(id,4)
            Pibox(kfp,4) = Pi4(id,4)
            Prbox(kfp,5) = Pr4(id,5)
            Pibox(kfp,5) = Pi4(id,5)
            Prbox(kfp,6) = Pr4(id,6)
            Pibox(kfp,6) = Pi4(id,6)
            Prbox(kfp,7) = Pr4(id,7)
            Pibox(kfp,7) = Pi4(id,7)

        28 END DO

        CALL check_box(Nmax4,Kclose,Listclose,kexam,Listexam,Kpart, &
        Listpart,Ipar4Ch5,Imark4)

        DO 29 k = 1,kpart
            id = Listpart(k)
            n1 = NPB4(id,1)
            n2 = NPB4(id,2)
            DO 290 np = n1,n2
                nn = nn + 1
                XT(nn) = XN(np)
                YT(nn) = YN(np)
                GT(nn) = GN(np)
                IT(nn) = np
            290 END DO
        29 END DO

    ! ____________________
        LEVEL = 5
          
        CALL near_far(Nmax5,ib,jb,r15,IC5,JC5,kexam,Listexam, &
        kfar,Listfar,kclose,Listclose)

        DO 30 k =1,kfar
            kfp = kfp + 1
            id = Listfar(k)
            Xbox(kfp) = XC5(id)
            Ybox(kfp) = YC5(id)
            Prbox(kfp,0) = Pr5(id,0)
            Pibox(kfp,0) = Pi5(id,0)
            Prbox(kfp,1) = Pr5(id,1)
            Pibox(kfp,1) = Pi5(id,1)
            Prbox(kfp,2) = Pr5(id,2)
            Pibox(kfp,2) = Pi5(id,2)
            Prbox(kfp,3) = Pr5(id,3)
            Pibox(kfp,3) = Pi5(id,3)
            Prbox(kfp,4) = Pr5(id,4)
            Pibox(kfp,4) = Pi5(id,4)
            Prbox(kfp,5) = Pr5(id,5)
            Pibox(kfp,5) = Pi5(id,5)
            Prbox(kfp,6) = Pr5(id,6)
            Pibox(kfp,6) = Pi5(id,6)
            Prbox(kfp,7) = Pr5(id,7)
            Pibox(kfp,7) = Pi5(id,7)
                        
        30 END DO

        CALL check_box(Nmax5,Kclose,Listclose,kexam,Listexam,Kpart, &
        Listpart,Ipar5Ch6,Imark5)

        DO 31 k = 1,kpart
            id = Listpart(k)
            n1 = NPB5(id,1)
            n2 = NPB5(id,2)
            DO 310 np = n1,n2
                nn = nn + 1
                XT(nn) = XN(np)
                YT(nn) = YN(np)
                GT(nn) = GN(np)
                IT(nn) = np
            310 END DO
        31 END DO

    ! ____________________
        LEVEL = 6

        CALL near_far(Nmax6,ib,jb,r16,IC6,JC6,kexam,Listexam, &
        kfar,Listfar,kclose,Listclose)

        DO 32 k =1,kfar
            kfp = kfp + 1
            id = Listfar(k)
            Xbox(kfp) = XC6(id)
            Ybox(kfp) = YC6(id)
            Prbox(kfp,0) = Pr6(id,0)
            Pibox(kfp,0) = Pi6(id,0)
            Prbox(kfp,1) = Pr6(id,1)
            Pibox(kfp,1) = Pi6(id,1)
            Prbox(kfp,2) = Pr6(id,2)
            Pibox(kfp,2) = Pi6(id,2)
            Prbox(kfp,3) = Pr6(id,3)
            Pibox(kfp,3) = Pi6(id,3)
            Prbox(kfp,4) = Pr6(id,4)
            Pibox(kfp,4) = Pi6(id,4)
            Prbox(kfp,5) = Pr6(id,5)
            Pibox(kfp,5) = Pi6(id,5)
            Prbox(kfp,6) = Pr6(id,6)
            Pibox(kfp,6) = Pi6(id,6)
            Prbox(kfp,7) = Pr6(id,7)
            Pibox(kfp,7) = Pi6(id,7)
                        
        32 END DO

        CALL check_box(Nmax6,Kclose,Listclose,kexam,Listexam,Kpart, &
        Listpart,Ipar6Ch7,Imark6)

        DO 33 k = 1,kpart
            id = Listpart(k)
            n1 = NPB6(id,1)
            n2 = NPB6(id,2)
            DO 330 np = n1,n2
                nn = nn + 1
                XT(nn) = XN(np)
                YT(nn) = YN(np)
                GT(nn) = GN(np)
                IT(nn) = np
            330 END DO
        33 END DO

    ! ____________________
        LEVEL = 7

        CALL near_far(Nmax7,ib,jb,r17,IC7,JC7,kexam,Listexam, &
        kfar,Listfar,kclose,Listclose)

        DO 34 k =1,kfar
            kfp = kfp + 1
            id = Listfar(k)
            Xbox(kfp) = XC7(id)
            Ybox(kfp) = YC7(id)
            Prbox(kfp,0) = Pr7(id,0)
            Pibox(kfp,0) = Pi7(id,0)
            Prbox(kfp,1) = Pr7(id,1)
            Pibox(kfp,1) = Pi7(id,1)
            Prbox(kfp,2) = Pr7(id,2)
            Pibox(kfp,2) = Pi7(id,2)
            Prbox(kfp,3) = Pr7(id,3)
            Pibox(kfp,3) = Pi7(id,3)
            Prbox(kfp,4) = Pr7(id,4)
            Pibox(kfp,4) = Pi7(id,4)
            Prbox(kfp,5) = Pr7(id,5)
            Pibox(kfp,5) = Pi7(id,5)
            Prbox(kfp,6) = Pr7(id,6)
            Pibox(kfp,6) = Pi7(id,6)
            Prbox(kfp,7) = Pr7(id,7)
            Pibox(kfp,7) = Pi7(id,7)
        34 END DO

        CALL check_box(Nmax7,Kclose,Listclose,kexam,Listexam,Kpart, &
        Listpart,Ipar7Ch8,Imark7)

        DO 35 k = 1,kpart
            id = Listpart(k)
            n1 = NPB7(id,1)
            n2 = NPB7(id,2)
            DO 350 np = n1,n2
                nn = nn + 1
                XT(nn) = XN(np)
                YT(nn) = YN(np)
                GT(nn) = GN(np)
                IT(nn) = np
            350 END DO
        35 END DO

    ! ____________________
        LEVEL = 8

        CALL near_far(Nmax8,ib,jb,r18,IC8,JC8,kexam,Listexam, &
        kfar,Listfar,kclose,Listclose)

        DO 36 k =1,kfar
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
        36 END DO


        CALL check_box(Nmax8,Kclose,Listclose,kexam,Listexam,Kpart, &
        Listpart,Ipar8Ch9,Imark8)

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

        CALL near_far(Nmax9,ib,jb,r19,IC9,JC9,kexam,Listexam, &
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
                 
                 
        IF(nn > np_max)WRITE(*,*)'error in int_chless1p',nn
        IF(kfp > nbox_max)WRITE(*,*)'error in int_chless1b',kfp
        DO 351 n = nb1,nb2
            xnn = XN(n)
            ynn = YN(n)
            gnn = GN(n)
            CALL int_part2(gnn,xnn,ynn,up2,vp2,gp2,nn)
            CALL int_part_box(xnn,ynn,ubox,vbox,kfp)
            UU(n) = UU(n) + (up2 + ubox)*dyopiinv
            VV(n) = VV(n) + (vp2 + vbox)*dyopiinv
            gdiff(n) = gdiff(n) + gp2
        351 END DO

    10 END DO

    RETURN
    END SUBROUTINE INT_CHLESS1
