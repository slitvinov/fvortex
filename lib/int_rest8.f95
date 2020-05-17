!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    SUBROUTINE  INT_REST8(kp8)

!  Same as int_rest2 for level 8 boxes.

    implicit none

    include 'tree_tmp.h'
    include 'main_dim.h'
    include 'part.h'
    include 'tree9.h'

    integer :: limpar
    real :: x0,y0
    COMMON/GEOM/X0,Y0,Limpar

    integer :: kp8

    integer :: Listfar(Nhlp),Listclose(Nhlp),Listexam(Nhlp)
    integer :: Listpart(Nhlp),kb,ib,jb,ipar,jpar,i,kexam
    integer :: n4,k,id,n1,n2,np,kbb,kclose,kfar,kpart
    real :: xb,yb,dyopiinv
    real :: r71,r72,r73,r74,r75,r76,r77,r81,r82,r83,r84,r85,r86
    real :: r87,r88
!---------------------------------------------------------------------

    dyopiinv=1./(8.*atan(1.))

    r71 = 0.015625
    r72 = 0.03125
    r73 = 0.06250
    r74 = 0.12500
    r75 = 0.25000
    r76 = 0.50000
    r77 = 1.0
    r81 = 0.0078125
    r82 = 0.015625
    r83 = 0.03125
    r84 = 0.06250
    r85 = 0.12500
    r86 = 0.25000
    r87 = 0.50000
    r88 = 1.0

    DO 20 kb = 1,kp8       ! All boxes Childless & Parents
        ib = IC8(kb)
        jb = JC8(kb)
        xb = XC8(kb)
        yb = YC8(kb)
        ipar = (xb-X0)/ds7 + 1
        jpar = (yb-Y0)/ds7 + 1
        do 1 i=1,kp1
            kexam=kp1
            Listexam(i)=Liststart(i)
        1 END DO
        CALL near_far(Nmax1,ipar,jpar,r71,IC1,JC1,kexam,Listexam, &
        kfar,Listfar,Kclose,Listclose)

        CALL check_box(Nmax1,kclose,Listclose,kexam,Listexam,kpart &
        ,Listpart,Ipar1Ch2,Imark1)               !NT

        CALL near_far(Nmax1,ib,jb,r81,IC1,JC1,kpart,Listpart, &
        kfar,Listfar,Kclose,Listclose)

        n4 = 0
        DO  21 k = 1,kfar
            id = Listfar(k)
            n1 = NPB1(id,1)
            n2 = NPB1(id,2)
            DO 210 np = n1,n2
                n4 = n4 + 1
                XT(n4) = XN(np)
                YT(n4) = YN(np)
                GT(n4) = GN(np)
            210 END DO
        21 END DO

        CALL near_far(Nmax2,ipar,jpar,r72,IC2,JC2,kexam,Listexam, &
        kfar,Listfar,Kclose,Listclose)

        CALL check_box(Nmax2,kclose,Listclose,kexam,Listexam,kpart &
        ,Listpart,Ipar2Ch3,Imark2)                 ! NT

        CALL near_far(Nmax2,ib,jb,r82,IC2,JC2,kpart,Listpart, &
        kfar,Listfar,Kclose,Listclose)

        DO  22 k = 1,kfar
            id = Listfar(k)
            n1 = NPB2(id,1)
            n2 = NPB2(id,2)
            DO 220 np = n1,n2
                n4 = n4 + 1
                XT(n4) = XN(np)
                YT(n4) = YN(np)
                GT(n4) = GN(np)
            220 END DO
        22 END DO

        CALL near_far(Nmax3,ipar,jpar,r73,IC3,JC3,kexam,Listexam, &
        kfar,Listfar,Kclose,Listclose)

        CALL check_box(Nmax3,kclose,Listclose,kexam,Listexam,kpart &
        ,Listpart,Ipar3Ch4,Imark3)                 ! NT
                                                        
        CALL near_far(Nmax3,ib,jb,r83,IC3,JC3,kpart,Listpart, &
        kfar,Listfar,Kclose,Listclose)

        DO  23 k = 1,kfar
            id = Listfar(k)
            n1 = NPB3(id,1)
            n2 = NPB3(id,2)
            DO 230 np = n1,n2
                n4 = n4 + 1
                XT(n4) = XN(np)
                YT(n4) = YN(np)
                GT(n4) = GN(np)
            230 END DO
        23 END DO

        CALL near_far(Nmax4,ipar,jpar,r74,IC4,JC4,kexam,Listexam, &
        kfar,Listfar,Kclose,Listclose)

        CALL check_box(Nmax4,kclose,Listclose,kexam,Listexam,kpart &
        ,Listpart,Ipar4Ch5,Imark4)                 ! NT
                                                        
        CALL near_far(Nmax4,ib,jb,r84,IC4,JC4,kpart,Listpart, &
        kfar,Listfar,Kclose,Listclose)

        DO  24 k = 1,kfar
            id = Listfar(k)
            n1 = NPB4(id,1)
            n2 = NPB4(id,2)
            DO 240 np = n1,n2
                n4 = n4 + 1
                XT(n4) = XN(np)
                YT(n4) = YN(np)
                GT(n4) = GN(np)
            240 END DO
        24 END DO

        CALL near_far(Nmax5,ipar,jpar,r75,IC5,JC5,kexam,Listexam, &
        kfar,Listfar,Kclose,Listclose)

        CALL check_box(Nmax5,kclose,Listclose,kexam,Listexam,kpart &
        ,Listpart,Ipar5Ch6,Imark5)                 ! NT
                                                        
        CALL near_far(Nmax5,ib,jb,r85,IC5,JC5,kpart,Listpart, &
        kfar,Listfar,Kclose,Listclose)

        DO  25 k = 1,kfar
            id = Listfar(k)
            n1 = NPB5(id,1)
            n2 = NPB5(id,2)
            DO 250 np = n1,n2
                n4 = n4 + 1
                XT(n4) = XN(np)
                YT(n4) = YN(np)
                GT(n4) = GN(np)
            250 END DO
        25 END DO


        CALL near_far(Nmax6,ipar,jpar,r76,IC6,JC6,Kexam,Listexam, &
        kfar,Listfar,kclose,Listclose)

        CALL check_box(Nmax6,kclose,Listclose, &
        kexam,Listexam,kpart,Listpart,Ipar6Ch7,Imark6)

        CALL near_far(Nmax6,ib,jb,r86,IC6,JC6,Kpart,Listpart, &
        kfar,Listfar,kclose,Listclose)

        DO  26 k = 1,kfar
            id = Listfar(k)
            n1 = NPB6(id,1)
            n2 = NPB6(id,2)
            DO 260 np = n1,n2
                n4 = n4 + 1
                XT(n4) = XN(np)
                YT(n4) = YN(np)
                GT(n4) = GN(np)
            260 END DO
        26 END DO

        CALL near_far(Nmax7,ipar,jpar,r77,IC7,JC7,Kexam,Listexam, &
        kfar,Listfar,kclose,Listclose)

        CALL check_box(Nmax7,kclose,Listclose, &
        kexam,Listexam,kpart,Listpart,Ipar7Ch8,Imark7)

        CALL near_far(Nmax7,ib,jb,r87,IC7,JC7,Kpart,Listpart, &
        kfar,Listfar,kclose,Listclose)

        DO  27 k = 1,kfar
            id = Listfar(k)
            n1 = NPB7(id,1)
            n2 = NPB7(id,2)
            DO 270 np = n1,n2
                n4 = n4 + 1
                XT(n4) = XN(np)
                YT(n4) = YN(np)
                GT(n4) = GN(np)
            270 END DO
        27 END DO


        IF (n4 == 0)GOTO 88
        if (n4 > np_max) write(*,*)'error in rest8b',n4
        CALL int_box_part(Nmax8,kb,xb,yb,n4,Br8,Bi8)

        88 CALL near_far(Nmax8,ib,jb,r88,IC8,JC8,kexam,Listexam, &
        kfar,Listfar,Kclose,Listclose)

    ! CDIR$SHORTLOOP
        DO kbb = 1,kfar
            id = Listfar(kbb)
            Xbox(kbb) = XC8(id)
            Ybox(kbb) = YC8(id)

            Prbox(kbb,0) = Pr8(id,0)
            Pibox(kbb,0) = Pi8(id,0)
            Prbox(kbb,1) = Pr8(id,1)
            Pibox(kbb,1) = Pi8(id,1)
            Prbox(kbb,2) = Pr8(id,2)
            Pibox(kbb,2) = Pi8(id,2)
            Prbox(kbb,3) = Pr8(id,3)
            Pibox(kbb,3) = Pi8(id,3)
            Prbox(kbb,4) = Pr8(id,4)
            Pibox(kbb,4) = Pi8(id,4)
            Prbox(kbb,5) = Pr8(id,5)
            Pibox(kbb,5) = Pi8(id,5)
            Prbox(kbb,6) = Pr8(id,6)
            Pibox(kbb,6) = Pi8(id,6)
            Prbox(kbb,7) = Pr8(id,7)
            Pibox(kbb,7) = Pi8(id,7)
        enddo
                   
                   
        if (kfar > nbox_max) write(*,*)'error in rest8',kbb
        CALL int_box(Nmax8,kb,xb,yb,kfar,Br8,Bi8)

    20 END DO
    RETURN
    END SUBROUTINE 
