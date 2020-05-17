!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    SUBROUTINE  INT_REST5(kp5)

!  Same as int_rest2 for level 5 boxes.

    implicit none

    include 'tree_tmp.h'
    include 'main_dim.h'
    include 'part.h'
    include 'tree9.h'

    integer :: limpar
    real :: x0,y0
    COMMON/GEOM/X0,Y0,Limpar

    integer :: kp5

    integer :: Listfar(Nhlp),Listclose(Nhlp),Listexam(Nhlp)
    integer :: Listpart(Nhlp),kb,ib,jb,ipar,jpar,i,kexam
    integer :: n4,k,id,n1,n2,np,kbb,kclose,kfar,kpart
    real :: dyopiinv,r41,r42,r43,r44,r51,r52,r53,r54,r55,xb,yb
!---------------------------------------------------------------------

    dyopiinv=1./(8.*atan(1.))

    r41 = 0.125
    r42 = 0.25
    r43 = 0.50
    r44 = 1.0
    r51 = 0.0625
    r52 = 0.125
    r53 = 0.25
    r54 = 0.50
    r55 = 1.0

    DO 20 kb = 1,kp5       ! All boxes Childless & Parents

        ib = IC5(kb)
        jb = JC5(kb)
        xb = XC5(kb)
        yb = YC5(kb)
        ipar = (xb-X0)/ds4 + 1
        jpar = (yb-Y0)/ds4 + 1
        do 1 i=1,kp1
            kexam=kp1
            Listexam(i)=liststart(i)
        1 END DO

        CALL near_far(Nmax1,ipar,jpar,r41,IC1,JC1,kexam,Listexam, &
        kfar,Listfar,Kclose,Listclose)

        CALL check_box(Nmax1,kclose,Listclose,kexam,Listexam,kpart &
        ,Listpart,Ipar1Ch2,Imark1)

        CALL near_far(Nmax1,ib,jb,r51,IC1,JC1,kpart,Listpart, &
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

        CALL near_far(Nmax2,ipar,jpar,r42,IC2,JC2,kexam,Listexam, &
        kfar,Listfar,Kclose,Listclose)

        CALL check_box(Nmax2,kclose,Listclose,kexam,Listexam,kpart &
        ,Listpart,Ipar2Ch3,Imark2)                 ! NT

        CALL near_far(Nmax2,ib,jb,r52,IC2,JC2,kpart,Listpart, &
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

        CALL near_far(Nmax3,ipar,jpar,r43,IC3,JC3,kexam,Listexam, &
        kfar,Listfar,Kclose,Listclose)

        CALL check_box(Nmax3,kclose,Listclose,kexam,Listexam,kpart &
        ,Listpart,Ipar3Ch4,Imark3)                 ! NT
                                                        
        CALL near_far(Nmax3,ib,jb,r53,IC3,JC3,kpart,Listpart, &
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

    !--------------------------------------------------------------------------
    ! -> 4th level

    ! Close to parents(?)
        CALL near_far(Nmax4,ipar,jpar,r44,IC4,JC4,Kexam,Listexam, &
        kfar,Listfar,kclose,Listclose)

    ! Close to parents - Childless(?)
        CALL check_box(Nmax4,kclose,Listclose, &
        kexam,Listexam,kpart,Listpart,Ipar4Ch5,Imark4)

    ! Close to parents & childless - close to box(?)
        CALL near_far(Nmax4,ib,jb,r54,IC4,JC4,Kpart,Listpart, &
        kfar,Listfar,kclose,Listclose)

    
    !  Boxes that are far from the child now belong to list 4 of the box
    
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

        IF (n4 == 0)GOTO 88
        if (n4 > np_max) write(*,*)'error in rest5b',n4
        CALL int_box_part(Nmax5,kb,xb,yb,n4,Br5,Bi5)

        88 CALL near_far(Nmax5,ib,jb,r55,IC5,JC5,kexam,Listexam, &
        kfar,Listfar,Kclose,Listclose)

    ! CDIR$SHORTLOOP
        DO 25 kbb = 1,kfar
            id = Listfar(kbb)
            Xbox(kbb) = XC5(id)
            Ybox(kbb) = YC5(id)
            Prbox(kbb,0) = Pr5(id,0)
            Pibox(kbb,0) = Pi5(id,0)
            Prbox(kbb,1) = Pr5(id,1)
            Pibox(kbb,1) = Pi5(id,1)
            Prbox(kbb,2) = Pr5(id,2)
            Pibox(kbb,2) = Pi5(id,2)
            Prbox(kbb,3) = Pr5(id,3)
            Pibox(kbb,3) = Pi5(id,3)
            Prbox(kbb,4) = Pr5(id,4)
            Pibox(kbb,4) = Pi5(id,4)
            Prbox(kbb,5) = Pr5(id,5)
            Pibox(kbb,5) = Pi5(id,5)
            Prbox(kbb,6) = Pr5(id,6)
            Pibox(kbb,6) = Pi5(id,6)
            Prbox(kbb,7) = Pr5(id,7)
            Pibox(kbb,7) = Pi5(id,7)
        25 END DO

        if (kfar > nbox_max) write(*,*)'error in rest5',kbb
        CALL int_box(Nmax5,kb,xb,yb,kfar,Br5,Bi5)

    20 END DO
    RETURN
    END SUBROUTINE 
