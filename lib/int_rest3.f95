!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    SUBROUTINE  INT_REST3(kp3)

!  Same as int_rest2 for level 3 boxes.

    implicit none

    include 'tree_tmp.h'
    include 'main_dim.h'
    include 'part.h'
    include 'tree9.h'

    integer :: limpar
    real :: x0,y0
    COMMON/GEOM/X0,Y0,Limpar

    integer :: kp3

    integer :: Listfar(Nhlp),Listclose(Nhlp),Listexam(Nhlp)
    integer :: Listpart(Nhlp),kb,ib,jb,ipar,jpar,i,kexam
    integer :: n4,k,id,n1,n2,np,kbb,kclose,kfar,kpart
    real :: dyopiinv,r21,r22,r31,r32,r33,xb,yb
!---------------------------------------------------------------------

    dyopiinv=1./(8.*atan(1.))

    r21 = 0.50
    r22 = 1.0
    r31 = 0.25
    r32 = 0.50
    r33 = 1.0

    DO 20 kb = 1,kp3       ! All boxes Childless & Parents
        ib = IC3(kb)
        jb = JC3(kb)
        xb = XC3(kb)
        yb = YC3(kb)
        ipar = (xb-X0)/ds2 + 1
        jpar = (yb-Y0)/ds2 + 1
        do 1 i=1,kp1
            kexam=kp1
            Listexam(i)=Liststart(i)
        1 END DO

        CALL near_far(Nmax1,ipar,jpar,r21,IC1,JC1,kexam,Listexam, &
        kfar,Listfar,Kclose,Listclose)

        CALL check_box(Nmax1,kclose,Listclose,kexam,Listexam,kpart &
        ,Listpart,Ipar1Ch2,Imark1)

        CALL near_far(Nmax1,ib,jb,r31,IC1,JC1,kpart,Listpart, &
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

        CALL near_far(Nmax2,ipar,jpar,r22,IC2,JC2,kexam,Listexam, &
        kfar,Listfar,Kclose,Listclose)

        CALL check_box(Nmax2,kclose,Listclose,kexam,Listexam,kpart &
        ,Listpart,Ipar2Ch3,Imark2)

        CALL near_far(Nmax2,ib,jb,r32,IC2,JC2,kpart,Listpart, &
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

        IF (n4 == 0)GOTO 88
        if (n4 > np_max) write(*,*)'error in rest3b',n4
        CALL int_box_part(Nmax3,kb,xb,yb,n4,Br3,Bi3)

        88 CALL near_far(Nmax3,ib,jb,r33,IC3,JC3,kexam,Listexam, &
        kfar,Listfar,Kclose,Listclose)

    ! CDIR$SHORTLOOP
        DO 25 kbb = 1,kfar
            id = Listfar(kbb)
            Xbox(kbb) = XC3(id)
            Ybox(kbb) = YC3(id)
            Prbox(kbb,0) = Pr3(id,0)
            Pibox(kbb,0) = Pi3(id,0)
            Prbox(kbb,1) = Pr3(id,1)
            Pibox(kbb,1) = Pi3(id,1)
            Prbox(kbb,2) = Pr3(id,2)
            Pibox(kbb,2) = Pi3(id,2)
            Prbox(kbb,3) = Pr3(id,3)
            Pibox(kbb,3) = Pi3(id,3)
            Prbox(kbb,4) = Pr3(id,4)
            Pibox(kbb,4) = Pi3(id,4)
            Prbox(kbb,5) = Pr3(id,5)
            Pibox(kbb,5) = Pi3(id,5)
            Prbox(kbb,6) = Pr3(id,6)
            Pibox(kbb,6) = Pi3(id,6)
            Prbox(kbb,7) = Pr3(id,7)
            Pibox(kbb,7) = Pi3(id,7)
        25 END DO

        if (kfar > nbox_max) write(*,*)'error in rest3',kbb
        CALL int_box(Nmax3,kb,xb,yb,kfar,Br3,Bi3)

    20 END DO
    RETURN
    END SUBROUTINE 
