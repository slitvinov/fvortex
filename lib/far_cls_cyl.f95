!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    SUBROUTINE  FAR_CLS(icheck,Nmax,xtest,ytest,ds,IC,JC,kexam, &
    Xst,Yst,Listexam,kfar,Listfar,kclose,Listclose)

!  This  subroutine finds all the far & close boxes at a certain
!  level associated with a certain particle. The term icheck is needed
!  because the calculation requires special treatment near theta=90 due
!     to the arctan function so particles near here are not interacted
!     as boxes when trying to compute a term involving arctan.

    implicit none

    include 'tree_tmp.h'

    integer :: limpar
    real :: x0,y0
    COMMON/GEOM/X0,Y0,Limpar

    integer :: icheck,nmax,kexam,kfar,kclose
    integer :: Listfar(Nhlp),Listclose(Nhlp),Listexam(Nhlp)
    integer :: IC(Nmax),JC(Nmax)
    real :: xtest,ytest,ds,xst,yst

    integer :: k,ib,jb,ks,lx,ly
    real :: dshaf,ax,radius,ay,radius_ds
!------------------------------------------------------------------------------

    lx = (xtest-x0)/ds + 1     ! Indices of box where particle is located
    ly = (ytest-y0)/ds + 1
    dshaf = 0.5001*ds
    kfar = 0
    kclose = 0
    radius = 1.                ! assumed Rcyl=1. here
    radius_ds = radius + dshaf

    DO 2 k = 1,kexam
        ks = Listexam(k)
        ib = IC(ks)
        jb = JC(ks)
        ax = Xst + ib*ds
        ay = yst + jb*ds
        IF ( (IABS(ib-lx) <= 1) &
         .AND. (IABS(jb-ly) <= 1) )THEN
            kclose = kclose + 1
            Listclose(kclose) = ks        ! close
        ELSE IF((icheck == 1) .AND. &
            (abs(ay) <= radius_ds) .AND. &
            (ax <= radius_ds) ) THEN
            kclose = kclose + 1
            Listclose(kclose) = ks        ! close
        ELSE
            kfar = kfar + 1
            Listfar(kfar) = ks            ! far away
        ENDIF
    2 END DO


    RETURN
    END SUBROUTINE 
