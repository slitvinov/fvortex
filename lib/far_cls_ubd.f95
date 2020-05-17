!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    SUBROUTINE  FAR_CLS_UBD(icheck,Nmax,xtest,ytest,ds,IC,JC,kexam, &
    Xst,Yst,Listexam,kfar,Listfar,kclose,Listclose)

!  This  subroutine finds all the far & close boxes at a certain
!  level associated with a certain particle. The term icheck is needed
!  because the calculation requires special treatment near theta=90 due
!  to the arctan function so particles near here are not interacted
!  as boxes when trying to compute a term involving arctan.

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
!------------------------------------------------------------------------------

    lx = (xtest-x0)/ds + 1     ! Indices of box where particle is located
    ly = (ytest-y0)/ds + 1
    kfar = 0
    kclose = 0

    DO 2 k = 1,kexam
        ks = Listexam(k)
        ib = IC(ks)
        jb = JC(ks)
        IF ( (IABS(ib-lx) <= 1) &
         .AND. (IABS(jb-ly) <= 1) )THEN
            kclose = kclose + 1
            Listclose(kclose) = ks        ! close
        ELSE
            kfar = kfar + 1
            Listfar(kfar) = ks            ! far away
        ENDIF
    2 END DO


    RETURN
    END SUBROUTINE 
