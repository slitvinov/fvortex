!  This set of subroutines performs the various types of interactions
!  between particles and boxes that are called for in the tree routine.
! ---------------------------------------------------------------------

    SUBROUTINE int_part1 (xtest,ytest,gtest,upart,vpart,gpart,kpart)

!  This  subroutine calculates the velocities induced on the
!  particle located at *xtest*,*ytest*, by the particles in its
!  interaction list *XT,YT,GT*.
     
    implicit none

    include 'main_dim.h'
    include 'tree_tmp.h'

    real :: eps
    PARAMETER (eps = 1.e-09)

    integer :: np
    real :: s2,ovrlp,gnu
    COMMON/PART/Np,s2,ovrlp,gnu

    real :: gdelta,gauss(ngauss)
    COMMON/GAUSS_TABLE/gdelta,GAUSS

    real :: visc_cutoff
    common/cutoff/visc_cutoff

    integer :: kpart
    real :: xtest,ytest,gtest,upart,vpart,gpart

    integer :: m,i
    real :: s2inv2,xx,yy,r2,arg,C,fc,rad1
!------------------------------------------------------------------------------

    s2inv2 = 0.5/s2
    Upart = 0.0
    Vpart = 0.0
    gpart = 0.0
          
    DO 4 m = 1,kpart
        xx = xtest - XT(m)
        yy = YT(m) - ytest
        r2 = xx*xx + yy*yy
        arg = r2*s2inv2
        IF(arg < gauss_cut) then
            i=arg/gdelta + 1
            C = gauss(i)*(1 + i*gdelta - arg) ! includes first error term
        else
            C = 0.
        endif
        fc = GT(m) * (1.0 - C)/(r2+eps)
        Upart = Upart + yy*fc
        Vpart = Vpart + xx*fc
    !* rad1 = xtest*xtest + ytest*ytest
    !* if(rad1.lt.visc_cutoff)then
        gpart = gpart + (GT(m) - gtest) * C
    !*         endif
    4 END DO
          
    RETURN
    end SUBROUTINE int_part1
          
