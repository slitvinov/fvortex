!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    SUBROUTINE POT_PART_UBD (xtest,ytest,kpart,up)

!  This subroutine finds the velocity potential induced at (xtest,ytest)
!  by the particles in XT,YT.

    implicit none

    include 'tree_tmp.h'

    integer :: kpart
    real :: xtest,ytest,up

    integer :: m
    real :: xa,ya,xv,yv,gv
!------------------------------------------------------------------------------

    Up = 0.0
          
    DO 4 m = 1,kpart
        gv = GT(m)
        xv = XT(m)
        yv = YT(m)
        xa = xtest - xv
        ya = ytest - yv

        up = up + gv * ATAN2(ya,xa)
    4 END DO
          
    RETURN
    END SUBROUTINE POT_PART_UBD
          
