!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    SUBROUTINE potential(Xbc,Ybc,potp,potb)

!  This subroutine finds the value of the velocity potential at
!  (xbc,ybc) induced by the vorticity field and returns the contribution
!  from the particles (potp) and boxes (potb).

    implicit none

    include 'tree_tmp.h'

    real :: xbc,ybc,potp,potb

    integer :: nn,kfp
!--------------------------------------------------------

    call build_tree(1,xbc,ybc,nn,kfp)

    CALL POT_PART(xbc,ybc,nn,potp)
    IF (nn > np_max)THEN
        write(*,*)'error in pot_part, nn=',nn
        STOP
    ENDIF

    CALL POT_BOX(xbc,ybc,kfp,potb)
    IF (kfp > nbox_max) then
        write(*,*)'error in pot_box, kfp=',kfp
        stop
    endif

    900 RETURN
    end SUBROUTINE potential
