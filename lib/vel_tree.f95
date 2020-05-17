!  This group of routines is used to get various outputs using the
!  interaction tree.
!----------------------------------------------------------------------

    SUBROUTINE vel_tree(Xbc,Ybc,u,v)

!  Driver for calculating the velocity at xbc,ybc from the vorticity
!  field using the fast tree structure.

    implicit none

    include 'tree_tmp.h'

    real :: xbc,ybc,u,v

    integer :: nn,kfp
    real :: upart,vpart,ubox,vbox
!------------------------------------------------------------
       
    call build_tree(0,xbc,ybc,nn,kfp)

    CALL vel_PART(xbc,ybc,upart,vpart,nn)
    IF (nn > np_max) then
        write(*,*)'error in vel_part, nn=',nn
        stop
    endif

    CALL vel_BOX(xbc,ybc,ubox,vbox,kfp)
    IF (kfp > nbox_max) then
        write(*,*)'error in vel_box, kfp=',kfp
        stop
    endif

    u = upart + ubox
    v = vpart + vbox
                    
    900 RETURN
    end SUBROUTINE vel_tree
