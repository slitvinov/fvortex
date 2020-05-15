c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        SUBROUTINE streamfunction(Xbc,Ybc,Sp,Sb)

C  This subroutine finds the streamfunction value at (xbc,ybc) induced
C  by the vorticity field, sending back contributions from particle
C  interactions (Sp) and box interactions (Sb).

        implicit none

        include 'tree_tmp.h'

        real xbc,ybc,sp,sb
        
        integer nn,kfp
c---------------------------------------------------------------------------

        call build_tree(0,xbc,ybc,nn,kfp)

        CALL STR_PART(xbc,ybc,nn,Sp)
        IF (nn.GT.np_max) then 
          write(*,*)'error in str_part, nn=',nn
          stop
        endif

        CALL STR_BOX(xbc,ybc,kfp,Sb)
        IF (kfp.GT.nbox_max) then
          write(*,*)'error in str_box, kfp=',kfp
          stop
        endif

900     RETURN
        END
