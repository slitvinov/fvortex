c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        SUBROUTINE potential(Xbc,Ybc,potp,potb)

C  This subroutine finds the value of the velocity potential at
C  (xbc,ybc) induced by the vorticity field and returns the contribution
C  from the particles (potp) and boxes (potb).

        implicit none

        include 'tree_tmp.h'

        real xbc,ybc,potp,potb

        integer nn,kfp
c--------------------------------------------------------

        call build_tree(1,xbc,ybc,nn,kfp)

        CALL POT_PART(xbc,ybc,nn,potp)
        IF (nn.GT.np_max)THEN
          write(*,*)'error in pot_part, nn=',nn
          STOP
        ENDIF

        CALL POT_BOX(xbc,ybc,kfp,potb)
        IF (kfp.GT.nbox_max) then
          write(*,*)'error in pot_box, kfp=',kfp
          stop
        endif

900     RETURN
        END
