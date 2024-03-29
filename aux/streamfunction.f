      subroutine streamfunction(Xbc, Ybc, Sp, Sb)

C This subroutine finds the streamfunction value at (xbc,ybc) induced by
C the vorticity field, sending back contributions from particle
C interactions (Sp) and box interactions (Sb).

      include 'tree_tmp.h'

      real  xbc, ybc, sp, sb

      integer  nn, kfp

      call build_tree(0, xbc, ybc, nn, kfp)

      call str_part(xbc, ybc, nn, Sp)
      if (nn > np_max) then
         write (*, *) 'error in str_part, nn=', nn
         stop
      endif

      call str_box(xbc, ybc, kfp, Sb)
      if (kfp > nbox_max) then
         write (*, *) 'error in str_box, kfp=', kfp
         stop
      endif

  900 continue
      end
