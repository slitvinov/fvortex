subroutine far_cls_ubd(icheck, Nmax, xtest, ytest, ds, ic, jc, kexam, &
                       Xst, Yst, listexam, kfar, Listfar, kclose, Listclose)

!  This  subroutine finds all the far & close boxes at a certain
!  level associated with a certain particle. The term icheck is needed
!  because the calculation requires special treatment near theta=90 due
!  to the arctan function so particles near here are not interacted
!  as boxes when trying to compute a term involving arctan.

   implicit none

   include 'tree_tmp.h'

   integer :: limpar
   real :: x0, y0
   common/geom/x0, y0, Limpar

   integer :: icheck, nmax, kexam, kfar, kclose
   integer :: Listfar(Nhlp), Listclose(Nhlp), listexam(Nhlp)
   integer :: ic(Nmax), jc(Nmax)
   real :: xtest, ytest, ds, xst, yst

   integer :: k, ib, jb, ks, lx, ly
!------------------------------------------------------------------------------

   lx = (xtest - x0)/ds + 1     ! Indices of box where particle is located
   ly = (ytest - y0)/ds + 1
   kfar = 0
   kclose = 0

   do 2 k = 1, kexam
      ks = listexam(k)
      ib = ic(ks)
      jb = jc(ks)
      if ((iabs(ib - lx) <= 1) &
          .and. (iabs(jb - ly) <= 1)) then
         kclose = kclose + 1
         Listclose(kclose) = ks        ! close
      else
         kfar = kfar + 1
         Listfar(kfar) = ks            ! far away
      endif
2  end do

   return
end subroutine
