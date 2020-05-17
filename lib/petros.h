subroutine condiff(Npart,islip,visc_rmax,istats)
  implicit none
  integer :: npart
  integer :: islip
  integer :: istats
  real :: visc_rmax
end subroutine
subroutine initial(Rmax,ell_x,ell_y)
  implicit none
  real :: rmax
  real :: ell_x
  real :: ell_y
end subroutine
subroutine mv_ab(irk)
  implicit none
  integer :: irk
end subroutine
subroutine mv_rk(visc_rmax)
  implicit none
  real :: visc_rmax
end subroutine
subroutine read_restart(time,np,s2,ovrlp,nvort,xp,yp,gp)
  implicit none
  integer :: np
  integer :: nvort
  real :: time
  real :: s2
  real :: ovrlp
  real, dimension(nvort) :: xp
  real, dimension(nvort) :: yp
  real, dimension(nvort) :: gp
end subroutine
subroutine vel_ext(tm)
  implicit none
  real :: tm
end subroutine
subroutine vort_field(iframe)
  implicit none
  integer :: iframe
end subroutine
subroutine write_restart(time,np,s2,ovrlp,nvort,xp,yp,gp)
  implicit none
  integer :: np
  integer :: nvort
  real :: time
  real :: s2
  real :: ovrlp
  real, dimension(nvort) :: xp
  real, dimension(nvort) :: yp
  real, dimension(nvort) :: gp
end subroutine
