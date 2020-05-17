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
subroutine write_restart(ivalue,time,np,s2,ovrlp,nvort,xp,yp,gp)
  implicit none
  integer :: ivalue
  integer :: np
  integer :: nvort
  real :: time
  real :: s2
  real :: ovrlp
  real, dimension(nvort) :: xp
  real, dimension(nvort) :: yp
  real, dimension(nvort) :: gp
end subroutine
