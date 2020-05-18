subroutine f(n, x)
  implicit none
  integer :: n
  real, dimension(n) :: x
  integer :: i
  do i = 1, n
     print *, x(i)
  end do
end subroutine f
