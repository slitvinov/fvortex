program main
  real, dimension(5) :: x
  integer :: n
  n = 5
  do i = 1, n
     x(i) = i
  end do
  call f(n, x)
end program main
