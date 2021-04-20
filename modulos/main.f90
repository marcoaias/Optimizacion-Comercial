program main
  use lib
  implicit none

  integer :: A(3,3), n

  real(8) :: q,b,c

  q = 0
  b = -1

  n = 1000

  A(1,:) = (/1,2,3/)
  A(2,:) = (/4,5,6/)
  A(3,:) = (/7,8,9/)

  call display_integer_matrix(A,3)

  call trapecio(q,b,c,n)
  print *, c + 0.5

  call simpson(q,b,n,c)
  print *, c + 0.5

end program
