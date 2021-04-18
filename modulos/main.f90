program main
  use lib
  implicit none

  integer :: A(3,3)


  A(1,:) = (/1,2,3/)
  A(2,:) = (/4,5,6/)
  A(3,:) = (/7,8,9/)

  call display_integer_matrix(A,3)

end program
