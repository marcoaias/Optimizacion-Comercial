module lib
  implicit none

contains

  subroutine display_matrix(A,n)
    implicit none
    integer, intent(in) :: n
    real(8), intent(in) :: A(n,n)


    integer :: i

    do i = 1, n
      print *, A(i,:)
    end do
  end subroutine

  subroutine display_integer_matrix(A,n)
    implicit none
    integer, intent(in) :: n
    integer, intent(in) :: A(n,n)


    integer :: i

    do i = 1, n
      print *, A(i,:)
    end do
  end subroutine
end module
