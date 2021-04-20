module lib
  implicit none

contains

  subroutine display_matrix(A,n)

    ! Muestra matrices cuadradas de reales por pantalla

    implicit none
    integer, intent(in) :: n
    real(8), intent(in) :: A(n,n)


    integer :: i

    do i = 1, n
      print *, A(i,:)
    end do
  end subroutine

  subroutine display_integer_matrix(A,n)

    ! Muestra matrizes cuadradas de enteros por pantalla

    implicit none
    integer, intent(in) :: n
    integer, intent(in) :: A(n,n)


    integer :: i

    do i = 1, n
      print *, A(i,:)
    end do
  end subroutine


  function gaussNormal(x) result(f)

    ! gaussNormal(x) devuelve el valor de la distribución normal Gaussiana
    ! para cada x pedida. La distribución es (0,1).

    implicit none
    real(8), intent(in) :: x
    real(8) :: f

    f = exp(-0.5 * x**2)/(sqrt(2*acos(-1.0)))

  end function

end module
