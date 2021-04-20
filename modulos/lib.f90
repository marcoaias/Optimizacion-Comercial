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

  function gauss(mu, sigma, x) result(f)

    ! funcion gauss, media, desviación, x, f(x)

    implicit none
    real(8), intent(in) :: mu, sigma, x
    real(8) :: f

    f = exp(-0.5 * ((x-mu)/sigma)**2)/(sigma*sqrt(2*acos(-1.0)))
  end function


  function f1(x)

    ! f1 es la función a integrar por el método del trapecio en la subrutina trapecio

    implicit none

    real(8), intent(in) :: x
    real(8) :: f1

    f1 = gaussNormal(x)

  end function

  subroutine trapecio(a,b,c,n)
    implicit none

    real(8), intent(in) :: a, b
    real(8), intent(inout) :: c
    integer, intent(in) :: n

    real(8) :: s, x, h
    integer :: i

    ! cálculo del intervalo
    h = (b-a)/dble(n)

    s = 0d0

    do i = 1, n-1
      x = a + h*dble(i)

      ! f1 es la funcion f(x) a integrar
      ! f1 está definida en este módulo
      s = s + f1(x)
    end do

    c = 0.5*h*(f1(a) + f1(b) + 2*s)

  end subroutine

  function f2(x)
    implicit none

    real(8), intent(in) :: x
    real(8) :: f2

    f2 = gaussNormal(x)
  end function


  subroutine simpson(a,b,n,s)

    ! calculo de la integral de f2, f(x), por el método de simpson. límites de integración, número de intervalos, resultado.

    implicit none

    integer, intent(in) :: n
    real(8), intent(in) :: a, b
    real(8), intent(inout) :: s

    real(8) :: s1, s2, h, x
    integer :: i

    h = (b-a)/dble(n)

    s1 = 0d0
    do i = 1, n-1,2
      x = a + h*dble(i)
      s1 = s1 + f2(x)
    end do

    s2 = 0d0
    do i = 2, n-2, 2
      x = a + h*dble(i)
      s2 = s2 + f2(x)
    end do

    s = (h/dble(3))*(f2(a) + f2(b) + dble(4)*s1 + dble(2)*s2)

  end subroutine




end module
