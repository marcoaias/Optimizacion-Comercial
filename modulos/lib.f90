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

  subroutine show_array(A, m, n)
    implicit none

    integer, intent(in) :: m, n
    real(8), intent(in) :: A(m,n)

    integer :: i

    do i = 1, m
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


  function probabilidad(t, mu, sigma) result(P)
    implicit none

    real(8), intent(in) :: t, mu, sigma
    real(8) :: P, s

    call integralCDF(mu, t, 1000, s, sigma)

    P = 0.5 + s

  end function


  subroutine integralCDF(mu,t,n,s,sigma)

    ! Según el método de simpson se integra la probabilidad compuesta en función a mu y sigma
    ! Función de probabilidad acumulada, CDF

    implicit none

    integer, intent(in) :: n
    real(8), intent(in) :: t, mu, sigma
    real(8), intent(inout) :: s

    real(8) :: s1, s2, h, x
    integer :: i

    h = (t-mu)/dble(n)

    s1 = 0d0
    do i = 1, n-1,2
      x = mu + h*dble(i)
      s1 = s1 + gauss(mu, sigma, x)
    end do

    s2 = 0d0
    do i = 2, n-2, 2
      x = mu + h*dble(i)
      s2 = s2 + gauss(mu, sigma, x)
    end do

    s = (h/dble(3))*(gauss(mu, sigma, mu) + gauss(mu, sigma, t) + dble(4)*s1 + dble(2)*s2)

  end subroutine



  ! Esta subrutina calcula los valores emsr relevantes, según el algoritmo EMSR-b, desde y hacia la tabla A(filas, 6), ordenada descendentemente en función de las tarifas de las clases.

  ! La tabla A() se usará para calcular los niveles de protección para cada clase.

  subroutine valores_emsr(A, filas)
    implicit none

    integer, intent(in) :: filas
    real(8), intent(inout) :: A(filas, 6)

    integer :: i, k
    real(8) :: s

    do i = 1, filas

      s = 0.d0
      do k = 1, i
        s = s + A(k, 2)
      end do

      A(i,5) = s

      s = 0.d0
      do k = 1, i
        s = s + A(k, 1)*A(k, 2)
      end do

      A(i,4) = s/A(i,5)

      s = 0.d0
      do k = 1, i
        s = s + A(k, 3)*A(k, 3)
      end do

      A(i, 6) = sqrt(s)


    end do
  end subroutine


  ! aplica la regla de Littlewood; compara cada clase al conjunto restante según el algoritmo EMSRb;

  ! recibe como parámetros la tabla de los valores emsr calculados en la subrutina valores_emsr, el número de filas (clases) que contiene la tabla, y el vector solución v que contiene los niveles de protección para cada clase comparado con el resto.

  ! véase:
  ! https://youtu.be/mZY4CU05PLw

  subroutine proteger (A, filas, v)
    implicit none

    integer, intent(in) :: filas
    real(8), intent(in) :: A(filas, 6)
    real(8), intent(inout) :: v(filas)

    integer :: i, j
    real(8) :: s, x, tol, dx

    v = 0.d0

    tol = dble(0.0000000001)



    do i = 1, filas-1

      ! solución inicial centrada en mu siempre garantiza convergencia

      x = A(i,5)

      ! resolución iterativa según la linealización del complementario cdf, quasi método de Newton

      ! solución inicial centrada alrededor de mu, en media desviación estandar.

      do j = 1, 50
        s = A(i+1, 1)/A(i, 4) - 1 + probabilidad(x, A(i,5), A(i,6))

        ! print *, "x: ", x, "s: ", s

        if ( abs(s) < tol ) then
          v(i) = x
          print *, i, v(i), j

          print *, A(i+1, 1)/A(i, 4)
          print *, 1 - probabilidad(v(i), A(i,5), A(i,6))
          exit
        end if



        ! pendiente negativa ojo TODO
        dx = -gauss(A(i,5), A(i,6), x)

        ! print *, dx
        ! print *, i,j,x
        x = s/dx + x
        ! print *, i,j,x

      end do

    end do


    ! Ghost class

    x = A(filas, 5)

    do j = 1, 100
      s = 0.1 - 1 + probabilidad(x, A(filas,5), A(filas,6))

      ! print *, "x: ", x, "s: ", s

      if ( abs(s) < tol ) then
        v(filas) = x
        print *, "GHOST CLASS"
        print *, filas, v(filas), j

        print *, 0.1
        print *, 1 - probabilidad(v(filas), A(filas,5), A(filas,6))
        exit
      end if



      ! pendiente negativa ojo TODO
      dx = -gauss(A(filas,5), A(filas,6), x)

      ! print *, dx
      ! print *, i,j,x
      x = s/dx + x
      ! print *, i,j,x

    end do



  end subroutine



end module
