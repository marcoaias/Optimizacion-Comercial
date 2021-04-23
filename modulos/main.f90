program main
  use lib
  implicit none

  integer :: n, i
  real(8) :: t, mu, sigma, h, array(251, 2), A(5,6), v(5), B(4,6)

  n = 250
  mu = 0.0
  sigma = 0.5

  h = 1.5*3*2*sigma/dble(n)



  do i = 0, n
    t = -3*sigma + i*h + mu



    array(i+1, 1) = t
    array(i+1, 2) = probabilidad(t, mu, sigma)

    ! print *, i, array(i+1, 1), array(i+1, 2)

  end do

  ! datos de la tabla de valores EMSR b

  A(1,:) = (/180, 14, 9, 0, 0, 0/)
  A(2,:) = (/130, 87, 8, 0, 0, 0/)
  A(3,:) = (/100, 89, 9, 0, 0, 0/)
  A(4,:) = (/80, 14, 9, 0, 0, 0/)
  A(5,:) = (/40, 60, 9, 0, 0, 0/)


  ! prueba de congruencia

  ! referencia:
  ! https://youtu.be/mZY4CU05PLw

  B(1,:) = (/500, 16, 5, 0, 0, 0/)
  B(2,:) = (/420, 44, 15, 0, 0, 0/)
  B(3,:) = (/290, 35, 11, 0, 0, 0/)
  B(4,:) = (/125, 0, 0, 0, 0, 0/)



  call valores_emsr(A, 5)

  call proteger(A,5,v)


  print *, "MATRIZ VALORES EMSR: "

  call show_array(A, 5, 6)

  print *, "NIVELES DE PROTECCION"

  call show_array(v,5, 1)




  ! print *, probabilidad(dble(0), dble(1), dble(2))

  ! call random_number(h)
  ! print *, h
  !
  ! call random_number(h)
  ! print *, h

  ! output data into a file



   ! open(1, file = 'data2.dat', status = 'new')
   ! do i=1,n+1
   !    write(1,*) array(i,1), array(i,2)
   ! end do
   !
   ! close(1)

end program
