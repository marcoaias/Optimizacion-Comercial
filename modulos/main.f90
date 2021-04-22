program main
  use lib
  implicit none

  integer :: n, i
  real(8) :: t, mu, sigma, h, array(201, 2), A(5,6), v(3), B(4,6)

  n = 200
  mu = 0.0
  sigma = 1.0

  h = 3*2*sigma/dble(n)



  do i = 0, n
    t = -3*sigma + i*h + mu



    array(i+1, 1) = t
    array(i+1, 2) = 1 - probabilidad(t, mu, sigma)

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



  call valores_emsr(B, 4)

  call proteger(B,4,v)


  print *, "MATRIZ VALORES EMSR: "

  call show_array(B, 4, 6)

  print *, "NIVELES DE PROTECCION"

  call show_array(v,3, 1)




  ! print *, probabilidad(dble(0), dble(1), dble(2))



  ! output data into a file


   ! open(1, file = 'data.csv', status = 'new')
   ! do i=1,n+1
   !    write(1,*) array(i,1), ",", array(i,2)
   ! end do
   !
   ! close(1)

end program
