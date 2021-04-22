program main
  use lib
  implicit none

  integer :: n, i
  real(8) :: t, mu, sigma, h, array(201, 2), A(6,6), v(5)

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


  A(1,:) = (/180, 14, 9, 0, 0, 0/)
  A(2,:) = (/130, 87, 8, 0, 0, 0/)
  A(3,:) = (/100, 89, 9, 0, 0, 0/)
  A(4,:) = (/80, 14, 9, 0, 0, 0/)
  A(5,:) = (/40, 60, 9, 0, 0, 0/)
  A(6,:) = (/0, 0, 0, 0, 0, 0/)

  call valores_emsr(A, 6)
  ! call display_matrix(A, 6)

  call proteger(A,6,v)



  ! print *, probabilidad(dble(0), dble(1), dble(2))



  ! output data into a file
   ! open(1, file = 'data5.dat', status = 'new')
   ! do i=1,n+1
   !    write(1,*) array(i,1), array(i,2)
   ! end do
   !
   ! close(1)

end program
