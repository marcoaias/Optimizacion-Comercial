program main
  use lib
  implicit none

  integer :: A(3,3), n, i
  real(8) :: t, mu, sigma, h, array(201, 2)

  n = 200
  mu = -2.0
  sigma = 0.3

  h = 3*2*sigma/dble(n)



  do i = 0, n
    t = -3*sigma + i*h + mu



    array(i+1, 1) = t
    array(i+1, 2) = probabilidad(t, mu, sigma)

    print *, i, array(i+1, 1), array(i+1, 2)

  end do


  A(1,:) = (/1,2,3/)
  A(2,:) = (/4,5,6/)
  A(3,:) = (/7,8,9/)

  call display_integer_matrix(A,3)

  print *, probabilidad(dble(0), dble(1), dble(2))



  ! output data into a file
   open(1, file = 'data4.dat', status = 'new')
   do i=1,n+1
      write(1,*) array(i,1), array(i,2)
   end do

   close(1)

end program
