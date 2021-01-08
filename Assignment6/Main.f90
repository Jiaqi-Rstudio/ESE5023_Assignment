Program Main

implicit none

integer :: i
real(4), dimension(4) :: a, b, c
real(4), dimension(3,4) :: M
real(4), dimension(3,3) :: N
real(4), dimension(3,4) :: T

open(3, file='M.dat', status='old')

do i=1,4
read(3, *) a(i), b(i), c(i)
M(1,i)=a(i)
M(2,i)=b(i)
M(3,i)=c(i)
enddo
close(3)

do i=1,4
write(*,*) M(:,i)
enddo

open(3, file='N.dat', status='old')
do i=1,3
read(3, *) a(i),b(i), c(i)
N(1,i)=a(i)
N(2,i)=b(i)
N(3,i)=c(i)
enddo
close(3)

do i=1,3
write(*,*) N(:,i)
enddo

call Matrix_multip(M, N, T)
open(3, file='MN.dat', status='replace')
do i=1,4
write(3, '(f8.1,f8.1,f8.1,f8.1)') T(:,i)
enddo
close(3)

End Program Main
 
! excellent work
