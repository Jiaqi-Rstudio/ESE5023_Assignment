subroutine Matrix_multip(M, N, T)
implicit none
real(4), dimension(3,4) :: M
real(4), dimension(3,3) :: N
real(4), dimension(3,4) :: T
integer :: i, j, k
real(4) :: value
do j=1,3
do i=1,4
value=0
do k=1,3
value=value+N(j,k)*M(k,i)
enddo
T(j,i)=value
enddo
enddo
end subroutine Matrix_multip

