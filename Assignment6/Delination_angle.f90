module Delination_angle
contains 
subroutine cal_delination_angle(N, delta)
implicit none
real(4) :: pi=3.1415926536
Integer,intent(in) :: N
real(4),intent(out) :: delta
delta=(23.45/180*pi)*sin((N+284)/365*2*pi)
print *, "delta=", delta
end subroutine cal_delination_angle
end module Delination_angle

 
! excellent work
