program Cal_SZA

use Delination_angle
use AST
implicit none
real(4) :: pi
real(4) :: H
real(4) :: L
real(4) :: zenith_angle, beta_1


integer :: N
real(4) :: delta
real(4) :: Long, LST
real(4) :: AST_time

N=355 

call cal_delination_angle(N, delta)
print *, "delta=", delta

Long=114.062996
LST=875

call cal_AST(Long, N, LST, AST_time)
print *, "AST_time=", AST_time

L=22.542883
pi=3.1415926536
H=(AST_time-720)/4
zenith_angle=ACOS(cos(pi*L/180)*cos(delta)*cos(pi*H/180)+sin(pi*L/180)*sin(delta))/pi*180
beta_1=ASIN(cos(pi*L/180)*cos(delta)*cos(pi*H/180)+sin(pi*L/180)*sin(delta))/pi*180
print *, "zenith angle=", zenith_angle
print *, "beta_1=", beta_1

end program Cal_SZA
 
! excellent work
