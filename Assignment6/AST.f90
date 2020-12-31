module AST
contains
subroutine cal_AST(Long, N, LST, AST_time)
implicit none
real(4) :: pi=3.1415926536
integer :: LSTM
real(4),intent(in) :: Long
real(4) :: D
real(4) :: ET
integer,intent(in) :: N
real(4),intent(in) :: LST
real(4),intent(out) :: AST_time
LSTM=15*INT(Long/15)
print *, "LSTM=", LSTM
D=2*pi*(N-81)/365
ET=9.87*sin(2*D)-7.53*cos(D)-1.5*sin(D)
print *, "D=", D
print *, "ET=", ET
AST_time=LST+4*(LSTM-Long)+ET
print *, "AST_time=", AST_time
end subroutine cal_AST

end module AST

