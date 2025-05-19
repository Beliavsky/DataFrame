program xcbind
! test cbind
use util_mod
use kind_mod, only: dp
integer, parameter :: n1=4
real(kind=dp) :: x(n1, 3), y(n1, 2), v(n1)
real(kind=dp), allocatable :: z(:,:)
call random_number(x)
call random_number(y)
call random_number(v)
call display(x, title="x")
call display(v, title="v")
z = cbind(x, v)
call display(cbind(x, v), title="cbind(x, v)")
call display(y, title="y")
call display(cbind(x, y), title="cbind(x, y)")
call display(cbind(v, 10*v), title="cbind(v, 10*v)")
end program xcbind