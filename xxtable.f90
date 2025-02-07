program xxtable
use  kind_mod, only: dp
use table_mod, only: table, create_table
implicit none
integer, parameter :: n=4, ncol=3
real(kind=dp) :: x(n, ncol)
type(Table) :: y
call random_number(x)
x(:,3) = x(:,3)*10
y = create_table(x)
call y%display()
y = create_table(x, index=["d", "e", "f", "g"], &
    columns=["a", "b", "c"])
call y%display()
y = create_table(index=["a", "b", "c"], columns=["d", "e"])
call y%display()
y = create_table(index=["a", "b", "c"], columns=["d", "e"], value=-1.0_dp)
call y%display()
! test create_table with inconsistent dimensions
y = create_table(x, index=["d", "e", "f"], &
    columns=["a", "b", "c"])
y = create_table(x, index=["d", "e", "f", "g"], &
    columns=["a", "b"])
end program xxtable