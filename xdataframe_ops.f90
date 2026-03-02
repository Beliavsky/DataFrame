program xdataframe_ops
use kind_mod, only: dp
use dataframe_mod, only: DataFrame, allocate_df, operator(+), operator(-), operator(*), operator(/)
implicit none
type(DataFrame) :: a, b, c
integer :: i, j
real(kind=dp), parameter :: tol = 1.0e-12_dp
real(kind=dp) :: want, errmax

call allocate_df(a, 3, 2, default_indices=.false., default_columns=.false.)
call allocate_df(b, 3, 2, default_indices=.false., default_columns=.false.)

a%index = [10, 20, 30]
b%index = a%index

a%columns(1) = "c1"
a%columns(2) = "c2"
b%columns = a%columns

do j = 1, 2
   do i = 1, 3
      a%values(i,j) = real(i + 10*j, kind=dp)
      b%values(i,j) = real(100 + i + 10*j, kind=dp)
   end do
end do

print*,"test strict df op df (+ - * /)"

c = a + b
errmax = 0.0_dp
do j = 1, 2
   do i = 1, 3
      want = a%values(i,j) + b%values(i,j)
      errmax = max(errmax, abs(c%values(i,j) - want))
   end do
end do
if (errmax > tol) error stop "add df df failed"

c = a - b
errmax = 0.0_dp
do j = 1, 2
   do i = 1, 3
      want = a%values(i,j) - b%values(i,j)
      errmax = max(errmax, abs(c%values(i,j) - want))
   end do
end do
if (errmax > tol) error stop "sub df df failed"

c = a * b
errmax = 0.0_dp
do j = 1, 2
   do i = 1, 3
      want = a%values(i,j) * b%values(i,j)
      errmax = max(errmax, abs(c%values(i,j) - want))
   end do
end do
if (errmax > tol) error stop "mul df df failed"

c = a / b
errmax = 0.0_dp
do j = 1, 2
   do i = 1, 3
      want = a%values(i,j) / b%values(i,j)
      errmax = max(errmax, abs(c%values(i,j) - want))
   end do
end do
if (errmax > tol) error stop "div df df failed"

if (any(c%index /= a%index)) error stop "index not preserved"
if (any(c%columns /= a%columns)) error stop "columns not preserved"

print*,"xdataframe_ops: PASS"
end program xdataframe_ops
