program xdataframe_align_ops
use kind_mod, only: dp
use dataframe_mod, only: DataFrame, allocate_df
use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
implicit none
type(DataFrame) :: x, y, z
real(kind=dp), parameter :: tol = 1.0e-12_dp
real(kind=dp) :: want, err
integer :: i, j

call allocate_df(x, 2, 2, default_indices=.false., default_columns=.false.)
call allocate_df(y, 2, 2, default_indices=.false., default_columns=.false.)

x%index = [1, 2]
x%columns(1) = "a"
x%columns(2) = "b"
x%values(:,1) = [1.0_dp, 3.0_dp]
x%values(:,2) = [2.0_dp, 4.0_dp]

y%index = [2, 3]
y%columns(1) = "b"
y%columns(2) = "c"
y%values(:,1) = [10.0_dp, 30.0_dp]
y%values(:,2) = [20.0_dp, 40.0_dp]

print*,"test aligned df member ops add/subtract/multiply/divide"

! default is outer union with missing -> NaN
z = x%add(y)

if (size(z%index) /= 3) error stop "add outer: nrow mismatch"
if (size(z%columns) /= 3) error stop "add outer: ncol mismatch"
if (any(z%index /= [1, 2, 3])) error stop "add outer: index mismatch"
if (trim(z%columns(1)) /= "a") error stop "add outer: col1 mismatch"
if (trim(z%columns(2)) /= "b") error stop "add outer: col2 mismatch"
if (trim(z%columns(3)) /= "c") error stop "add outer: col3 mismatch"

do j = 1, 3
   do i = 1, 3
      if (i == 2 .and. j == 2) then
         want = 14.0_dp
         err = abs(z%values(i,j) - want)
         if (err > tol) error stop "add outer: value mismatch at (2,b)"
      else
         if (.not. ieee_is_nan(z%values(i,j))) error stop "add outer: expected NaN"
      end if
   end do
end do

! outer with fill_value=0: missing treated as 0 (including NaNs)
z = x%add(y, fill_value=0.0_dp)

want = 0.0_dp
if (any(z%index /= [1, 2, 3])) error stop "add fill: index mismatch"
if (trim(z%columns(1)) /= "a") error stop "add fill: col1 mismatch"
if (trim(z%columns(2)) /= "b") error stop "add fill: col2 mismatch"
if (trim(z%columns(3)) /= "c") error stop "add fill: col3 mismatch"

! expected matrix:
! row 1: [1, 2, 0]
! row 2: [3, 14, 20]
! row 3: [0, 30, 40]
if (abs(z%values(1,1) - 1.0_dp) > tol) error stop "add fill: (1,a)"
if (abs(z%values(1,2) - 2.0_dp) > tol) error stop "add fill: (1,b)"
if (abs(z%values(1,3) - 0.0_dp) > tol) error stop "add fill: (1,c)"
if (abs(z%values(2,1) - 3.0_dp) > tol) error stop "add fill: (2,a)"
if (abs(z%values(2,2) - 14.0_dp) > tol) error stop "add fill: (2,b)"
if (abs(z%values(2,3) - 20.0_dp) > tol) error stop "add fill: (2,c)"
if (abs(z%values(3,1) - 0.0_dp) > tol) error stop "add fill: (3,a)"
if (abs(z%values(3,2) - 30.0_dp) > tol) error stop "add fill: (3,b)"
if (abs(z%values(3,3) - 40.0_dp) > tol) error stop "add fill: (3,c)"

! inner intersection
z = x%add(y, how="inner")
if (size(z%index) /= 1) error stop "add inner: nrow mismatch"
if (size(z%columns) /= 1) error stop "add inner: ncol mismatch"
if (z%index(1) /= 2) error stop "add inner: index mismatch"
if (trim(z%columns(1)) /= "b") error stop "add inner: column mismatch"
if (abs(z%values(1,1) - 14.0_dp) > tol) error stop "add inner: value mismatch"

z = x%subtract(y, how="inner")
if (abs(z%values(1,1) + 6.0_dp) > tol) error stop "subtract inner: value mismatch"

z = x%multiply(y, how="inner")
if (abs(z%values(1,1) - 40.0_dp) > tol) error stop "multiply inner: value mismatch"

z = x%divide(y, how="inner")
if (abs(z%values(1,1) - 0.4_dp) > tol) error stop "divide inner: value mismatch"

print*,"PASS"
end program xdataframe_align_ops
