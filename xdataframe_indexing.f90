program xdataframe_append
use kind_mod, only: dp
use dataframe_mod, only: DataFrame, random, display
use util_mod, only: cbind
implicit none
type(DataFrame) :: df
logical, parameter :: cause_error = .false.
integer :: i, j
real(kind=dp) :: x1, x2, xold11, xold31

call random(df, 3, 2)

print*,"test scalar indexing (row_pos/col_pos/at/iat/set_at/set_iat)"
i = df%row_pos(2)
j = df%col_pos("C2")
if (i /= 2) error stop "row_pos test failed"
if (j /= 2) error stop "col_pos test failed"

x1 = df%iat(i, j)
x2 = df%at(2, "C2")
if (x1 /= x2) error stop "iat/at mismatch test failed"

xold11 = df%iat(1, 1)
xold31 = df%iat(3, 1)
call df%set_iat(1, 1, -1.0_dp)
if (df%iat(1, 1) /= -1.0_dp) error stop "set_iat test failed"
call df%set_at(3, "C1", -2.0_dp)
if (df%at(3, "C1") /= -2.0_dp) error stop "set_at test failed"

call df%set_iat(1, 1, xold11)
call df%set_at(3, "C1", xold31)
print*,"scalar indexing: PASS"

call display(df)

call df%append_col("10*C2", 10*df%values(:,2))
if (df%col_pos("10*C2") /= 3) error stop "col_pos test failed for appended column 10*C2"
if (df%at(2, "10*C2") /= df%iat(2, 3)) error stop "at/iat mismatch for appended column 10*C2"
call display(df)

call df%set_col("C1", 100*df%values(:,1))
call display(df)

call df%set_col("C3", 10*df%values(:,1))
if (df%col_pos("C3") /= 4) error stop "col_pos test failed for new column C3"
call df%set_at(2, "C3", 999.0_dp)
if (df%iat(2, 4) /= 999.0_dp) error stop "set_at test failed for C3"
call df%set_iat(2, 4, 10.0_dp*df%iat(2, 1)) ! restore original formula for row 2
call display(df)

call df%append_cols(["2*C1", "3*C1"], cbind(df%values(:,1)*2, df%values(:,1)*3))
if (df%col_pos("2*C1") /= 5) error stop "col_pos test failed for 2*C1"
if (df%col_pos("3*C1") /= 6) error stop "col_pos test failed for 3*C1"
call display(df)

if (cause_error) then
   print*,"cause run-time error:"
   call df%set_col("C3", [10.0d0, 20.0d0])  ! will cause run-time error
   call df%append_col("abc", [10.0d0, 20.0d0]) ! will cause run-time error
   print*,"and scalar indexing errors:"
   x1 = df%at(100, "C1") ! missing index -> error stop
end if

call display(df)
print "(/,a)", "(1) finished xdataframe_indexing.f90"
end program xdataframe_append
