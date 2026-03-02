program xdataframe_reindex
use kind_mod, only: dp
use dataframe_mod, only: DataFrame, allocate_df
use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
implicit none

call test_shift_pct_change
call test_reindex
print *, "xdataframe_reindex: PASS"

contains

subroutine assert_true(cond, msg)
logical, intent(in) :: cond
character(len=*), intent(in) :: msg
if (.not. cond) error stop msg
end subroutine assert_true

subroutine assert_close(x, y, tol, msg)
real(kind=dp), intent(in) :: x, y, tol
character(len=*), intent(in) :: msg
if (abs(x-y) > tol) error stop msg
end subroutine assert_close

subroutine test_shift_pct_change
type(DataFrame) :: df, d1, d2, pc
real(kind=dp), parameter :: fill = -999.0_dp
real(kind=dp), parameter :: tol = 1.0e-12_dp

call allocate_df(df, 4, 2, default_indices=.false., default_columns=.false.)

df%index = [10, 20, 30, 40]
df%columns(1) = "a"
df%columns(2) = "b"
df%values(:,1) = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp]
df%values(:,2) = [10.0_dp, 20.0_dp, 30.0_dp, 40.0_dp]

! shift down by 1

 d1 = df%shift(1, fill)
call assert_close(d1%values(1,1), fill, tol, "shift(1) fill")
call assert_close(d1%values(2,1), 1.0_dp, tol, "shift(1) value")
call assert_close(d1%values(4,2), 30.0_dp, tol, "shift(1) value col2")

! shift up by 1

 d2 = df%shift(-1, fill)
call assert_close(d2%values(4,1), fill, tol, "shift(-1) fill")
call assert_close(d2%values(1,1), 2.0_dp, tol, "shift(-1) value")
call assert_close(d2%values(1,2), 20.0_dp, tol, "shift(-1) value col2")

! pct_change

 pc = df%pct_change(1)
call assert_true(ieee_is_nan(pc%values(1,1)), "pct_change first row should be NaN")
call assert_close(pc%values(2,1), 1.0_dp, tol, "pct_change row2")
call assert_close(pc%values(3,1), 0.5_dp, tol, "pct_change row3")
call assert_close(pc%values(4,2), 40.0_dp/30.0_dp - 1.0_dp, tol, "pct_change col2")
end subroutine test_shift_pct_change

subroutine test_reindex
type(DataFrame) :: df, r0, rf, rb, rz
integer, allocatable :: new_index(:)
real(kind=dp), parameter :: tol = 1.0e-12_dp

call allocate_df(df, 3, 1, default_indices=.false., default_columns=.false.)

df%index = [1, 3, 4]
df%columns(1) = "x"
df%values(:,1) = [10.0_dp, 30.0_dp, 40.0_dp]

new_index = [0, 1, 2, 3, 4, 5]

r0 = df%reindex(new_index)
call assert_true(ieee_is_nan(r0%values(1,1)), "reindex none: idx 0 should be NaN")
call assert_close(r0%values(2,1), 10.0_dp, tol, "reindex none: idx 1")
call assert_true(ieee_is_nan(r0%values(3,1)), "reindex none: idx 2 should be NaN")
call assert_close(r0%values(4,1), 30.0_dp, tol, "reindex none: idx 3")

rf = df%reindex(new_index, method="ffill")
call assert_true(ieee_is_nan(rf%values(1,1)), "reindex ffill: idx 0 should be NaN")
call assert_close(rf%values(3,1), 10.0_dp, tol, "reindex ffill: idx 2")
call assert_close(rf%values(6,1), 40.0_dp, tol, "reindex ffill: idx 5")

rb = df%reindex(new_index, method="bfill")
call assert_close(rb%values(1,1), 10.0_dp, tol, "reindex bfill: idx 0")
call assert_close(rb%values(3,1), 30.0_dp, tol, "reindex bfill: idx 2")
call assert_true(ieee_is_nan(rb%values(6,1)), "reindex bfill: idx 5 should be NaN")

rz = df%reindex(new_index, fill_value=0.0_dp)
call assert_close(rz%values(1,1), 0.0_dp, tol, "reindex fill_value: idx 0")
call assert_close(rz%values(3,1), 0.0_dp, tol, "reindex fill_value: idx 2")
end subroutine test_reindex

end program xdataframe_reindex
