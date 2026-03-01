program xdataframe_na
! test NaN handling of dataframe
use kind_mod, only: dp
use util_mod, only: assert_equal
use dataframe_mod, only: DataFrame, nrow, ncol
implicit none

type(DataFrame) :: df, df_any, df_all, df_cols, df_rows
logical, allocatable :: mask(:,:)
character(len=*), parameter :: fname = "tmp_na_test.csv"
integer :: unit, io, k
real(kind=dp) :: tol

tol = 1.0d-12

! create a CSV with missing values and NA strings
open(newunit=unit, file=fname, status="replace", action="write", iostat=io)
if (io /= 0) error stop "cannot open tmp_na_test.csv for writing"

write(unit, "(a)") ",A,B,C,D,E"
write(unit, "(a)") "1,1,2,3,100,NA"
write(unit, "(a)") "2,,5,6,101,NA"
write(unit, "(a)") "3,NA,,9,102,NA"
write(unit, "(a)") "4,10,11,NaN,103,NA"
write(unit, "(a)") "5,12,null,,104,NA"
write(unit, "(a)") "6,,,,105,NA"
write(unit, "(a)") "7,20,21,22,106,NA"
close(unit)

! read and count NaNs
call df%read_csv(fname)
mask = df%isna()
k = count(mask)
call assert_equal(k, 16, "count(isna)")

! drop columns with any NaN: should keep only D
df_any = df%dropna(axis="cols", how="any")
call assert_equal(ncol(df_any), 1, "ncol(dropna cols any)")
call assert_equal(nrow(df_any), 7, "nrow(dropna cols any)")
if (trim(df_any%columns(1)) /= "D") error stop "dropna(cols,any): expected column D"

! drop columns with all NaN: should drop only E
df_all = df%dropna(axis="cols", how="all")
call assert_equal(ncol(df_all), 4, "ncol(dropna cols all)")
if (trim(df_all%columns(1)) /= "A") error stop "dropna(cols,all): expected column A first"
if (trim(df_all%columns(4)) /= "D") error stop "dropna(cols,all): expected column D last"

! drop rows with any NaN after removing column E (all-NA column)
df_cols = df%dropna(axis="cols", how="all")
df_rows = df_cols%dropna(axis="rows", how="any")
call assert_equal(nrow(df_rows), 2, "nrow(dropna rows any after dropping all-NA cols)")
if (df_rows%index(1) /= 1) error stop "dropna(rows,any): expected first index 1"
if (df_rows%index(2) /= 7) error stop "dropna(rows,any): expected second index 7"

! fill with constant
call df%read_csv(fname)
call df%fillna(value=0.0_dp)
mask = df%isna()
call assert_equal(count(mask), 0, "count(isna) after fillna(value)")

if (abs(df%values(2,1) - 0.0_dp) > tol) error stop "fillna(value): A at row 2 expected 0"
if (abs(df%values(4,3) - 0.0_dp) > tol) error stop "fillna(value): C at row 4 expected 0"
if (abs(df%values(1,5) - 0.0_dp) > tol) error stop "fillna(value): E at row 1 expected 0"

! forward fill
call df%read_csv(fname)
call df%fillna(method="ffill")
mask = df%isna()
call assert_equal(count(mask), 7, "count(isna) after fillna(ffill) (column E remains all NA)")

if (abs(df%values(2,1) - 1.0_dp) > tol) error stop "fillna(ffill): A at row 2 expected 1"
if (abs(df%values(3,2) - 5.0_dp) > tol) error stop "fillna(ffill): B at row 3 expected 5"
if (abs(df%values(4,3) - 9.0_dp) > tol) error stop "fillna(ffill): C at row 4 expected 9"
if (abs(df%values(6,3) - 9.0_dp) > tol) error stop "fillna(ffill): C at row 6 expected 9"

! backward fill
call df%read_csv(fname)
call df%fillna(method="bfill")
mask = df%isna()
call assert_equal(count(mask), 7, "count(isna) after fillna(bfill) (column E remains all NA)")

if (abs(df%values(2,1) - 10.0_dp) > tol) error stop "fillna(bfill): A at row 2 expected 10"
if (abs(df%values(3,1) - 10.0_dp) > tol) error stop "fillna(bfill): A at row 3 expected 10"
if (abs(df%values(5,2) - 21.0_dp) > tol) error stop "fillna(bfill): B at row 5 expected 21"
if (abs(df%values(6,2) - 21.0_dp) > tol) error stop "fillna(bfill): B at row 6 expected 21"
if (abs(df%values(4,3) - 22.0_dp) > tol) error stop "fillna(bfill): C at row 4 expected 22"

print *, "xdataframe_na: PASS"

end program xdataframe_na
