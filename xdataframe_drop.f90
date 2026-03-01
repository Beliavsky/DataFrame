program xdataframe_drop
! test has_col/has_idx/drop_cols/drop_rows/rename_cols
use util_mod, only: assert_equal
use dataframe_mod, only: DataFrame, nrow, ncol
implicit none

type(DataFrame) :: df, df2, df3
integer :: n0
integer :: idx1, idx2

call df%read_csv("spy_efa_eem_tlt.csv")

print*, "xdataframe_drop"
print*, "nrow=", nrow(df), " ncol=", ncol(df)

! has_col
if (.not. df%has_col("SPY")) error stop "has_col failed for SPY"
if (df%has_col("NO_SUCH_COLUMN")) error stop "has_col should be false for NO_SUCH_COLUMN"

! has_idx
idx1 = df%index(1)
if (.not. df%has_idx(idx1)) error stop "has_idx failed for first index"
if (df%has_idx(-123456)) error stop "has_idx should be false for missing index"

! rename_cols
call df%rename_cols(["SPY","EFA"], ["SPY2","EFA2"])
if (.not. df%has_col("SPY2")) error stop "rename_cols failed for SPY -> SPY2"
if (df%has_col("SPY")) error stop "rename_cols did not remove old name SPY"

! drop_cols
df2 = df%drop_cols(["EEM","TLT"])
call assert_equal(ncol(df2), 2, "drop_cols should leave 2 columns")
if (.not. df2%has_col("SPY2")) error stop "drop_cols lost SPY2"
if (df2%has_col("EEM")) error stop "drop_cols did not drop EEM"

! drop_cols with missing='ignore'
df3 = df2%drop_cols(["NO_SUCH_COLUMN"], missing="ignore")
call assert_equal(ncol(df3), ncol(df2), "drop_cols(ignore) changed ncol")

! drop_rows
n0 = nrow(df3)
idx1 = df3%index(1)
idx2 = df3%index(2)
df2 = df3%drop_rows([idx1, idx2])
call assert_equal(nrow(df2), n0 - 2, "drop_rows should drop 2 rows")
if (df2%has_idx(idx1)) error stop "drop_rows did not drop first index"

! drop_rows with missing='ignore'
df3 = df2%drop_rows([-999999], missing="ignore")
call assert_equal(nrow(df3), nrow(df2), "drop_rows(ignore) changed nrow")

print*, "xdataframe_drop: PASS"
end program xdataframe_drop
