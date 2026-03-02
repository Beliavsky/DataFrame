program xdataframe_select
use kind_mod, only: dp
use dataframe_mod, only: DataFrame, nrow, ncol
implicit none

type(DataFrame) :: df, df2
integer, allocatable :: idx(:)
character(len=100), allocatable :: cols(:)
real(kind=dp), allocatable :: vals(:,:)
logical, allocatable :: mr(:), mc(:)
integer, allocatable :: ivec(:), jvec(:)
real(kind=dp), parameter :: tol = 1.0e-12_dp

idx = [10, 20, 30, 40]
cols = ["A", "B", "C"]

allocate(vals(4,3))
vals = reshape([ &
   1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, &
   10.0_dp, 20.0_dp, 30.0_dp, 40.0_dp, &
   100.0_dp, 200.0_dp, 300.0_dp, 400.0_dp ], [4,3])

df = DataFrame(index=idx, columns=cols, values=vals)

! where_cols: keep A and C
mc = [.true., .false., .true.]
df2 = df%where_cols(mc)
if (ncol(df2) /= 2) error stop "where_cols: ncol must be 2"
if (trim(df2%columns(1)) /= "A") error stop "where_cols: col(1) must be A"
if (trim(df2%columns(2)) /= "C") error stop "where_cols: col(2) must be C"
if (abs(df2%values(4,2) - 400.0_dp) > tol) error stop "where_cols: value mismatch"

! filter_cols: keep only B (drop omitted => keep mask==true)
mc = [.false., .true., .false.]
df2 = df%filter_cols(mc)
if (ncol(df2) /= 1) error stop "filter_cols keep: ncol must be 1"
if (trim(df2%columns(1)) /= "B") error stop "filter_cols keep: col must be B"
if (abs(df2%values(3,1) - 30.0_dp) > tol) error stop "filter_cols keep: value mismatch"

! filter_cols: drop B
df2 = df%filter_cols(mc, drop=.true.)
if (ncol(df2) /= 2) error stop "filter_cols drop: ncol must be 2"
if (trim(df2%columns(1)) /= "A") error stop "filter_cols drop: col(1) must be A"
if (trim(df2%columns(2)) /= "C") error stop "filter_cols drop: col(2) must be C"

! where: keep rows 1 and 3, cols A and C
mr = [.true., .false., .true., .false.]
mc = [.true., .false., .true.]
df2 = df%where(mr, mc)
if (nrow(df2) /= 2) error stop "where: nrow must be 2"
if (ncol(df2) /= 2) error stop "where: ncol must be 2"
if (df2%index(1) /= 10 .or. df2%index(2) /= 30) error stop "where: index mismatch"
if (abs(df2%values(2,2) - 300.0_dp) > tol) error stop "where: value mismatch"

! filter: drop rows 2 and 4, keep only column B
mr = [.false., .true., .false., .true.]
mc = [.false., .true., .false.]
df2 = df%filter(mr, mc, drop_rows=.true., drop_cols=.false.)
if (nrow(df2) /= 2) error stop "filter: nrow must be 2"
if (ncol(df2) /= 1) error stop "filter: ncol must be 1"
if (trim(df2%columns(1)) /= "B") error stop "filter: col must be B"
if (abs(df2%values(2,1) - 30.0_dp) > tol) error stop "filter: value mismatch"

! iloc: rows [2,4], cols [1,3] => index [20,40], cols A,C
ivec = [2, 4]
jvec = [1, 3]
df2 = df%iloc(rows=ivec, cols=jvec)
if (nrow(df2) /= 2 .or. ncol(df2) /= 2) error stop "iloc: shape mismatch"
if (df2%index(1) /= 20 .or. df2%index(2) /= 40) error stop "iloc: index mismatch"
if (trim(df2%columns(2)) /= "C") error stop "iloc: col mismatch"
if (abs(df2%values(2,2) - 400.0_dp) > tol) error stop "iloc: value mismatch"

! select: irows + column names
ivec = [1, 4]
df2 = df%select(irows=ivec, columns=["B","C"])
if (nrow(df2) /= 2 .or. ncol(df2) /= 2) error stop "select: shape mismatch"
if (df2%index(1) /= 10 .or. df2%index(2) /= 40) error stop "select: index mismatch"
if (trim(df2%columns(1)) /= "B") error stop "select: col mismatch"
if (abs(df2%values(2,2) - 400.0_dp) > tol) error stop "select: value mismatch"

! select: label rows + positional column
df2 = df%select(rows=[20,30], icols=[2])
if (nrow(df2) /= 2 .or. ncol(df2) /= 1) error stop "select mix: shape mismatch"
if (df2%index(1) /= 20 .or. df2%index(2) /= 30) error stop "select mix: index mismatch"
if (trim(df2%columns(1)) /= "B") error stop "select mix: col mismatch"
if (abs(df2%values(2,1) - 30.0_dp) > tol) error stop "select mix: value mismatch"

print *, "xdataframe_select: PASS"

end program xdataframe_select
