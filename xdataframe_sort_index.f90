program xdataframe_sort_index
use kind_mod, only: dp
use dataframe_mod, only: DataFrame
implicit none

type(DataFrame) :: df, df2
integer, allocatable :: idx(:)
character(len=100), allocatable :: cols(:)
real(kind=dp), allocatable :: vals(:,:)
logical :: ok
integer :: pos

! unsorted index with duplicates
allocate(idx(4), cols(2), vals(4,2))
idx = [3, 1, 2, 2]
cols(1) = "a"
cols(2) = "b"
vals(:,1) = [30.0_dp, 10.0_dp, 20.0_dp, 21.0_dp]
vals(:,2) = [300.0_dp, 100.0_dp, 200.0_dp, 201.0_dp]
df = DataFrame(index=idx, columns=cols, values=vals)

ok = df%is_sorted_index()
if (ok) error stop "is_sorted_index should be false before sort"
ok = df%is_unique_index()
if (ok) error stop "is_unique_index should be false (duplicate index values)"

call df%sort_index()  ! ascending
ok = df%is_sorted_index()
if (.not. ok) error stop "is_sorted_index should be true after ascending sort"

! index should now be [1,2,2,3]
if (any(df%index /= [1,2,2,3])) error stop "sort_index ascending produced wrong index order"

! row_pos: test first occurrence of duplicate when assuming sorted
pos = df%row_pos(2, assume_sorted=.true.)
if (pos /= 2) error stop "row_pos should return first occurrence for duplicate index"

call df%sort_index(ascending=.false.)  ! descending
ok = df%is_sorted_index(ascending=.false.)
if (.not. ok) error stop "is_sorted_index(ascending=.false.) should be true after descending sort"

! index should now be [3,2,2,1]
if (any(df%index /= [3,2,2,1])) error stop "sort_index descending produced wrong index order"

pos = df%row_pos(2, assume_sorted=.true., ascending=.false.)
if (pos /= 2) error stop "row_pos descending should return first occurrence for duplicate index"


deallocate(idx, cols, vals)

! unique example
allocate(idx(3), cols(1), vals(3,1))
idx = [10, 30, 20]
cols(1) = "x"
vals(:,1) = [1.0_dp, 3.0_dp, 2.0_dp]
df2 = DataFrame(index=idx, columns=cols, values=vals)

ok = df2%is_unique_index()
if (.not. ok) error stop "is_unique_index should be true for unique index"

call df2%sort_index()
if (any(df2%index /= [10,20,30])) error stop "sort_index ascending failed on unique index"

print *, "xdataframe_sort_index: PASS"

end program xdataframe_sort_index