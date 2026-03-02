module dataframe_mod
use kind_mod, only: dp
use util_mod, only: default, split_string, seq, cbind
use iso_fortran_env, only: output_unit
use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_is_nan
implicit none
private
public :: DataFrame, nrow, ncol, print_summary, random, operator(*), &
   operator(/), operator(+), operator(-), display, allocate_df, &
   operator(**), shape, subset_stride
integer, parameter :: nlen_columns = 100, nrows_print = 10 ! number of rows to print by default.
logical, save :: blank_line_before_display = .true.
interface display
   module procedure display_data
end interface display
interface operator (*)
   module procedure mult_x_df, mult_df_x, mult_n_df, mult_df_n
   module procedure mult_df_df
end interface
interface operator (/)
   module procedure div_df_x, div_df_n, div_x_df, div_n_df
   module procedure div_df_df
end interface
interface operator (+)
   module procedure add_x_df, add_df_x, add_n_df, add_df_n
   module procedure add_df_df
end interface
interface operator (-)
   module procedure subtract_x_df, subtract_df_x, &
      subtract_n_df, subtract_df_n, subtract_df_df
end interface
interface operator (**)
   module procedure power_df_n, power_df_x
end interface

type :: DataFrame
   integer, allocatable          :: index(:)
   character(len=nlen_columns), allocatable :: columns(:)
   real(kind=dp), allocatable    :: values(:,:)
   contains
      procedure :: read_csv, display=>display_data, write_csv, irow, icol, &
         loc, append_col, append_cols, set_col, col_pos, row_pos, at, iat, set_at, set_iat, &
         has_col, has_idx, drop_cols, drop_rows, rename_cols, where_cols, filter_cols, where, filter, iloc, select, add, subtract, multiply, divide
end type DataFrame

contains

pure function shape(df) result(ishape)
! return a 2-element array with the number of rows and columns of the dataframe
type(DataFrame), intent(in) :: df
integer                     :: ishape(2)
ishape = [nrow(df), ncol(df)]
end function shape

pure function icol(df, ivec) result(df_new)
! returns a dataframe with the subset of columns in ivec(:)
class(DataFrame), intent(in) :: df
integer, intent(in) :: ivec(:)
type(DataFrame) :: df_new
df_new = DataFrame(index=df%index, columns=df%columns(ivec), values=df%values(:, ivec))
end function icol

pure function loc(df, rows, columns) result(df_new)
! return a subset of a dataframe with the specified rows (index values) and columns
class(DataFrame), intent(in) :: df
integer, intent(in), optional :: rows(:)
character (len=*), intent(in), optional :: columns(:)
type(DataFrame) :: df_new
integer, allocatable :: rows_(:)
character (len=nlen_columns), allocatable :: columns_(:)
integer :: i
integer, allocatable :: jrow(:), jcol(:)
if (present(rows)) then
   rows_ = rows
   allocate (jrow(size(rows)))
   do i=1,size(rows)
      jrow(i) = findloc(df%index, rows(i), dim=1)
   end do
else
   rows_ = df%index
   jrow = seq(1, nrow(df))
end if
if (present(columns)) then
   columns_ = columns
   allocate(jcol(size(columns)))
   do i=1,size(columns)
      jcol(i) = findloc(df%columns, columns(i), dim=1)
   end do
else
   columns_ = df%columns
   jcol = seq(1, ncol(df))
end if
df_new = DataFrame(index=rows_, columns=columns_, values=df%values(jrow, jcol))
end function loc

pure function row_pos(self, idx) result(irow)
! return the row position (1..nrow) for index value idx
class(DataFrame), intent(in) :: self
integer, intent(in) :: idx
integer :: irow
irow = findloc(self%index, idx, dim=1)
if (irow == 0) error stop "in row_pos, index not found"
end function row_pos

pure function col_pos(self, column) result(jcol)
! return the column position (1..ncol) for column name
class(DataFrame), intent(in) :: self
character(len=*), intent(in) :: column
integer :: jcol
jcol = findloc(self%columns, column, dim=1)
if (jcol == 0) error stop "in col_pos, column not found: " // trim(column)
end function col_pos

pure function iat(self, i, j) result(x)
! return a scalar element by 1-based row/column positions
class(DataFrame), intent(in) :: self
integer, intent(in) :: i, j
real(kind=dp) :: x
if (i < 1 .or. i > nrow(self)) error stop "in iat, row position out of range"
if (j < 1 .or. j > ncol(self)) error stop "in iat, column position out of range"
x = self%values(i, j)
end function iat

pure function at(self, idx, column) result(x)
! return a scalar element by index value and column name
class(DataFrame), intent(in) :: self
integer, intent(in) :: idx
character(len=*), intent(in) :: column
real(kind=dp) :: x
integer :: i, j
i = self%row_pos(idx)
j = self%col_pos(column)
x = self%values(i, j)
end function at

pure subroutine set_iat(self, i, j, x)
! set a scalar element by 1-based row/column positions
class(DataFrame), intent(in out) :: self
integer, intent(in) :: i, j
real(kind=dp), intent(in) :: x
if (i < 1 .or. i > nrow(self)) error stop "in set_iat, row position out of range"
if (j < 1 .or. j > ncol(self)) error stop "in set_iat, column position out of range"
self%values(i, j) = x
end subroutine set_iat

pure subroutine set_at(self, idx, column, x)
! set a scalar element by index value and column name
class(DataFrame), intent(in out) :: self
integer, intent(in) :: idx
character(len=*), intent(in) :: column
real(kind=dp), intent(in) :: x
integer :: i, j
i = self%row_pos(idx)
j = self%col_pos(column)
self%values(i, j) = x
end subroutine set_at

logical function has_col(self, name)
! return .true. if dataframe has a column with the given name
class(DataFrame), intent(in) :: self
character(len=*), intent(in) :: name
integer :: j
character(len=nlen_columns) :: key
key = trim(name)
j = findloc(self%columns, key, dim=1)
has_col = (j > 0)
end function has_col

logical function has_idx(self, idx)
! return .true. if dataframe has a row with the given index value
class(DataFrame), intent(in) :: self
integer, intent(in) :: idx
integer :: i
i = findloc(self%index, idx, dim=1)
has_idx = (i > 0)
end function has_idx

function drop_cols(self, names, missing) result(df_new)
! drop columns by name
class(DataFrame), intent(in) :: self
character(len=*), intent(in) :: names(:)
character(len=*), intent(in), optional :: missing
type(DataFrame) :: df_new
logical, allocatable :: keep(:)
integer, allocatable :: ivec_keep(:)
integer :: k, j, n
character(len=100) :: miss
character(len=nlen_columns) :: key

miss = trim(default("error", missing))
miss = str_lower(miss)

n = ncol(self)
allocate(keep(n))
keep = .true.

do k = 1, size(names)
   key = trim(names(k))
   j = findloc(self%columns, key, dim=1)
   if (j <= 0) then
      if (miss == "ignore") cycle
      error stop "drop_cols: column not found: "//trim(names(k))
   end if
   keep(j) = .false.
end do

ivec_keep = pack(seq(1, n), keep)
df_new = self%icol(ivec_keep)
end function drop_cols

function drop_rows(self, idx, missing) result(df_new)
! drop rows by index value
class(DataFrame), intent(in) :: self
integer, intent(in) :: idx(:)
character(len=*), intent(in), optional :: missing
type(DataFrame) :: df_new
logical, allocatable :: keep(:)
integer, allocatable :: ivec_keep(:)
integer :: k, i, n
character(len=100) :: miss

miss = trim(default("error", missing))
miss = str_lower(miss)

n = nrow(self)
allocate(keep(n))
keep = .true.

do k = 1, size(idx)
   i = findloc(self%index, idx(k), dim=1)
   if (i <= 0) then
      if (miss == "ignore") cycle
      error stop "drop_rows: index not found"
   end if
   keep(i) = .false.
end do

ivec_keep = pack(seq(1, n), keep)
df_new = self%irow(ivec_keep)
end function drop_rows

subroutine rename_cols(self, old, new, missing)
! rename columns: replace each old(i) with new(i)
class(DataFrame), intent(in out) :: self
character(len=*), intent(in) :: old(:), new(:)
character(len=*), intent(in), optional :: missing
integer :: k, j
character(len=100) :: miss
character(len=nlen_columns) :: key

if (size(old) /= size(new)) error stop "rename_cols: size(old) /= size(new)"

miss = trim(default("error", missing))
miss = str_lower(miss)

do k = 1, size(old)
   key = trim(old(k))
   j = findloc(self%columns, key, dim=1)
   if (j <= 0) then
      if (miss == "ignore") cycle
      error stop "rename_cols: column not found: "//trim(old(k))
   end if
   self%columns(j) = trim(new(k))
end do
end subroutine rename_cols



function where_cols(self, mask_cols) result(df_new)
! keep columns where mask_cols(j) is .true.
class(DataFrame), intent(in) :: self
logical, intent(in) :: mask_cols(:)
type(DataFrame) :: df_new
integer, allocatable :: j_keep(:)
if (size(mask_cols) /= ncol(self)) error stop "where_cols: size(mask_cols) /= ncol(self)"
j_keep = pack(seq(1, ncol(self)), mask_cols)
df_new = self%icol(j_keep)
end function where_cols

function filter_cols(self, mask_cols, drop) result(df_new)
! filter columns by mask; if drop=.true. then drop columns where mask is .true.
class(DataFrame), intent(in) :: self
logical, intent(in) :: mask_cols(:)
logical, intent(in), optional :: drop
type(DataFrame) :: df_new
logical :: drop_
logical, allocatable :: keep(:)
drop_ = default(.false., drop)
if (size(mask_cols) /= ncol(self)) error stop "filter_cols: size(mask_cols) /= ncol(self)"
allocate(keep(size(mask_cols)))
if (drop_) then
   keep = .not. mask_cols
else
   keep = mask_cols
end if
df_new = self%where_cols(keep)
end function filter_cols

function where(self, mask_rows, mask_cols) result(df_new)
! keep rows and columns where masks are .true.
class(DataFrame), intent(in) :: self
logical, intent(in) :: mask_rows(:)
logical, intent(in) :: mask_cols(:)
type(DataFrame) :: df_new
integer, allocatable :: i_keep(:), j_keep(:)
if (size(mask_rows) /= nrow(self)) error stop "where: size(mask_rows) /= nrow(self)"
if (size(mask_cols) /= ncol(self)) error stop "where: size(mask_cols) /= ncol(self)"
i_keep = pack(seq(1, nrow(self)), mask_rows)
j_keep = pack(seq(1, ncol(self)), mask_cols)
df_new = DataFrame(index=self%index(i_keep), columns=self%columns(j_keep), values=self%values(i_keep, j_keep))
end function where

function filter(self, mask_rows, mask_cols, drop_rows, drop_cols) result(df_new)
! filter rows and columns by masks; if drop_rows/drop_cols are .true. then drop where mask is .true.
class(DataFrame), intent(in) :: self
logical, intent(in) :: mask_rows(:)
logical, intent(in) :: mask_cols(:)
logical, intent(in), optional :: drop_rows, drop_cols
type(DataFrame) :: df_new
logical :: drop_r, drop_c
logical, allocatable :: keep_rows(:), keep_cols(:)

if (size(mask_rows) /= nrow(self)) error stop "filter: size(mask_rows) /= nrow(self)"
if (size(mask_cols) /= ncol(self)) error stop "filter: size(mask_cols) /= ncol(self)"

drop_r = default(.false., drop_rows)
drop_c = default(.false., drop_cols)

allocate(keep_rows(size(mask_rows)))
allocate(keep_cols(size(mask_cols)))

if (drop_r) then
   keep_rows = .not. mask_rows
else
   keep_rows = mask_rows
end if

if (drop_c) then
   keep_cols = .not. mask_cols
else
   keep_cols = mask_cols
end if

df_new = self%where(keep_rows, keep_cols)
end function filter

function iloc(self, rows, cols) result(df_new)
! positional selection by row/column positions (1-based)
class(DataFrame), intent(in) :: self
integer, intent(in), optional :: rows(:)
integer, intent(in), optional :: cols(:)
type(DataFrame) :: df_new
if (present(rows) .and. present(cols)) then
   df_new = self%select(irows=rows, icols=cols)
else if (present(rows)) then
   df_new = self%select(irows=rows)
else if (present(cols)) then
   df_new = self%select(icols=cols)
else
   df_new = self%select()
end if
end function iloc

function select(self, rows, columns, irows, icols) result(df_new)
! select a sub-dataframe using label- or position-based selectors on each axis.
! rules:
!  - at most one of rows/irows may be present
!  - at most one of columns/icols may be present
class(DataFrame), intent(in) :: self
integer, intent(in), optional :: rows(:)
character(len=*), intent(in), optional :: columns(:)
integer, intent(in), optional :: irows(:)
integer, intent(in), optional :: icols(:)
type(DataFrame) :: df_new
integer, allocatable :: i_keep(:), j_keep(:)
integer :: k

if (present(rows) .and. present(irows)) error stop "select: both rows and irows are present"
if (present(columns) .and. present(icols)) error stop "select: both columns and icols are present"

if (present(rows)) then
   allocate(i_keep(size(rows)))
   do k = 1, size(rows)
      i_keep(k) = self%row_pos(rows(k))
   end do
else if (present(irows)) then
   i_keep = irows
   do k = 1, size(i_keep)
      if (i_keep(k) < 1 .or. i_keep(k) > nrow(self)) error stop "select: row position out of range"
   end do
else
   i_keep = seq(1, nrow(self))
end if

if (present(columns)) then
   allocate(j_keep(size(columns)))
   do k = 1, size(columns)
      j_keep(k) = self%col_pos(columns(k))
   end do
else if (present(icols)) then
   j_keep = icols
   do k = 1, size(j_keep)
      if (j_keep(k) < 1 .or. j_keep(k) > ncol(self)) error stop "select: column position out of range"
   end do
else
   j_keep = seq(1, ncol(self))
end if

df_new = DataFrame(index=self%index(i_keep), columns=self%columns(j_keep), values=self%values(i_keep, j_keep))
end function select
pure function str_lower(str) result(out)
! return str converted to lowercase (ASCII)
character(len=*), intent(in) :: str
character(len=len(str))      :: out
integer :: i, c
out = str
do i = 1, len(str)
   c = iachar(out(i:i))
   if (c >= iachar('A') .and. c <= iachar('Z')) out(i:i) = achar(c + 32)
end do
end function str_lower




pure function irow(df, ivec) result(df_new)
! returns a dataframe with the subset of columns in ivec(:)
class(DataFrame), intent(in) :: df
integer, intent(in) :: ivec(:)
type(DataFrame) :: df_new
df_new = DataFrame(index=df%index(ivec), columns=df%columns, values=df%values(ivec, :))
end function irow

pure subroutine set_col(df, column, values)
! append a column with specified values to DataFrame df if column is not in df,
! and set the values of that column if it is already present
class(DataFrame), intent(in out) :: df
character (len=*), intent(in) :: column
real(kind=dp), intent(in) :: values(:)
integer :: jcol
if (size(values) /= nrow(df)) error stop "in set_col, size(values) /= nrow(df)"
jcol = findloc(df%columns, column, dim=1)
if (jcol == 0) then
   call append_col(df, column, values)
else
   df%values(:,jcol) = values
end if
end subroutine set_col

pure subroutine append_col(df, column, values)
! append a column with specified values to DataFrame df
class(DataFrame), intent(in out) :: df
character (len=*), intent(in) :: column
real(kind=dp), intent(in) :: values(:)
character (len=nlen_columns) :: column_
if (size(values) /= nrow(df)) error stop "in append_col, size(values) /= nrow(df)"
column_ = column
df%columns = [df%columns, column_]
df%values  = cbind(df%values, values)
end subroutine append_col

pure subroutine append_cols(df, columns, values)
! append a column with specified values to DataFrame df
class(DataFrame), intent(in out) :: df
character (len=*), intent(in) :: columns(:)
real(kind=dp), intent(in) :: values(:,:)
character (len=nlen_columns), allocatable :: columns_(:)
if (size(values, 1) /= nrow(df)) error stop "in append_cols, size(values) /= nrow(df)"
if (size(values, 2) /= size(columns)) error stop "in append_cols, size(values, 2) /= size(columns)"
columns_ = columns
df%columns = [df%columns, columns_]
df%values  = cbind(df%values, values)
end subroutine append_cols

subroutine allocate_df(df, n1, n2, default_indices, default_columns)
type(DataFrame), intent(out) :: df
integer        , intent(in)  :: n1, n2
logical        , intent(in), optional :: default_indices, default_columns
integer :: i
allocate (df%index(n1), df%columns(n2), df%values(n1, n2))
if (default(.true., default_indices)) then
   do i=1,n1
      df%index(i) = i
   end do
end if
if (default(.true., default_columns)) then
   do i=1,n2
      write (df%columns(i), "('x',i0)") i
   end do
end if
end subroutine allocate_df

elemental function nrow(df) result(num_rows)
! return the # of rows
type(DataFrame), intent(in) :: df
integer                     :: num_rows
if (allocated(df%values)) then
   num_rows = size(df%values, 1)
else
   num_rows = -1
end if
end function nrow

elemental function ncol(df) result(num_col)
! return the # of columns
type(DataFrame), intent(in) :: df
integer                     :: num_col
if (allocated(df%values)) then
   num_col = size(df%values, 2)
else
   num_col = -1
end if
end function ncol

!------------------------------------------------------------------
! read_csv:
!
! Reads from a CSV file with the following format:
!
!      ,Col1,Col2,...
!      index1,val11,val12,...
!      index2,val21,val22,...
!
! The header row begins with an empty token (before the first comma).
!------------------------------------------------------------------
subroutine read_csv(self, filename, max_col, max_rows)
class(DataFrame), intent(inout) :: self
character(len=*), intent(in)    :: filename
integer, intent(in), optional :: max_col, max_rows
integer :: io, unit, i, j, nrows, ncols, maxlen
character(len=1024) :: line
character(:), allocatable :: tokens(:)

! 1) Open the file.
open(newunit=unit, file=filename, status='old', action='read', iostat=io)
if (io /= 0) then
   print *, "Error opening file:", filename
   stop
end if

! 2) Read the header line.
read(unit, '(A)', iostat=io) line
if (io /= 0) then
   print *, "Error reading header line from:", filename
   stop
end if

call split_string(line, ",", tokens)
! The first token should be empty; remaining tokens are column names.
ncols = size(tokens) - 1
if (present(max_col)) ncols = min(ncols, max_col)
if (ncols <= 0) then
   print *, "No columns detected in header of", filename
   stop
end if

! Determine maximum length among the column name tokens.
maxlen = 0
do i = 2, size(tokens)
   maxlen = max(maxlen, len_trim(tokens(i)))
end do

! Allocate columns
allocate(self%columns(ncols))
do i = 1, ncols
   self%columns(i) = tokens(i+1)
end do

! 3) Count the remaining data lines.
nrows = 0
do
   if (present(max_rows)) then
      if (nrows >= max_rows) exit
   end if
   read(unit, '(A)', iostat=io) line
   if (io /= 0 .or. trim(line) == "") exit
   nrows = nrows + 1
end do

if (nrows == 0) then
   print *, "No data lines detected in file:", filename
   stop
end if

! 4) Rewind the file and skip the header.
rewind(unit)
read(unit, '(A)')  ! skip header

! 5) Allocate the index and values arrays.
allocate(self%index(nrows), self%values(nrows, ncols))

! 6) Read each data row.
do i = 1, nrows
   read(unit, '(A)', iostat=io) line
   if (trim(line) == "") exit
   call split_string(line, ",", tokens)
   ! First token is t<he index.
   read(tokens(1), *) self%index(i)
   ! Remaining tokens are the real values.
   do j = 1, ncols
      read(tokens(j+1), *) self%values(i,j)
   end do
end do

close(unit)
end subroutine read_csv

!------------------------------------------------------------------
! display_data:
!
! Prints the DataFrame to the screen in a CSV-like format.
! If the DataFrame has more than nrows_print observations, by default only
! the first nrows_print/2 and the last (nrows_print - nrows_print/2) rows are
! printed with an indication of omitted rows.
!
! An optional logical argument 'print_all' may be provided. If it is present
! and set to .true., then all rows are printed.
!------------------------------------------------------------------
impure elemental subroutine display_data(self, print_all, fmt_ir, fmt_header, fmt_trailer, title)
class(DataFrame), intent(in) :: self
logical, intent(in), optional :: print_all
character (len=*), intent(in), optional :: fmt_ir, fmt_header, fmt_trailer, title
integer :: total, i, n_top, n_bottom
logical :: print_all_
character (len=100) :: fmt_ir_, fmt_header_
fmt_ir_ = default("(i10,*(1x,f10.4))", fmt_ir)
fmt_header_ = default("(a10,*(1x,a10))", fmt_header)
print_all_ = default(.false., print_all)
total = size(self%index)
if (blank_line_before_display) write(*,*)
if (present(title)) write(*,"(a)") title
! Print header.
write(*,fmt_header_) "index", (trim(self%columns(i)), i=1,size(self%columns))

if (print_all_) then
   ! Print all rows.
   do i = 1, total
      write(*,fmt_ir_) self%index(i), self%values(i,:)
   end do
else
   if (total <= nrows_print) then
      ! Print all rows if total is less than or equal to nrows_print.
      do i = 1, total
         write(*,fmt_ir_) self%index(i), self%values(i,:)
      end do
   else
      ! Compute number of rows for the top and bottom parts.
      n_top = nrows_print / 2
      n_bottom = nrows_print - n_top
      ! Print first n_top rows.
      do i = 1, n_top
         write(*,fmt_ir_) self%index(i), self%values(i,:)
      end do
      ! Indicate omitted rows.
      write(*,*) "   ... (", total - nrows_print, " rows omitted) ..."
      ! Print last n_bottom rows.
      do i = total - n_bottom + 1, total
         write(*,fmt_ir_) self%index(i), self%values(i,:)
      end do
   end if
end if
if (present(fmt_trailer)) write(*,fmt_trailer)
end subroutine display_data

!------------------------------------------------------------------
! write_csv:
!
! Writes the DataFrame to a CSV file in the same format as read_csv.
!------------------------------------------------------------------
subroutine write_csv(self, filename)
class(DataFrame), intent(in) :: self
character(len=*), intent(in) :: filename
integer :: i, j, unit, io

open(newunit=unit, file=filename, status='replace', action='write', iostat=io)
if (io /= 0) then
   print *, "Error opening output file:", filename
   stop
end if

! Write header: empty token for index, then column names.
write(unit,'(A)', advance='no') ""
do j = 1, size(self%columns)
   write(unit,'(",", A)', advance='no') trim(self%columns(j))
end do
write(unit,*)

! Write each data row.
do i = 1, size(self%index)
   write(unit,'(I10)', advance='no') self%index(i)
   do j = 1, size(self%columns)
      write(unit,'(",", F10.4)', advance='no') self%values(i,j)
   end do
   write(unit,*)
end do
close(unit)
end subroutine write_csv

subroutine print_summary(self, outu, fmt_header, fmt_trailer)
type(DataFrame), intent(in) :: self
integer, intent(in), optional :: outu
character (len=*), intent(in), optional :: fmt_header, fmt_trailer
integer :: outu_, nr, nc
outu_ = default(output_unit, outu)
if (present(fmt_header)) write (outu_, fmt_header)
nr = nrow(self)
nc = ncol(self)
write(outu_, "('#rows, columns:', 2(1x,i0))") nr, nc
if (nr > 0) write(outu_, "('first, last indices:', 2(1x,i0))") &
   self%index(1), self%index(nr)
if (nc > 0) write(outu_, "('first, last columns:', 2(1x,a))") &
   trim(self%columns(1)), trim(self%columns(nc))
if (present(fmt_trailer)) write (outu_, fmt_trailer)
end subroutine print_summary

subroutine alloc(self, nr, nc)
type(DataFrame), intent(out) :: self
integer        , intent(in)  :: nr, nc
allocate (self%index(nr), self%values(nr, nc))
allocate (self%columns(nc))
end subroutine alloc

subroutine random(self, nr, nc)
type(DataFrame), intent(out) :: self
integer, intent(in) :: nr, nc
integer :: i
call alloc(self, nr, nc)
call random_number(self%values)
do i=1,nr
   self%index(i) = i
end do
do i=1,nc
   write (self%columns(i), "('C',i0)") i
end do
end subroutine random

function mult_x_df(x, df) result(res)
! return x * df
real(kind=dp)  , intent(in) :: x
type(DataFrame), intent(in) :: df
type(DataFrame)             :: res
res = df
if (allocated(res%values)) res%values = x*res%values
end function mult_x_df

function mult_df_x(df, x) result(res)
! return df * x
type(DataFrame), intent(in) :: df
real(kind=dp)  , intent(in) :: x
type(DataFrame)             :: res
res = df
if (allocated(res%values)) res%values = x*res%values
end function mult_df_x

function add_x_df(x, df) result(res)
! return x * df
real(kind=dp)  , intent(in) :: x
type(DataFrame), intent(in) :: df
type(DataFrame)             :: res
res = df
if (allocated(res%values)) res%values = x + res%values
end function add_x_df

function add_df_x(df, x) result(res)
! return df * x
type(DataFrame), intent(in) :: df
real(kind=dp)  , intent(in) :: x
type(DataFrame)             :: res
res = df
if (allocated(res%values)) res%values = res%values + x
end function add_df_x

function subtract_x_df(x, df) result(res)
! return x - df
real(kind=dp)  , intent(in) :: x
type(DataFrame), intent(in) :: df
type(DataFrame)             :: res
res = df
if (allocated(res%values)) res%values = x - res%values
end function subtract_x_df

function subtract_df_x(df, x) result(res)
! return df - x
type(DataFrame), intent(in) :: df
real(kind=dp)  , intent(in) :: x
type(DataFrame)             :: res
res = df
if (allocated(res%values)) res%values = res%values - x
end function subtract_df_x

function div_df_x(df, x) result(res)
! return df / x
real(kind=dp)  , intent(in) :: x
type(DataFrame), intent(in) :: df
type(DataFrame)             :: res
res = df
if (allocated(res%values)) res%values = res%values/x
end function div_df_x

function div_x_df(x, df) result(res)
! return df / x
real(kind=dp)  , intent(in) :: x
type(DataFrame), intent(in) :: df
type(DataFrame)             :: res
res = df
if (allocated(res%values)) res%values = x/res%values
end function div_x_df

function div_n_df(n, df) result(res)
! return n / x
integer        , intent(in) :: n
type(DataFrame), intent(in) :: df
type(DataFrame)             :: res
res = df
if (allocated(res%values)) res%values = n/res%values
end function div_n_df

function mult_n_df(n, df) result(res)
! return n * df
integer        , intent(in) :: n
type(DataFrame), intent(in) :: df
type(DataFrame)             :: res
res = df
if (allocated(res%values)) res%values = n*res%values
end function mult_n_df

function mult_df_n(df, n) result(res)
! return df * n
type(DataFrame), intent(in) :: df
integer        , intent(in) :: n
type(DataFrame)             :: res
res = df
if (allocated(res%values)) res%values = n*res%values
end function mult_df_n

function add_n_df(n, df) result(res)
! return n * df
integer        , intent(in) :: n
type(DataFrame), intent(in) :: df
type(DataFrame)             :: res
res = df
if (allocated(res%values)) res%values = n + res%values
end function add_n_df

function add_df_n(df, n) result(res)
! return df * n
type(DataFrame), intent(in) :: df
integer        , intent(in) :: n
type(DataFrame)             :: res
res = df
if (allocated(res%values)) res%values = res%values + n
end function add_df_n

function subtract_n_df(n, df) result(res)
! return n - df
integer        , intent(in) :: n
type(DataFrame), intent(in) :: df
type(DataFrame)             :: res
res = df
if (allocated(res%values)) res%values = n - res%values
end function subtract_n_df

function subtract_df_n(df, n) result(res)
! return df - n
type(DataFrame), intent(in) :: df
integer        , intent(in) :: n
type(DataFrame)             :: res
res = df
if (allocated(res%values)) res%values = res%values - n
end function subtract_df_n

function div_df_n(df, n) result(res)
! return df / n
integer        , intent(in) :: n
type(DataFrame), intent(in) :: df
type(DataFrame)             :: res
res = df
if (allocated(res%values)) res%values = res%values/n
end function div_df_n

subroutine require_unique_labels(df, who)
! error stop if df has duplicate index or duplicate column names
type(DataFrame), intent(in) :: df
character(len=*), intent(in) :: who
character(len=200) :: msg
integer :: i, j

do i = 1, nrow(df) - 1
   do j = i + 1, nrow(df)
      if (df%index(i) == df%index(j)) then
         write(msg, "(a, a)") trim(who), ": duplicate index"
         error stop msg
      end if
   end do
end do

do i = 1, ncol(df) - 1
   do j = i + 1, ncol(df)
      if (trim(df%columns(i)) == trim(df%columns(j))) then
         write(msg, "(a, a)") trim(who), ": duplicate columns"
         error stop msg
      end if
   end do
end do
end subroutine require_unique_labels

integer function find_col_trim(cols, name) result(pos)
! return position of name in cols using trim() equality, or 0 if not found
character(len=*), intent(in) :: cols(:)
character(len=*), intent(in) :: name
integer :: j
pos = 0
do j = 1, size(cols)
   if (trim(cols(j)) == trim(name)) then
      pos = j
      return
   end if
end do
end function find_col_trim

function union_int(a, b) result(c)
integer, intent(in) :: a(:), b(:)
integer, allocatable :: c(:)
integer, allocatable :: tmp(:)
integer :: n, i
allocate(tmp(size(a) + size(b)))
n = 0
do i = 1, size(a)
   n = n + 1
   tmp(n) = a(i)
end do
do i = 1, size(b)
   if (.not. any(tmp(1:n) == b(i))) then
      n = n + 1
      tmp(n) = b(i)
   end if
end do
allocate(c(n))
c = tmp(1:n)
end function union_int

function intersect_int(a, b) result(c)
integer, intent(in) :: a(:), b(:)
integer, allocatable :: c(:)
integer, allocatable :: tmp(:)
integer :: n, i
allocate(tmp(size(a)))
n = 0
do i = 1, size(a)
   if (any(b == a(i))) then
      n = n + 1
      tmp(n) = a(i)
   end if
end do
allocate(c(n))
c = tmp(1:n)
end function intersect_int

function union_cols(a, b) result(c)
character(len=nlen_columns), intent(in) :: a(:), b(:)
character(len=nlen_columns), allocatable :: c(:)
character(len=nlen_columns), allocatable :: tmp(:)
integer :: n, i
allocate(tmp(size(a) + size(b)))
n = 0
do i = 1, size(a)
   n = n + 1
   tmp(n) = a(i)
end do
do i = 1, size(b)
   if (find_col_trim(tmp(1:n), b(i)) == 0) then
      n = n + 1
      tmp(n) = b(i)
   end if
end do
allocate(c(n))
c = tmp(1:n)
end function union_cols

function intersect_cols(a, b) result(c)
character(len=nlen_columns), intent(in) :: a(:), b(:)
character(len=nlen_columns), allocatable :: c(:)
character(len=nlen_columns), allocatable :: tmp(:)
integer :: n, i
allocate(tmp(size(a)))
n = 0
do i = 1, size(a)
   if (find_col_trim(b, a(i)) /= 0) then
      n = n + 1
      tmp(n) = a(i)
   end if
end do
allocate(c(n))
c = tmp(1:n)
end function intersect_cols

function aligned_binary(self, other, op, how, fill_value) result(res)
! pandas-like aligned arithmetic on union/intersection of index and columns
class(DataFrame), intent(in) :: self
type(DataFrame), intent(in)  :: other
character(len=*), intent(in) :: op
character(len=*), intent(in), optional :: how
real(kind=dp), intent(in), optional :: fill_value
type(DataFrame) :: res

character(len=20) :: how0
integer, allocatable :: idx_out(:)
character(len=nlen_columns), allocatable :: col_out(:)
real(kind=dp), allocatable :: a(:,:), b(:,:)
real(kind=dp) :: fill
logical :: do_fill
integer :: i, j, ii, jj, n1, n2
real(kind=dp) :: x

call require_unique_labels(self, "aligned_binary self")
call require_unique_labels(other, "aligned_binary other")

how0 = trim(default("outer", how))
select case (how0)
case ("outer")
   idx_out = union_int(self%index, other%index)
   col_out = union_cols(self%columns, other%columns)
case ("inner")
   idx_out = intersect_int(self%index, other%index)
   col_out = intersect_cols(self%columns, other%columns)
case ("left")
   idx_out = self%index
   col_out = self%columns
case ("right")
   idx_out = other%index
   col_out = other%columns
case default
   error stop "aligned_binary: how must be outer/inner/left/right"
end select

n1 = size(idx_out)
n2 = size(col_out)

do_fill = present(fill_value)
if (do_fill) then
   fill = fill_value
else
   fill = ieee_value(0.0_dp, ieee_quiet_nan)
end if

allocate(a(n1, n2), b(n1, n2))
a = fill
b = fill

! place self values into aligned array
do i = 1, nrow(self)
   ii = findloc(idx_out, self%index(i), dim=1)
   if (ii <= 0) cycle
   do j = 1, ncol(self)
      jj = find_col_trim(col_out, self%columns(j))
      if (jj <= 0) cycle
      x = self%values(i, j)
      if (do_fill) then
         if (ieee_is_nan(x)) x = fill
      end if
      a(ii, jj) = x
   end do
end do

! place other values into aligned array
do i = 1, nrow(other)
   ii = findloc(idx_out, other%index(i), dim=1)
   if (ii <= 0) cycle
   do j = 1, ncol(other)
      jj = find_col_trim(col_out, other%columns(j))
      if (jj <= 0) cycle
      x = other%values(i, j)
      if (do_fill) then
         if (ieee_is_nan(x)) x = fill
      end if
      b(ii, jj) = x
   end do
end do

allocate(res%index(n1), res%columns(n2), res%values(n1, n2))
res%index = idx_out
res%columns = col_out

select case (trim(op))
case ("+")
   res%values = a + b
case ("-")
   res%values = a - b
case ("*")
   res%values = a * b
case ("/")
   res%values = a / b
case default
   error stop "aligned_binary: invalid op"
end select
end function aligned_binary

function add(self, other, how, fill_value) result(res)
class(DataFrame), intent(in) :: self
type(DataFrame), intent(in) :: other
character(len=*), intent(in), optional :: how
real(kind=dp), intent(in), optional :: fill_value
type(DataFrame) :: res
res = aligned_binary(self, other, "+", how, fill_value)
end function add

function subtract(self, other, how, fill_value) result(res)
class(DataFrame), intent(in) :: self
type(DataFrame), intent(in) :: other
character(len=*), intent(in), optional :: how
real(kind=dp), intent(in), optional :: fill_value
type(DataFrame) :: res
res = aligned_binary(self, other, "-", how, fill_value)
end function subtract

function multiply(self, other, how, fill_value) result(res)
class(DataFrame), intent(in) :: self
type(DataFrame), intent(in) :: other
character(len=*), intent(in), optional :: how
real(kind=dp), intent(in), optional :: fill_value
type(DataFrame) :: res
res = aligned_binary(self, other, "*", how, fill_value)
end function multiply

function divide(self, other, how, fill_value) result(res)
class(DataFrame), intent(in) :: self
type(DataFrame), intent(in) :: other
character(len=*), intent(in), optional :: how
real(kind=dp), intent(in), optional :: fill_value
type(DataFrame) :: res
res = aligned_binary(self, other, "/", how, fill_value)
end function divide


subroutine require_same_df(df0, df1, who)
type(DataFrame), intent(in) :: df0, df1
character(len=*), intent(in) :: who
character(len=200) :: msg
integer :: j
if (nrow(df0) /= nrow(df1)) then
   write(msg,'("in ",a,", nrow mismatch")') trim(who)
   error stop msg
end if
if (ncol(df0) /= ncol(df1)) then
   write(msg,'("in ",a,", ncol mismatch")') trim(who)
   error stop msg
end if
if (any(df0%index /= df1%index)) then
   write(msg,'("in ",a,", index mismatch")') trim(who)
   error stop msg
end if
do j=1,ncol(df0)
   if (trim(df0%columns(j)) /= trim(df1%columns(j))) then
      write(msg,'("in ",a,", columns mismatch")') trim(who)
      error stop msg
   end if
end do
end subroutine require_same_df

function add_df_df(df0, df1) result(res)
type(DataFrame), intent(in) :: df0
type(DataFrame), intent(in) :: df1
type(DataFrame)             :: res
call require_same_df(df0, df1, "add_df_df")
res = df0
if (allocated(res%values)) res%values = df0%values + df1%values
end function add_df_df

function subtract_df_df(df0, df1) result(res)
type(DataFrame), intent(in) :: df0
type(DataFrame), intent(in) :: df1
type(DataFrame)             :: res
call require_same_df(df0, df1, "subtract_df_df")
res = df0
if (allocated(res%values)) res%values = df0%values - df1%values
end function subtract_df_df

function mult_df_df(df0, df1) result(res)
type(DataFrame), intent(in) :: df0
type(DataFrame), intent(in) :: df1
type(DataFrame)             :: res
call require_same_df(df0, df1, "mult_df_df")
res = df0
if (allocated(res%values)) res%values = df0%values * df1%values
end function mult_df_df

function div_df_df(df0, df1) result(res)
type(DataFrame), intent(in) :: df0
type(DataFrame), intent(in) :: df1
type(DataFrame)             :: res
call require_same_df(df0, df1, "div_df_df")
res = df0
if (allocated(res%values)) res%values = df0%values / df1%values
end function div_df_df



elemental function power_df_n(df, n) result(res)
! return df**n element-wise
integer        , intent(in) :: n
type(DataFrame), intent(in) :: df
type(DataFrame)             :: res
res = df
if (allocated(res%values)) res%values = res%values**n
end function power_df_n

elemental function power_df_x(df, x) result(res)
! return df**x element-wise
real(kind=dp), intent(in)   :: x
type(DataFrame), intent(in) :: df
type(DataFrame)             :: res
res = df
if (allocated(res%values)) res%values = res%values**x
end function power_df_x

elemental function subset_stride(df, stride) result(df_new)
type(DataFrame), intent(in) :: df
integer, intent(in) :: stride
type(DataFrame) :: df_new
! print*,"df%index(1:nrow(df):stride)", df%index(1:nrow(df):stride)
! print*,"df%values(1:nrow(df):stride, :)", df%values(1:nrow(df):stride, :)
! in the line below, some parentheses are added to work around
! compiler bugs
if (stride == 0) error stop "in subset_stride, stride must nost equal 0"
df_new = DataFrame(index=(df%index(1:nrow(df):stride)), &
   columns=df%columns, values = (df%values(1:nrow(df):stride, :)))
end function subset_stride
end module dataframe_mod
