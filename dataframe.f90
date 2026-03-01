module dataframe_mod
use kind_mod, only: dp
use util_mod, only: default, split_string, seq, cbind
use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_value, ieee_quiet_nan
use iso_fortran_env, only: output_unit
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
end interface
interface operator (/)
   module procedure div_df_x, div_df_n, div_x_df, div_n_df
end interface
interface operator (+)
   module procedure add_x_df, add_df_x, add_n_df, add_df_n
end interface
interface operator (-)
   module procedure subtract_x_df, subtract_df_x, &
      subtract_n_df, subtract_df_n
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
         loc, append_col, append_cols, set_col, isna, fillna, dropna
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
subroutine read_csv(self, filename, max_col, max_rows, na_values, empty_is_na)
! Read CSV with header row and integer index column.
! Missing values:
! - empty tokens (if empty_is_na=.true.) become NaN
! - tokens matching NA strings (case-insensitive) become NaN
! - tokens that fail numeric parsing become NaN
class(DataFrame), intent(inout) :: self
character(len=*), intent(in)    :: filename
integer, intent(in), optional   :: max_col, max_rows
character(len=*), intent(in), optional :: na_values(:)
logical, intent(in), optional   :: empty_is_na
integer :: io, unit, i, j, nrows, ncols, maxlen
character(len=1024) :: line
character(:), allocatable :: tokens(:)
character(:), allocatable :: tok
real(kind=dp) :: nan, x
logical :: empty_is_na_

empty_is_na_ = .true.
if (present(empty_is_na)) empty_is_na_ = empty_is_na
nan = ieee_value(0.0_dp, ieee_quiet_nan)

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

! Determine maximum length among the column name tokens (informational).
maxlen = 0
do i = 2, size(tokens)
   maxlen = max(maxlen, len_trim(tokens(i)))
end do

! Allocate columns.
if (allocated(self%columns)) deallocate (self%columns)
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
if (allocated(self%index)) deallocate (self%index)
if (allocated(self%values)) deallocate (self%values)
allocate(self%index(nrows), self%values(nrows, ncols))

! 6) Read each data row.
do i = 1, nrows
   read(unit, '(A)', iostat=io) line
   if (io /= 0 .or. trim(line) == "") exit
   call split_string(line, ",", tokens)

   ! First token is the index.
   if (size(tokens) < 1) error stop "in read_csv: empty line after split_string"
   tok = tokens(1)
   if (token_is_na(tok, na_values, .false.)) then
      error stop "in read_csv: missing index value"
   end if
   read(tok, *, iostat=io) self%index(i)
   if (io /= 0) then
      print *, "Error parsing index token on row", i, "in", filename
      stop
   end if

   ! Remaining tokens are the real values.
   do j = 1, ncols
      if (j+1 <= size(tokens)) then
         tok = tokens(j+1)
      else
         tok = ""
      end if

      if (token_is_na(tok, na_values, empty_is_na_)) then
         self%values(i,j) = nan
      else
         read(tok, *, iostat=io) x
         if (io /= 0) then
            self%values(i,j) = nan
         else
            self%values(i,j) = x
         end if
      end if
   end do
end do

close(unit)
end subroutine read_csv


pure function str_upper(str) result(out)
! return str converted to uppercase (ASCII)
character(len=*), intent(in) :: str
character(len=len(str))      :: out
integer :: i, c
out = str
do i = 1, len(str)
   c = iachar(out(i:i))
   if (c >= iachar('a') .and. c <= iachar('z')) out(i:i) = achar(c - 32)
end do
end function str_upper

pure logical function token_is_na(tok, na_values, empty_is_na)
! True if tok indicates a missing value.
character(len=*), intent(in) :: tok
character(len=*), intent(in), optional :: na_values(:)
logical, intent(in) :: empty_is_na
character(len=len(tok)) :: u
integer :: k

if (empty_is_na .and. len_trim(tok) == 0) then
   token_is_na = .true.
   return
end if

u = str_upper(adjustl(trim(tok)))

select case (trim(u))
case ("NA", "NAN", "NULL", "NONE", "N/A")
   token_is_na = .true.
   return
case default
end select

token_is_na = .false.
if (present(na_values)) then
   do k = 1, size(na_values)
      if (trim(u) == trim(str_upper(adjustl(trim(na_values(k)))))) then
         token_is_na = .true.
         return
      end if
   end do
end if

end function token_is_na

function isna(self) result(mask)
! return a logical mask indicating NaN values
class(DataFrame), intent(in) :: self
logical, allocatable :: mask(:,:)
integer :: n1, n2

if (.not. allocated(self%values)) then
   allocate(mask(0,0))
   return
end if

n1 = nrow(self)
n2 = ncol(self)
allocate(mask(n1, n2))
mask = ieee_is_nan(self%values)
end function isna

subroutine fillna(self, value, method)
! fill NaN values in-place
class(DataFrame), intent(inout) :: self
real(kind=dp), intent(in), optional :: value
character(len=*), intent(in), optional :: method
integer :: i, j, n1, n2
real(kind=dp) :: last, nan
character(len=:), allocatable :: mth

if (.not. allocated(self%values)) return

if (present(value) .and. present(method)) then
   error stop "fillna: supply only one of value or method"
end if
if ((.not. present(value)) .and. (.not. present(method))) then
   error stop "fillna: must supply value or method"
end if

n1 = nrow(self)
n2 = ncol(self)
nan = ieee_value(0.0_dp, ieee_quiet_nan)

if (present(value)) then
   do j = 1, n2
      do i = 1, n1
         if (ieee_is_nan(self%values(i,j))) self%values(i,j) = value
      end do
   end do
   return
end if

mth = str_upper(adjustl(trim(method)))

if (trim(mth) == "FFILL") then
   do j = 1, n2
      last = nan
      do i = 1, n1
         if (.not. ieee_is_nan(self%values(i,j))) then
            last = self%values(i,j)
         else
            if (.not. ieee_is_nan(last)) self%values(i,j) = last
         end if
      end do
   end do
else if (trim(mth) == "BFILL") then
   do j = 1, n2
      last = nan
      do i = n1, 1, -1
         if (.not. ieee_is_nan(self%values(i,j))) then
            last = self%values(i,j)
         else
            if (.not. ieee_is_nan(last)) self%values(i,j) = last
         end if
      end do
   end do
else
   error stop "fillna: method must be ffill or bfill"
end if

end subroutine fillna

function dropna(self, axis, how) result(df_new)
! return a new DataFrame with rows or columns dropped based on NaNs
class(DataFrame), intent(in) :: self
character(len=*), intent(in), optional :: axis
character(len=*), intent(in), optional :: how
type(DataFrame) :: df_new
character(len=:), allocatable :: ax, hw
logical, allocatable :: keep_row(:), keep_col(:)
integer :: n1, n2, i, j, k, nk

if (.not. allocated(self%values)) then
   allocate(df_new%index(0))
   allocate(df_new%columns(0))
   allocate(df_new%values(0,0))
   return
end if

n1 = nrow(self)
n2 = ncol(self)

ax = str_upper(adjustl(trim(default("rows", axis))))
hw = str_upper(adjustl(trim(default("any", how))))

if (trim(ax) == "ROWS") then
   allocate(keep_row(n1))
   do i = 1, n1
      if (trim(hw) == "ANY") then
         keep_row(i) = .not. any(ieee_is_nan(self%values(i,:)))
      else if (trim(hw) == "ALL") then
         keep_row(i) = .not. all(ieee_is_nan(self%values(i,:)))
      else
         error stop "dropna: how must be any or all"
      end if
   end do

   nk = count(keep_row)
   allocate(df_new%index(nk))
   allocate(df_new%columns(n2))
   allocate(df_new%values(nk, n2))
   df_new%columns = self%columns

   k = 0
   do i = 1, n1
      if (keep_row(i)) then
         k = k + 1
         df_new%index(k) = self%index(i)
         df_new%values(k,:) = self%values(i,:)
      end if
   end do

else if (trim(ax) == "COLS" .or. trim(ax) == "COLUMNS") then
   allocate(keep_col(n2))
   do j = 1, n2
      if (trim(hw) == "ANY") then
         keep_col(j) = .not. any(ieee_is_nan(self%values(:,j)))
      else if (trim(hw) == "ALL") then
         keep_col(j) = .not. all(ieee_is_nan(self%values(:,j)))
      else
         error stop "dropna: how must be any or all"
      end if
   end do

   nk = count(keep_col)
   allocate(df_new%index(n1))
   allocate(df_new%columns(nk))
   allocate(df_new%values(n1, nk))
   df_new%index = self%index

   k = 0
   do j = 1, n2
      if (keep_col(j)) then
         k = k + 1
         df_new%columns(k) = self%columns(j)
         df_new%values(:,k) = self%values(:,j)
      end if
   end do

else
   error stop "dropna: axis must be rows or cols"
end if

end function dropna




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
