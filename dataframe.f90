module dataframe_mod
use kind_mod, only: dp
use util_mod, only: default, split_string
use iso_fortran_env, only: output_unit
implicit none
private
public :: DataFrame, nrow, ncol, print_summary, random, operator(*), &
   operator(/), operator(+), operator(-), display, allocate_df, &
   operator(**)
integer, parameter :: nlen_columns = 100, nrows_print = 10 ! number of rows to print by default.
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
      procedure :: read_csv, display=>display_data, write_csv
end type DataFrame

contains

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

function nrow(df) result(num_rows)
! return the # of rows
type(DataFrame), intent(in) :: df
integer                     :: num_rows
if (allocated(df%values)) then
   num_rows = size(df%values, 1)
else
   num_rows = -1
end if
end function nrow

function ncol(df) result(num_col)
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
subroutine display_data(self, print_all, fmt_ir, fmt_header, title)
class(DataFrame), intent(in) :: self
logical, intent(in), optional :: print_all
character (len=*), intent(in), optional :: fmt_ir, fmt_header, title
integer :: total, i, n_top, n_bottom
logical :: print_all_
character (len=100) :: fmt_ir_, fmt_header_
fmt_ir_ = default("(i10,*(1x,f10.4))", fmt_ir)
fmt_header_ = default("(a10,*(1x,a10))", fmt_header)
print_all_ = default(.false., print_all)
total = size(self%index)
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

function power_df_n(df, n) result(res)
! return df**n element-wise
integer        , intent(in) :: n
type(DataFrame), intent(in) :: df
type(DataFrame)             :: res
res = df
if (allocated(res%values)) res%values = res%values**n
end function power_df_n

function power_df_x(df, x) result(res)
! return df**x element-wise
real(kind=dp), intent(in)   :: x
type(DataFrame), intent(in) :: df
type(DataFrame)             :: res
res = df
if (allocated(res%values)) res%values = res%values**x
end function power_df_x
end module dataframe_mod
