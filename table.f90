module table_mod
use kind_mod, only: dp
use util_mod, only: default, assert_equal
implicit none
private
public :: Table, create_table, t, display

! Named constant for the length of the index strings.
integer, parameter :: len_index = 20
! Number of rows to print by default.
integer, parameter :: nrows_print = 10

!---------------------------------------
! A minimal Table derived type.
!---------------------------------------
type :: Table
   ! The index is now an allocatable character array with fixed length len_index.
   character(len=len_index), allocatable :: index(:)
   character(len=:), allocatable         :: columns(:)
   real(kind=dp), allocatable            :: values(:,:)
   character (len=len_index)             :: index_name=""
   contains
   procedure :: read_csv, display=>display_data, write_csv
   ! no procedure for create_table here, because it's a separate module function
end type Table

contains

!------------------------------------------------------------------
! Utility: split_string
!------------------------------------------------------------------
subroutine split_string(str, delim, tokens)
character(len=*), intent(in)           :: str
character(len=*), intent(in)           :: delim
character(:), allocatable, intent(out) :: tokens(:)
integer :: start, pos, i, count, n

n = len_trim(str)
if (n == 0) then
   allocate(character(len=0) :: tokens(1))
   tokens(1) = ""
   return
end if

! First pass: count tokens.
count = 0
start = 1
do
   pos = index(str(start:), delim)
   if (pos == 0) then
      count = count + 1
      exit
   else
      count = count + 1
      start = start + pos
   end if
end do

! Allocate tokens; each token gets the full length of the input.
allocate(character(len=n) :: tokens(count))

! Second pass: extract tokens.
start = 1
i = 1
do
   pos = index(str(start:), delim)
   if (pos == 0) then
      tokens(i) = adjustl(str(start:))
      exit
   else
      tokens(i) = adjustl(str(start:start+pos-2))
      start = start + pos
      i = i + 1
   end if
end do
end subroutine split_string


!------------------------------------------------------------------
! read_csv
!------------------------------------------------------------------
subroutine read_csv(self, filename)
class(Table), intent(inout) :: self
character(len=*), intent(in) :: filename
integer :: io, unit, i, j, nrows, ncols, maxlen
character(len=1024) :: line
character(:), allocatable :: tokens(:)

open(newunit=unit, file=filename, status='old', action='read', iostat=io)
if (io /= 0) then
   print *, "Error opening file:", filename
   stop
end if

! 1) Read the header line
read(unit, '(A)', iostat=io) line
if (io /= 0) then
   print *, "Error reading header line from:", filename
   stop
end if

call split_string(line, ",", tokens)
ncols = size(tokens) - 1
if (ncols <= 0) then
   print *, "No columns detected in header of", filename
   stop
end if

! Determine maximum length among the column name tokens.
maxlen = 0
do i = 2, size(tokens)
   maxlen = max(maxlen, len_trim(tokens(i)))
end do

! Allocate columns with fixed length = maxlen
allocate(character(len=maxlen) :: self%columns(ncols))
do i = 1, ncols
   self%columns(i) = tokens(i+1)
end do

! 2) Count data lines
nrows = 0
do
   read(unit, '(A)', iostat=io) line
   if (io /= 0 .or. trim(line) == "") exit
   nrows = nrows + 1
end do

if (nrows == 0) then
   print *, "No data lines detected in file:", filename
   stop
end if

! 3) Rewind & skip header
rewind(unit)
read(unit, '(A)')

! 4) Allocate index & values
allocate(self%index(nrows), self%values(nrows, ncols))

! 5) Read data
do i = 1, nrows
   read(unit, '(A)', iostat=io) line
   if (trim(line) == "") exit
   call split_string(line, ",", tokens)
   self%index(i) = tokens(1)  ! truncated if > len_index
   do j = 1, ncols
      read(tokens(j+1), *) self%values(i,j)
   end do
end do

close(unit)
end subroutine read_csv

subroutine display(xtable, print_all, fmt_ir, fmt_header, &
fmt_trailer, title)
type(Table), intent(in) :: xtable
logical, intent(in), optional :: print_all
character(len=*), intent(in), optional :: fmt_ir, fmt_header, fmt_trailer, title
call display_data(xtable, print_all=print_all, fmt_ir=fmt_ir, &
fmt_header=fmt_header, fmt_trailer=fmt_trailer, title=title)
end subroutine display

!------------------------------------------------------------------
! display
!------------------------------------------------------------------
subroutine display_data(self, print_all, fmt_ir, fmt_header, fmt_trailer, title)
class(Table), intent(in) :: self
logical, intent(in), optional :: print_all
character(len=*), intent(in), optional :: fmt_ir, fmt_header, fmt_trailer, title
integer :: total, i, n_top, n_bottom
logical :: print_all_
character(len=100) :: fmt_ir_, fmt_header_

fmt_ir_     = default("(a20,*(1x,f10.4))", fmt_ir)
fmt_header_ = default("(a20,*(1x,a10))",   fmt_header)
print_all_  = default(.false., print_all)
total       = size(self%index)

if (present(title)) write(*,"(a)") title
! Print header
write(*,fmt_header_) trim(self%index_name), self%columns

if (print_all_) then
   do i = 1, total
      write(*,fmt_ir_) self%index(i), self%values(i,:)
   end do
else
   if (total <= nrows_print) then
      do i = 1, total
         write(*,fmt_ir_) self%index(i), self%values(i,:)
      end do
   else
      n_top    = nrows_print / 2
      n_bottom = nrows_print - n_top
      do i = 1, n_top
         write(*,fmt_ir_) self%index(i), self%values(i,:)
      end do
      write(*,*)
      write(*,*) "   ... (", total - nrows_print, " rows omitted) ..."
      write(*,*)
      do i = total - n_bottom + 1, total
         write(*,fmt_ir_) self%index(i), self%values(i,:)
      end do
   end if
end if
if (present(fmt_trailer)) write(*,fmt_trailer)
end subroutine display_data


!------------------------------------------------------------------
! write_csv
!------------------------------------------------------------------
subroutine write_csv(self, filename)
class(Table), intent(in) :: self
character(len=*), intent(in) :: filename
integer :: i, j, unit, io

open(newunit=unit, file=filename, status='replace', action='write', iostat=io)
if (io /= 0) then
   print *, "Error opening output file:", filename
   stop
end if

! Write header: empty token for index, then columns
write(unit,'(A)', advance='no') ""
do j = 1, size(self%columns)
   write(unit,'(",", A)', advance='no') trim(self%columns(j))
end do
write(unit,*)

! Write each data row
do i = 1, size(self%index)
   write(unit,'(A)', advance='no') trim(self%index(i))
   do j = 1, size(self%columns)
      write(unit,'(",", F10.4)', advance='no') self%values(i,j)
   end do
   write(unit,*)
end do

close(unit)
end subroutine write_csv

!------------------------------------------------------------------
! create_table:
!
! Creates and returns a new Table. The arguments are:
!  - values (optional): 2D real array
!  - index  (optional): 1D array of strings
!  - columns(optional): 1D array of strings
!  - index_name(optional): string
! If 'values' is present, it determines #rows x #columns.
! If 'index' or 'columns' is absent, they are automatically generated.
! If 'values' is absent but both 'index' and 'columns' are present, the
! table is allocated with all values set to 0.0.
! Otherwise, if there is not enough info to determine size, we stop.
!------------------------------------------------------------------
function create_table(values, index, columns, value, index_name) &
result(tbl)
real(kind=dp), intent(in), optional :: values(:,:)
character(len=*), intent(in), optional :: index(:), columns(:)
real(kind=dp), intent(in), optional :: value
character(len=*), intent(in), optional :: index_name
type(Table) :: tbl
integer :: nr, nc, i
integer :: maxlen
character(len=len_index) :: tmp_string
character (len=*), parameter :: msg="in table%create_table(), "
if (present(value) .and. present(values)) stop (msg // "should not specify both value and values")
!--------------------------------------------------------------------
! 1) Determine nr x nc from whichever arguments are present
!--------------------------------------------------------------------
if (present(values)) then
   nr = size(values,1)
   nc = size(values,2)
   if (present(columns)) &
   call assert_equal(size(columns), nc, msg // "size(columns)")
   if (present(index)) &
   call assert_equal(size(index), nr, msg // "size(index)")
else
   ! 'values' not present
   if (present(index) .and. present(columns)) then
      nr = size(index)
      nc = size(columns)
   else
      write(*,*) "ERROR in create_table: Not enough info to determine size."
      stop
   end if
end if

!--------------------------------------------------------------------
! 2) If 'index' was not given, generate default row labels r1, r2, ...
!    Otherwise, copy them in (truncating to len_index if needed).
!--------------------------------------------------------------------
allocate(tbl%index(nr))
if (present(index)) then
   do i = 1, nr
      if (i <= size(index)) then
         tbl%index(i) = index(i)  ! truncated if > len_index
      else
         ! If user-supplied index has fewer entries than nr (unlikely),
         ! we fill the remainder with something placeholder
         write(tmp_string, "(A,I0)") "rx", i
         tbl%index(i) = tmp_string
      end if
   end do
else
   ! generate "r1", "r2", ...
   do i = 1, nr
      write(tmp_string, "(A,I0)") "r", i
      tbl%index(i) = tmp_string
   end do
end if

!--------------------------------------------------------------------
! 3) If 'columns' was not given, generate default column labels c1, c2, ...
!    Otherwise, allocate and store them with the appropriate length.
!--------------------------------------------------------------------
if (present(columns)) then
   ! find the max length from user-supplied columns
   maxlen = 0
   do i = 1, nc
      maxlen = max(maxlen, len_trim(columns(i)))
   end do
   allocate(character(len=maxlen) :: tbl%columns(nc))
   do i = 1, nc
      tbl%columns(i) = trim(columns(i))
   end do
else
   ! default columns c1, c2, ...
   ! first find length needed for e.g. "c9999" if nr is large
   ! or just pick a small fixed length if you prefer
   maxlen = 0
   do i = 1, nc
      ! estimate length needed
      ! "c" plus up to ~ len of integer i
      ! we can guess up to 10 chars
      maxlen = max(maxlen, 5)
   end do
   allocate(character(len=maxlen) :: tbl%columns(nc))
   do i = 1, nc
      write(tbl%columns(i), "(A,I0)") "c", i
   end do
end if

!--------------------------------------------------------------------
! 4) Allocate the values array
!    If 'values' is present, store them. Otherwise, set them to 0.0
!--------------------------------------------------------------------
allocate(tbl%values(nr,nc))
if (present(values)) then
   tbl%values = values
else
   tbl%values = default(0.0d0, value)
end if
if (present(index_name)) tbl%index_name = index_name
end function create_table

function t(x) result(xtrans)
! return the transpose of a Table, transposing the values
! and interchanging the index and columns
type(Table), intent(in) :: x
type(Table)             :: xtrans
xtrans = create_table(transpose(x%values), index=x%columns, &
columns=x%index)
end function t

end module table_mod
