module kind_mod
implicit none
private
public :: dp
integer, parameter :: dp = kind(1.0d0)
end module kind_mod
module util_mod
use iso_fortran_env, only: output_unit
use kind_mod, only: dp
implicit none
private
public :: default, assert_equal, write_merge, split_string, display, &
   print_time_elapsed, read_words_line, str, print_table, exe_name, &
   join, seq, cbind
interface default
   module procedure default_int, default_real, default_logical, &
      default_character
end interface default
interface seq
   module procedure seq_stride, seq_unit_stride
end interface seq
interface cbind
   module procedure cbind_vec_vec, cbind_mat_vec, cbind_mat_mat
end interface cbind
interface display
   module procedure display_matrix, display_vector
end interface display
contains

elemental function default_int(x, xopt) result(y)
! return xopt if present, otherwise x
integer, intent(in) :: x
integer, intent(in), optional :: xopt
integer             :: y
if (present(xopt)) then
   y = xopt
else
   y = x
end if
end function default_int

elemental function default_real(x, xopt) result(y)
! return xopt if present, otherwise x
real(kind=dp), intent(in) :: x
real(kind=dp), intent(in), optional :: xopt
real(kind=dp)             :: y
if (present(xopt)) then
   y = xopt
else
   y = x
end if
end function default_real

elemental function default_logical(x, xopt) result(y)
! return xopt if present, otherwise x
logical, intent(in) :: x
logical, intent(in), optional :: xopt
logical             :: y
if (present(xopt)) then
   y = xopt
else
   y = x
end if
end function default_logical

elemental function default_character(x, xopt) result(y)
! return xopt if present, otherwise x
character (len=*), intent(in) :: x
character (len=*), intent(in), optional :: xopt
character (len=100) :: y
if (present(xopt)) then
   y = xopt
else
   y = x
end if
end function default_character

subroutine assert_equal(k, kreq, msg)
! check that k == kreq
integer, intent(in) :: k, kreq
character (len=*), intent(in) :: msg
if (k /= kreq) then
   print "(a, i0, a, i0)", msg // " = ", k, ", must equal ", kreq
   stop
end if
end subroutine assert_equal

subroutine write_merge(tf, x, y, outu, fmt)
!> Writes either `x` or `y` to the specified output unit using the given format.
!! If `tf` is true, writes `x`; otherwise, writes `y`.
!! @param tf Logical condition determining whether to write `x` or `y`.
!! @param x The first character string to write if `tf` is true.
!! @param y The second character string to write if `tf` is false.
!! @param outu Optional output unit (defaults to a predefined output unit).
!! @param fmt Optional format specifier (defaults to "(a)").
logical, intent(in) :: tf
character (len=*), intent(in) :: x, y
integer, intent(in), optional :: outu
character (len=*), intent(in), optional :: fmt
integer :: outu_
character (len=100) :: fmt_
outu_ = default(output_unit, outu)
if (present(fmt)) then
   fmt_ = fmt
else
   fmt_ = "(a)"
end if
if (tf) then
   write (outu_, fmt_) x
else
   write (outu_, fmt_) y
end if
end subroutine write_merge

!------------------------------------------------------------------
! Utility: split_string
!
! Splits the input string 'str' at each occurrence of the single-
! character delimiter 'delim' and returns the pieces in the allocatable
! array 'tokens'. To allocate each element (with deferred length)
! properly, we use the length of the input string.
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

subroutine display_matrix(x, outu, fmt_r, fmt_header, title)
! print a matrix
real(kind=dp)    , intent(in)           :: x(:,:)
integer          , intent(in), optional :: outu
character (len=*), intent(in), optional :: fmt_r, fmt_header, title
integer                                 :: i, outu_
character (len=100)                     :: fmt_r_
outu_  = default(output_unit, outu)
fmt_r_ = default("(*(1x,f10.4))", fmt_r)
if (present(fmt_header)) write(outu_, fmt_header)
if (present(title)) write (outu_, "(a)") title
do i=1,size(x,1)
   write(outu_,fmt_r_) x(i,:)
end do
end subroutine display_matrix

subroutine display_vector(x, outu, fmt_r, fmt_header, title)
! print a vector
real(kind=dp)    , intent(in)           :: x(:)
integer          , intent(in), optional :: outu
character (len=*), intent(in), optional :: fmt_r, fmt_header, title
integer                                 :: i, outu_
character (len=100)                     :: fmt_r_
outu_  = default(output_unit, outu)
fmt_r_ = default("(*(1x,f10.4))", fmt_r)
if (present(fmt_header)) write(outu_, fmt_header)
if (present(title)) write (outu_, "(a)") title
do i=1,size(x)
   write(outu_,fmt_r_) x(i)
end do
end subroutine display_vector

subroutine print_time_elapsed(old_time, outu)
real(kind=dp), intent(in) :: old_time ! previously set by call cpu_time(old_time)
real(kind=dp)             :: tt
integer      , intent(in), optional :: outu
integer                             :: outu_
character (len=100) :: fmt_time_
outu_ = default(output_unit, outu)
call cpu_time(tt)
fmt_time_= "('time elapsed (s): ', f0.4)"
write (outu_, fmt_time_) tt - old_time
end subroutine print_time_elapsed

subroutine read_words_line(iu,words)
! read words from line, where the line has the # of words followed by the words
! n word_1 word_2 ... word_n
integer          , intent(in)               :: iu
character (len=*), intent(out), allocatable :: words(:)
integer :: ierr, nwords
character (len=10000) :: text
read (iu,"(a)") text
read (text, *) nwords
allocate (words(nwords))
read (text, *, iostat=ierr) nwords, words
if (ierr /= 0) then
   print*,"could not read ", nwords, " words from '" // trim(text) // "'"
   error stop
end if
end subroutine read_words_line

function str(i) result(text)
! convert integer to string
integer, intent(in) :: i
character (len=20) :: text
write (text,"(i0)") i
end function str

subroutine print_table(x, row_names, col_names, outu, &
   fmt_col_names, fmt_row, fmt_header, fmt_trailer)
! print a table with row and column names
real(kind=dp)    , intent(in) :: x(:,:) ! matrix to be printed
character (len=*), intent(in) :: row_names(:), col_names(:)
integer          , intent(in), optional :: outu ! output unit
character (len=*), intent(in), optional :: fmt_col_names, fmt_row, &
   fmt_header, fmt_trailer
integer                       :: i, n1, n2, outu_
character (len=*), parameter  :: msg="in print_table, "
character (len=100) :: fmt_col_names_, fmt_row_
n1 = size(x, 1)
n2 = size(x, 2)
call assert_equal(size(row_names), n1, msg // "size(row_names)")
call assert_equal(size(col_names), n2, msg // "size(col_names)")
fmt_col_names_ = default("(*(a12,:,1x))", fmt_col_names)
fmt_row_ = default("(a12, *(1x,f12.6))", fmt_row)
outu_ = default(output_unit, outu)
if (present(fmt_header)) write (outu_, fmt_header)
write (outu_, fmt_col_names_) "", (trim(col_names(i)), i=1,n2)
do i=1,n1
   write (outu_, fmt_row_) trim(row_names(i)), x(i,:)
end do
if (present(fmt_trailer)) write (outu_, fmt_trailer)
end subroutine print_table

function exe_name() result(xname)
! return the program name
character (len=1000) :: xname
call get_command_argument(0,xname)
xname = trim(xname)
end function exe_name

function join(words,sep) result(str)
! trim and concatenate a vector of character variables,
! inserting sep between them
character (len=*), intent(in)                                   :: words(:),sep
character (len=(size(words)-1)*len(sep) + sum(len_trim(words))) :: str
integer                                                         :: i,nw
nw  = size(words)
str = ""
if (nw < 1) then
   return
else
   str = words(1)
end if
do i=2,nw
   str = trim(str) // sep // words(i) 
end do
end function join

pure function seq_stride(first, last, stride) result(vec)
!! return an integer sequence from first through last
integer, intent(in) :: first, last, stride
integer, allocatable :: vec(:)
integer :: i, n, idiff
idiff = last - first
n = max(0, 1 + idiff/stride)
allocate (vec(n))
do i=1, n
   vec(i) = first + (i - 1) * stride
end do
end function seq_stride

pure function seq_unit_stride(first, last) result(vec)
!! return an integer sequence from first through last
integer, intent(in) :: first, last
integer, allocatable :: vec(:)
integer :: i, n
n = max(0, last - first + 1)
allocate (vec(n))
do i=1, n
   vec(i) = first + i - 1
end do
end function seq_unit_stride

pure function cbind_vec_vec(x,y) result(xy)
! return a matrix whose columns are x(:) and y(:)
real(kind=dp), intent(in) :: x(:), y(:)
real(kind=dp), allocatable :: xy(:,:)
integer :: n
n = size(x,1)
if (size(y) /= n) error stop "mismatched sizes in cbind"
xy = reshape([x, y], [n, 2])
end function cbind_vec_vec

pure function cbind_mat_vec(x,y) result(xy)
! append vector y(:) to matrix x(:,:)
real(kind=dp), intent(in) :: x(:,:), y(:)
real(kind=dp), allocatable :: xy(:,:)
integer :: n1, n2
n1 = size(x,1)
if (size(y) /= n1) error stop "mismatched sizes in cbind"
n2 = size(x,2)
allocate (xy(n1, n2+1))
xy(:,:n2)  = x
xy(:,n2+1) = y 
end function cbind_mat_vec

pure function cbind_mat_mat(x,y) result(xy)
! append columns of y(:,:) to matrix x(:,:)
real(kind=dp), intent(in) :: x(:,:), y(:,:)
real(kind=dp), allocatable :: xy(:,:)
integer :: n1, n2
n1 = size(x,1)
if (size(y,1) /= n1) error stop "mismatched sizes in cbind"
n2 = size(x,2)
allocate (xy(n1, n2+size(y,2)))
xy(:,:n2)  = x
xy(:,n2+1:) = y 
end function cbind_mat_mat

! function appended_char_vec(x, y) result(xy)
! character (len=*), intent(in) :: x(:)
! character (len=*), intent(in) :: y
! character (len=len(x)), allocatable :: xy(:)
!  
! end function appended_char_vec

end module util_mod
module basic_stats_mod
use iso_fortran_env, only: output_unit
use kind_mod, only: dp
use util_mod, only: default, print_table
implicit none
private
public :: mean, variance, sd, mean_and_sd, kurtosis, basic_stats, &
   print_basic_stats, basic_stats_names, correl, acf, nbasic_stats, &
   stat, stats, corr_mat, rms, moving_sum, moving_average, &
   print_corr_mat, skew, cov, cov_mat, print_cov_mat, print_acf_mat, &
   print_acf, bug
integer, parameter :: nbasic_stats = 6
character (len=*), parameter :: basic_stats_names(nbasic_stats) = &
   [character(len=4) :: "mean", "sd", "skew", "kurt", "min", "max"]
real(kind=dp), parameter :: bad_value = -huge(1.0d0)
interface stats
   module procedure stats_many_vec, stats_many_mat
end interface stats
interface print_acf
   module procedure print_acf_vec, print_acf_mat
end interface print_acf   
interface print_basic_stats
   module procedure print_basic_stats_vec, print_basic_stats_mat
end interface print_basic_stats
interface acf
   module procedure acf_vec, acf_mat
end interface acf
contains

function stats_many_vec(funcs, x) result(y)
! return statistics on x(:)
character (len=*), intent(in) :: funcs(:)
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: y(size(funcs))
integer :: i
do i=1,size(funcs)
   y(i) = stat(funcs(i), x)
end do
end function stats_many_vec

function stats_many_mat(funcs, x) result(y)
! return a matrix of statistics on each column of x(:,:)
character (len=*), intent(in) :: funcs(:)
real(kind=dp), intent(in) :: x(:,:)
real(kind=dp) :: y(size(funcs), size(x,2))
integer :: i
do i=1,size(x,2)
   y(:,i) = stats_many_vec(funcs, x(:,i))
end do
end function stats_many_mat

function stat(func, x) result(y)
! return a statistic on x(:)
character (len=*), intent(in) :: func
real(kind=dp), intent(in) :: x(:)
real(kind=dp)             :: y
select case(func)
   case ("mean")    ; y = mean(x)
   case ("sd")      ; y = sd(x)
   case ("variance"); y = variance(x)
   case ("skew")    ; y = skew(x)
   case ("kurt")    ; y = kurtosis(x)
   case ("min")     ; y = minval(x)
   case ("max")     ; y = maxval(x)
   case ("first")
      if (size(x) > 0) then
         y = x(1)
      else
         y = bad_value
      end if 
   case ("last")
      if (size(x) > 0) then
         y = x(size(x))
      else
         y = bad_value
      end if 
   case default ; y = -huge(x)
end select
end function stat

pure function mean(x) result(xmean)
! return the mean of x(:)
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: xmean
xmean = sum(x)/max(1,size(x))
end function mean

pure function sd(x) result(xsd)
! return the standard deviation of x(:)
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: xsd
real(kind=dp) :: m, var
integer :: n
n = size(x)
m = sum(x) / n
var = sum((x - m)**2) / (n-1)
xsd = sqrt(max(0.0_dp, var))
end function sd

pure function rms(x) result(xrms)
! return the root-mean-square of x(:)
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: xrms
xrms = sqrt(sum(x**2)/size(x))
end function rms

pure function mean_and_sd(x) result(res)
! return the mean and standard deviation of x(:)
real(kind=dp), intent(in) :: x(:)
real(kind=dp)             :: res(2)
real(kind=dp)             :: var
integer :: n
n = size(x)
res(1) = sum(x) / n
var = sum((x - res(1))**2) / (n-1)
res(2) = sqrt(max(0.0_dp, var))
end function mean_and_sd

pure function variance(x) result(var)
! return the variance of x(:)
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: var, m
integer :: n
n = size(x)
m = sum(x) / n
var = sum((x - m)**2) / (n-1)
end function variance

pure function skew(x) result(skew_val)
! return the skewness of x
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: skew_val
real(kind=dp) :: mean_x, sd_x
integer :: n
n = size(x)
mean_x = mean(x)
sd_x = sd(x)
skew_val = sum(((x - mean_x) / sd_x)**3) / n
end function skew

pure function kurtosis(x) result(kurtosis_val)
! return the kurtosis of x
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: kurtosis_val
real(kind=dp) :: mean_x, sd_x
integer :: n
n = size(x)
mean_x = mean(x)
sd_x = sd(x)
kurtosis_val = sum(((x - mean_x) / sd_x)**4) / n - 3.0_dp
end function kurtosis

pure function basic_stats(x) result(stats)
real(kind=dp), intent(in) :: x(:)
real(kind=dp)             :: stats(nbasic_stats)
stats = [mean(x), sd(x), skew(x), kurtosis(x), minval(x), maxval(x)]
end function basic_stats

subroutine print_basic_stats_vec(x, outu, fmt_header, fmt_trailer, &
   title, fmt_r, fmt_stats_names)
! print stats on a 1-D array
real(kind=dp), intent(in) :: x(:)
integer, intent(in), optional :: outu
character (len=*), intent(in), optional :: fmt_header, fmt_trailer, &
   title, fmt_r, fmt_stats_names
character (len=100) :: fmt_r_, fmt_stats_names_
integer :: i, outu_
if (present(fmt_stats_names)) then
   fmt_stats_names_ = fmt_stats_names
else
   fmt_stats_names_ = "(*(a10))"
end if
if (present(fmt_r)) then
   fmt_r_ = fmt_r
else
   fmt_r_ = "(*(f10.4))"
end if
outu_ = default(output_unit, outu)
if (present(fmt_header)) write (outu_, fmt_header)
if (present(title)) write (outu_, "(a)") title
write (outu_, fmt_stats_names_) (trim(basic_stats_names(i)), i=1,nbasic_stats)
write (outu_, fmt_r_) basic_stats(x)
end subroutine print_basic_stats_vec

subroutine print_basic_stats_mat(x, labels, outu, &
   fmt_header, fmt_trailer, title, fmt_cr, fmt_stats_names)
! print stats on a 2-D array
real(kind=dp), intent(in) :: x(:,:)
character (len=*), intent(in) :: labels(:)
integer, intent(in), optional :: outu
character (len=*), intent(in), optional :: fmt_header, fmt_trailer, &
   title, fmt_cr, fmt_stats_names
character (len=100) :: fmt_cr_, fmt_stats_names_
integer :: i, outu_
if (present(fmt_stats_names)) then
   fmt_stats_names_ = fmt_stats_names
else
   fmt_stats_names_ = "(*(a10))"
end if
if (present(fmt_cr)) then
   fmt_cr_ = fmt_cr
else
   fmt_cr_ = "(*(f10.4))"
end if
outu_ = default(output_unit, outu)
if (present(fmt_header)) write (outu_, fmt_header)
if (present(title)) write (outu_, "(a)") title
write (outu_, fmt_stats_names_) "", (trim(basic_stats_names(i)), i=1,nbasic_stats)
do i=1,size(x, 2)
   write (outu_, fmt_cr_) trim(labels(i)), basic_stats(x(:,i))
end do
if (present(fmt_trailer)) write (outu_, fmt_trailer)
end subroutine print_basic_stats_mat

pure function correl(x, y) result(corr_xy)
! Returns the linear Pearson correlation of x(:) and y(:)
! Returns a correlation < -1.0_dp to signal an error
real(kind=dp), intent(in) :: x(:), y(:)
real(kind=dp) :: corr_xy
real(kind=dp) :: x_mean, y_mean, cov_xy, var_x, var_y
integer :: n
n = size(x)
if (n /= size(y) .or. n == 0) then
   corr_xy = -2.0_dp
   return
end if
x_mean = sum(x) / n
y_mean = sum(y) / n
cov_xy = sum((x - x_mean) * (y - y_mean))
var_x  = sum((x - x_mean)**2)
var_y  = sum((y - y_mean)**2)
if (var_x <= 0.0_dp .or. var_y <= 0.0_dp) then
   corr_xy = -3.0_dp
else
   corr_xy = cov_xy / sqrt(var_x * var_y)
end if
end function correl

pure function cov(x, y) result(cov_xy)
! Returns the covariance of two 1D arrays
real(kind=dp), intent(in) :: x(:), y(:)
real(kind=dp) :: cov_xy
real(kind=dp) :: x_mean, y_mean
integer :: n
n = size(x)
if (n /= size(y) .or. n == 0) then
   error stop "x and y must have same size > 0 in cov"
end if
x_mean = sum(x) / n
y_mean = sum(y) / n
cov_xy = sum((x - x_mean) * (y - y_mean))
end function cov

pure function acf_vec(x, nacf) result(xacf)
! return the autocorrelations at lags 1 through nacf
real(kind=dp), intent(in) :: x(:)         ! Input array
integer, intent(in) :: nacf               ! Number of autocorrelations to compute
real(kind=dp) :: xacf(nacf)               ! Output array for autocorrelations
real(kind=dp) :: denom
real(kind=dp), allocatable :: xdm(:)      ! Demeaned version of x
integer :: n, lag
n = size(x)
xdm = x - mean(x)                          ! Compute demeaned x
denom = sum(xdm**2)
! Compute autocorrelation for each lag from 1 to nacf
do lag = 1, nacf
   xacf(lag) = sum(xdm(1:n-lag) * xdm(lag+1:n)) / denom
end do
end function acf_vec

pure function acf_mat(x, nacf) result(xacf)
! return the autocorrelations at lags 1 through nacf
real(kind=dp), intent(in) :: x(:,:)       ! Input array
integer, intent(in) :: nacf               ! Number of autocorrelations to compute
real(kind=dp) :: xacf(nacf,size(x,2))     ! Output array for autocorrelations
integer :: icol
do icol=1,size(x,2)
   xacf(:,icol) = acf(x(:,icol), nacf)
end do
end function acf_mat

subroutine print_acf_vec(x, nacf, label, outu, fmt_header, &
   fmt_trailer, title, fmt_acf, fmt_label)
! print the autocorrelations at lags 1 through nacf of x(:)
real(kind=dp), intent(in) :: x(:)       ! Input array
integer, intent(in) :: nacf             ! Number of autocorrelations to compute
character (len=*), intent(in), optional :: title, label, &
   fmt_header, fmt_trailer, fmt_acf, fmt_label
character (len=100) :: fmt_acf_, fmt_label_
integer, intent(in), optional :: outu
real(kind=dp) :: xacf(nacf)     
integer :: iacf, outu_
outu_ = default(output_unit, outu)
fmt_label_ = default("(6x,a8)", fmt_label)
if (present(fmt_header)) write (outu_, fmt_header)
if (present(title)) write (outu_, "(a)") title
if (present(label)) write (outu_,fmt_label_) label
fmt_acf_ = default("('ACF_', i2.2, f8.4)", fmt_acf)
xacf = acf_vec(x, nacf)
do iacf=1,nacf
   write (outu_, fmt_acf_) iacf, xacf(iacf)
end do
if (present(fmt_trailer)) write (outu_, fmt_trailer)
end subroutine print_acf_vec

subroutine print_acf_mat(x, nacf, labels, outu, fmt_header, &
   fmt_trailer, title, fmt_acf, fmt_labels)
! print the autocorrelations at lags 1 t hrough nacf of the columns of x(:,:)
real(kind=dp), intent(in) :: x(:,:)       ! Input array
integer, intent(in) :: nacf               ! Number of autocorrelations to compute
character (len=*), intent(in), optional :: title, labels(:), &
   fmt_header, fmt_trailer, fmt_acf, fmt_labels
integer, intent(in), optional :: outu
real(kind=dp) :: xacf(nacf,size(x,2))
integer :: iacf, icol, outu_
character (len=100) :: fmt_acf_, fmt_labels_
outu_ = default(output_unit, outu)
fmt_labels_ = default("(6x,*(a8))", fmt_labels)
if (present(fmt_header)) then
   print*,"fmt_header = '" // trim(fmt_header) // "'" ! debug
   write (outu_, fmt_header)
end if
if (present(title)) write (outu_, "(a)") title
if (present(labels)) write (outu_, fmt_labels_) &
   (trim(labels(icol)), icol=1,size(labels))
xacf = acf_mat(x, nacf)
fmt_acf_ = default("('ACF_', i2.2, *(f8.4))", fmt_acf)
do iacf=1,nacf
   write (outu_, fmt_acf_) iacf, xacf(iacf,:)
end do
if (present(fmt_trailer)) write (outu_, fmt_trailer)
end subroutine print_acf_mat

subroutine print_corr_mat(x, col_names, outu, fmt_col_names, fmt_row, &
   fmt_header, fmt_trailer)
! print the correlation matrix of the columns of x(:,:)
real(kind=dp), intent(in) :: x(:,:)
character (len=*), intent(in) :: col_names(:)
integer          , intent(in), optional :: outu ! output unit
character (len=*), intent(in), optional :: fmt_header, fmt_trailer, &
   fmt_col_names, fmt_row
character (len=100) :: fmt_col_names_, fmt_row_
fmt_col_names_ = default("(*(a8,:,1x))", fmt_col_names)
fmt_row_ = default("(a8, *(1x,f8.4))", fmt_row)
call print_table(corr_mat(x), row_names=col_names, col_names=col_names, &
   fmt_header=fmt_header, fmt_trailer=fmt_trailer, outu=outu, &
   fmt_col_names=fmt_col_names_, fmt_row=fmt_row_)
end subroutine print_corr_mat

subroutine print_cov_mat(x, col_names, outu, fmt_col_names, fmt_row, &
   fmt_header, fmt_trailer)
! print the covariance matrix of the columns of x(:,:)
real(kind=dp), intent(in) :: x(:,:)
character (len=*), intent(in) :: col_names(:)
integer          , intent(in), optional :: outu ! output unit
character (len=*), intent(in), optional :: fmt_header, fmt_trailer, &
   fmt_col_names, fmt_row
character (len=100) :: fmt_col_names_, fmt_row_
fmt_col_names_ = default("(*(a8,:,1x))", fmt_col_names)
fmt_row_ = default("(a8, *(1x,f8.4))", fmt_row)
call print_table(cov_mat(x), row_names=col_names, col_names=col_names, &
   fmt_header=fmt_header, fmt_trailer=fmt_trailer, outu=outu, &
   fmt_col_names=fmt_col_names_, fmt_row=fmt_row_)
end subroutine print_cov_mat

pure function corr_mat(x) result(cor)
    ! return the correlation matrix of the columns of x(:,:)
    real(kind=dp), intent(in) :: x(:,:)
    real(kind=dp)             :: cor(size(x,2), size(x,2))
    real(kind=dp)             :: mean_vec(size(x,2)), std_vec(size(x,2))
    real(kind=dp)             :: centered_x(size(x,1), size(x,2))
    integer                   :: n, p

    n = size(x, 1)  ! Number of rows
    p = size(x, 2)  ! Number of columns

    ! Compute the mean of each column
    mean_vec = sum(x, dim=1) / n

    ! Center the matrix by subtracting the mean of each column
    centered_x = x - spread(mean_vec, dim=1, ncopies=n)

    ! Compute the standard deviation of each column
    std_vec = sqrt(sum(centered_x**2, dim=1) / (n - 1))

    cor = matmul(transpose(centered_x), centered_x) / (n - 1)
    cor = cor / spread(std_vec, dim=1, ncopies=p)
    cor = cor / spread(std_vec, dim=2, ncopies=p)
end function corr_mat

pure function cov_mat(x) result(xcov)
    ! return the covariance matrix of the columns of x(:,:)
    real(kind=dp), intent(in) :: x(:,:)
    real(kind=dp)             :: xcov(size(x,2), size(x,2))
    real(kind=dp)             :: mean_vec(size(x,2)), std_vec(size(x,2))
    real(kind=dp)             :: centered_x(size(x,1), size(x,2))
    integer                   :: n, p

    n = size(x, 1)  ! Number of rows
    p = size(x, 2)  ! Number of columns

    ! Compute the mean of each column
    mean_vec = sum(x, dim=1) / n

    ! Center the matrix by subtracting the mean of each column
    centered_x = x - spread(mean_vec, dim=1, ncopies=n)

    ! Compute the standard deviation of each column
    std_vec = sqrt(sum(centered_x**2, dim=1) / (n - 1))
    xcov = matmul(transpose(centered_x), centered_x) / (n - 1)
end function cov_mat

pure function moving_sum(x, k) result(xsum)
! return a moving sum of x(:) with k terms, using fewer terms for i < k
real(kind=dp), intent(in) :: x(:)
integer      , intent(in) :: k
real(kind=dp)             :: xsum(size(x))
integer                   :: i, n
n = size(x)
if (n < 1) return
if (k < 1) then
   xsum = 0.0_dp
   return
end if
xsum(1) = x(1)
do i=2,min(k, n)
   xsum(i) = xsum(i-1) + x(i)
end do
do i=k+1, n
   xsum(i) = xsum(i-1) + x(i) - x(i-k)
end do
end function moving_sum

pure function moving_average(x, k) result(xma)
! return a moving average of x(:) with k terms, using fewer terms for i < k
real(kind=dp), intent(in) :: x(:)
integer      , intent(in) :: k
real(kind=dp)             :: xma(size(x))
integer                   :: i, n
real(kind=dp)             :: xsum(size(x))
n = size(x)
if (k < 1) then
   xma = 0.0_dp
   return
end if
xsum = moving_sum(x, k)
do i=1,min(k, n)
   xma(i) = xsum(i)/i
end do
do i=k+1,n
   xma(i) = xsum(i)/k
end do
end function moving_average

subroutine bug(x, nacf, labels, outu, fmt_header, &
   fmt_trailer, title, fmt_acf, fmt_labels)
! print the autocorrelations at lags 1 t hrough nacf of the columns of x(:,:)
real(kind=dp), intent(in) :: x(:,:)       ! Input array
integer, intent(in) :: nacf               ! Number of autocorrelations to compute
character (len=*), intent(in), optional :: title, labels(:), &
   fmt_header, fmt_trailer, fmt_acf, fmt_labels
integer, intent(in), optional :: outu
real(kind=dp) :: xacf(nacf,size(x,2))
integer :: iacf, icol, outu_
character (len=100) :: fmt_acf_, fmt_labels_
outu_ = default(output_unit, outu)
fmt_labels_ = default("(6x,*(a8))", fmt_labels)
if (present(fmt_header)) then
   print*,"fmt_header = '" // trim(fmt_header) // "'" ! debug
   write (outu_, fmt_header)
end if
if (present(title)) write (outu_, "(a)") title
if (present(labels)) write (outu_, fmt_labels_) &
   (trim(labels(icol)), icol=1,size(labels))
xacf = acf_mat(x, nacf)
fmt_acf_ = default("('ACF_', i2.2, *(f8.4))", fmt_acf)
do iacf=1,nacf
   write (outu_, fmt_acf_) iacf, xacf(iacf,:)
end do
if (present(fmt_trailer)) write (outu_, fmt_trailer)
end subroutine bug
end module basic_stats_mod
module dataframe_stats_mod
use basic_stats_mod, only: print_acf
implicit none
private
public :: print_acf
interface print_acf
  module procedure print_acf_df
end interface print_acf
contains

impure elemental subroutine print_acf_df(nacf, outu, fmt_header)
! print the autocorrelations of the columns of a dataframe
! type(DataFrame)  , intent(in) :: df
integer          , intent(in) :: nacf
integer          , intent(in), optional :: outu
character (len=*), intent(in), optional :: fmt_header
if (present(fmt_header)) print*,"(4) in print_acf_df, fmt_header = '" // trim(fmt_header) // "'"
end subroutine print_acf_df

end module dataframe_stats_mod
program xdataframe_stats
use dataframe_stats_mod, only: print_acf
implicit none
print*,"calling print_acf with no title"
call print_acf(nacf = 5, fmt_header="()")
stop "here"
end program xdataframe_stats
