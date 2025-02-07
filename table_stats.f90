module table_stats_mod
use kind_mod, only: dp
use table_mod, only: Table, create_table
use basic_stats_mod, only: basic_stats, basic_stats_names, nbasic_stats, &
                           stats, corr_mat
use util_mod, only: assert_equal
implicit none
private
public :: basic_stats_table, stats_table, corr_table
contains

function basic_stats_table(x, columns, index_name) result(y)
! return a table where each column has the statistics of a column of x(:,:)
real(kind=dp), intent(in) :: x(:,:)
character (len=*), intent(in), optional :: columns(:)
character (len=*), intent(in), optional :: index_name
type(Table) :: y
real(kind=dp), allocatable :: xstats(:,:)
integer :: icol, ncol
ncol = size(x, 2)
allocate (xstats(nbasic_stats, ncol))
do icol=1,ncol
   xstats(:,icol) = basic_stats(x(:,icol))
end do
y = create_table(values=xstats, index=basic_stats_names, &
   columns=columns, index_name=index_name)
end function basic_stats_table

function stats_table(funcs, x, columns) result(y)
! return a table where each column has the statistics of a column of x(:,:)
character (len=*), intent(in) :: funcs(:)
real(kind=dp), intent(in) :: x(:,:)
character (len=*), intent(in), optional :: columns(:)
type(Table) :: y
real(kind=dp), allocatable :: xstats(:,:)
integer :: icol, ncol
ncol = size(x, 2)
allocate (xstats(size(funcs), ncol))
do icol=1,ncol
   xstats(:,icol) = stats(funcs, x(:,icol))
end do
y = create_table(values=xstats, index=funcs, &
   columns=columns)
end function stats_table

function corr_table(x, columns) result(y)
real(kind=dp), intent(in) :: x(:,:)
character (len=*), intent(in) :: columns(:)
type(Table) :: y
call assert_equal(size(columns), size(x,2), &
  "in corr_table, size(columns) /= size(x,2)")
y = create_table(values=corr_mat(x), index=columns, columns=columns)
end function corr_table
end module table_stats_mod
