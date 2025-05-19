module dataframe_stats_mod
use kind_mod       , only: dp
use dataframe_mod  , only: DataFrame, nrow
use table_mod      , only: Table
use table_stats_mod, only: stats_table
use basic_stats_mod, only: stats, corr_mat, print_acf
implicit none
private
public :: simple_ret, moving_sum, moving_average, stats, cor, &
   print_acf
interface stats
   module procedure stats_df
end interface stats
interface print_acf
   module procedure print_acf_df
end interface print_acf
contains

elemental function cor(df) result(xtable)
! return correlation matrix of the columns of a dataframe
type(DataFrame), intent(in) :: df
type(Table)                 :: xtable
xtable = Table(index=df%columns, columns=df%columns, &
   values=corr_mat(df%values))
xtable%index = df%columns
end function cor

function stats_df(funcs, df) result(xtable)
! return statistics on each column of a dataframe
character (len=*), intent(in) :: funcs(:)
type(DataFrame), intent(in) :: df
type(Table)                 :: xtable
xtable = stats_table(funcs, df%values, columns=df%columns)
end function stats_df

function simple_ret(df) result(df_ret)
! return simple returns (proportional changes) for each column of a dataframe
type(DataFrame), intent(in) :: df
type(DataFrame)             :: df_ret
integer                     :: nr
nr = nrow(df)
df_ret%columns = df%columns
if (nr > 1) then
   df_ret%index = df%index(2:)
   df_ret%values = df%values(2:,:)/df%values(:nr-1,:) - 1.0_dp
end if
end function simple_ret

function moving_sum(df, nterms) result(df_sum)
! return the moving sums of the columns of a dataframe
type(DataFrame), intent(in) :: df
integer        , intent(in) :: nterms
type(DataFrame)             :: df_sum
integer                     :: i, j, nr
df_sum = df
nr = nrow(df)
do j=1,nr
   i = max(1, j - nterms + 1)
   df_sum%values(j,:) = sum(df%values(i:j,:), dim=1)
end do
end function moving_sum

function moving_average(df, nterms) result(df_sum)
! return the moving averages of the columns of a dataframe
type(DataFrame), intent(in) :: df
integer        , intent(in) :: nterms
type(DataFrame)             :: df_sum
integer                     :: i, j, nr
df_sum = df
nr = nrow(df)
do j=1,nr
   i = max(1, j - nterms + 1)
   df_sum%values(j,:) = sum(df%values(i:j,:), dim=1)/(j-i+1.0_dp)
end do
end function moving_average

impure elemental subroutine print_acf_df(df, nacf, outu, &
   fmt_header, fmt_trailer, title, fmt_acf, fmt_labels)
! print the autocorrelations of the columns of a dataframe
type(DataFrame)  , intent(in) :: df
integer          , intent(in) :: nacf
integer          , intent(in), optional :: outu
character (len=*), intent(in), optional :: title, fmt_header, &
   fmt_trailer, fmt_acf, fmt_labels
call print_acf(df%values, nacf, df%columns, outu=outu, title=title, &
   fmt_header=fmt_header, fmt_trailer=fmt_trailer, &
   fmt_acf=fmt_acf, fmt_labels=fmt_labels)
end subroutine print_acf_df

end module dataframe_stats_mod
