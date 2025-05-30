program xreturns_dist
! reads price data, computes returns, and tests if price relatives p(t)/p(t-1)
! are better modeled as normal or lognormal
use       dataframe_mod, only: DataFrame, print_summary, operator(*), &
                               ncol, subset_stride
use dataframe_stats_mod, only: simple_ret
use     table_stats_mod, only: basic_stats_table, corr_table
use           table_mod, only: Table, display
use kind_mod, only: dp
use prob_dist_mod, only: fit_normal_lognormal
use basic_stats_mod, only: skew
implicit none
integer :: i
character (len=*), parameter :: data_file = "spy_efa_eem_tlt.csv"
type(DataFrame) :: df, df_ret
type(Table) :: ret_stats
real(kind=dp), allocatable :: xret(:)
real(kind=dp) :: ll_norm, ll_log
real(kind=dp), parameter :: scale_ret = 1.0_dp
integer :: stride
stride = 1
print "('data file: ', a)", trim(data_file)
call df%read_csv(data_file)
if (stride > 1) then
   print "('stride: ', i0)", stride
   df = subset_stride(df, stride)
end if
call print_summary(df, fmt_trailer="()")
df_ret = scale_ret*simple_ret(df)
ret_stats = basic_stats_table(df_ret%values, df_ret%columns, &
   index_name="returns")
call display(ret_stats, fmt_trailer="()")
call display(corr_table(df_ret%values,df_ret%columns), &
   title="return correlations")
do i=1,ncol(df_ret)
   xret = scale_ret + df_ret%values(:,i)
   print "(/,a,a,f7.3)", "symbol, skew: ", trim(df_ret%columns(i)), skew(xret)
   call fit_normal_lognormal(xret, ll_norm, ll_log)
end do
end program xreturns_dist
