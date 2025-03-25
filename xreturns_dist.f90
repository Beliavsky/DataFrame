program xreturns_dist
!! study if 1+returns are better fit by a normal or lognormal distribution
use       dataframe_mod, only: DataFrame, print_summary, operator(*), ncol
use dataframe_stats_mod, only: simple_ret
use     table_stats_mod, only: basic_stats_table, corr_table
use           table_mod, only: Table, display
use kind_mod, only: dp
use prob_dist_mod
use basic_stats_mod, only: skew
implicit none
integer :: i
character (len=*), parameter :: data_file = "spy_efa_eem_tlt.csv"
type(DataFrame) :: df, df_ret
type(Table) :: ret_stats
real(kind=dp), allocatable :: xret(:)
real(kind=dp) :: ll_norm, ll_log
real(kind=dp), parameter :: scale_ret = 1.0_dp
print "('data file: ', a)", trim(data_file)
call df%read_csv(data_file)
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
