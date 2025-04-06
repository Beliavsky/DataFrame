program xdataframe_stats
use            kind_mod, only: dp
use       dataframe_mod, only: DataFrame, print_summary, operator(*), &
                               operator(**)
use dataframe_stats_mod, only: simple_ret, cor, print_acf
use     table_stats_mod, only: basic_stats_table, corr_table
use           table_mod, only: Table, display
use basic_stats_mod, only: bug
implicit none
character (len=*), parameter :: data_file = "spy_efa_eem_tlt.csv"
type(DataFrame) :: df, df_ret
type(Table) :: ret_stats
print "('data file: ', a)", trim(data_file)
call df%read_csv(data_file)
call print_summary(df, fmt_trailer="()")
df_ret = 100.0_dp*simple_ret(df)
ret_stats = basic_stats_table(df_ret%values, df_ret%columns, &
   index_name="returns")
call display(ret_stats, fmt_trailer="()")
call display(corr_table(df_ret%values,df_ret%columns), &
   title="return correlations")
call display(cor(df_ret))
call print_acf(df_ret, nacf=5)
call print_acf(df_ret**2, nacf=5)
print*,"calling bug, fmt_header = '()'" ! debug
call bug(df_ret%values, nacf=5, fmt_header="()")
print*,"calling print_acf with no title"
call print_acf(df_ret, nacf=5, fmt_header="()")
stop "here"
print*,"calling print_acf with no title, powers [1, 2]"
call print_acf(df_ret**[1,2], nacf=5, fmt_header="()")
stop "here (1)"
print*,"calling print_acf(df_ret**[1,2], fmt_header = '()')" ! debug
call print_acf(df_ret**[1,2], nacf=5, title=["ret   ","ret**2"], &
   fmt_header="()")
stop "here (2)"
call print_acf(df_ret**[1,2], nacf=5, title=["ret   ","ret**2"], &
   fmt_header="('abc')")
end program xdataframe_stats
