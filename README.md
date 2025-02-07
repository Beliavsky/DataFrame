# DataFrame
`DataFrame` is a Fortran derived type with indices, columns, and values of type `integer`, `character`, and `real(kind=dp)`. `Table` is similar but has indices of type `character`. With the DataFrame and Table and associated procedures, it is simple to read CSV files and compute and display associated statistics. For example, the program

```fortran
program xdataframe_stats
use       dataframe_mod, only: DataFrame, print_summary, operator(*)
use dataframe_stats_mod, only: simple_ret
use     table_stats_mod, only: basic_stats_table, corr_table
use           table_mod, only: Table, display
use kind_mod, only: dp
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
end program xdataframe_stats
```

reads historical daily closing prices for a few ETFs from a CSV file, which were obtained using the Python script `xget_prices_yahoo.py`. The dates from the downloaded data file were transformed from YYYY-MM-DD format to YYYYMMDD to simplify reading them in Fortran. The program then computes returns and prints statistics on those returns, including a correlation matrix. The output is

```
data file: spy_efa_eem_tlt.csv
#rows, columns: 5490 4
first, last indices: 20030414 20250205
first, last columns: SPY TLT

             returns        SPY        EFA        EEM        TLT
mean                     0.0492     0.0363     0.0469     0.0180
sd                       1.1693     1.3104     1.7245     0.9159
skew                    -0.0872    -0.0650     0.5054     0.0797
kurt                    15.2201    14.3123    18.2294     3.4888
min                    -10.9424   -11.1633   -16.1661    -6.6682
max                     14.5198    15.8880    22.7700     7.5195

return correlations
                            SPY        EFA        EEM        TLT
SPY                      1.0000     0.8832     0.8183    -0.3127
EFA                      0.8832     1.0000     0.8679    -0.2848
EEM                      0.8183     0.8679     1.0000    -0.2662
TLT                     -0.3127    -0.2848    -0.2662     1.0000
```

```
The ETFs are
   SPY -- S&P 500 (U.S. stocks)
   EFA -- iShares MSCI EAFE (foreign developed market stocks)
   EEM -- iShares MSCI Emerging Markets (emerging market stocks)
   TLT -- iShares 20+ Year Treasury Bond (U.S. long-term government bonds)
```
The pairwise correlations of returns of all the stock ETFs (SPY, EFA, EEM) all exceed 0.8, while they are negatively correlated to TLT.
