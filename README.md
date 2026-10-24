# DataFrame
`DataFrame` is a Fortran derived type with indices, columns, and values of type `integer`, `character`, and `real(kind=dp)`. `Table` is similar but has indices of type `character`. With the DataFrame and Table and associated procedures, it is simple to read CSV files and compute and display associated statistics. For example, the [program](https://github.com/Beliavsky/DataFrame/blob/main/xdataframe_stats.f90)

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

reads historical daily closing prices for a few ETFs from a CSV file, which were obtained using the Python script `xget_prices_yahoo.py`. (The dates from the downloaded data file were transformed from YYYY-MM-DD format to YYYYMMDD to simplify reading them in Fortran.) The program then computes returns and prints statistics on those returns, including a correlation matrix. The output is

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
The ETFs are
```
   SPY -- S&P 500 (U.S. stocks)
   EFA -- iShares MSCI EAFE (foreign developed market stocks)
   EEM -- iShares MSCI Emerging Markets (emerging market stocks)
   TLT -- iShares 20+ Year Treasury Bond (U.S. long-term government bonds)
```
The pairwise correlations of returns of all the stock ETFs (SPY, EFA, EEM) all exceed 0.8, while they are negatively correlated to TLT.

## Missing values

`df%isna()` returns a logical matrix marking IEEE NaNs. Infinities are not
missing values. `df%dropna(axis="rows", how="any")` returns a filtered copy,
preserving labels and order; `rows` and `any` are the defaults. Use `axis="cols"`
(or `"columns"`) to filter columns, and `how="all"` to remove only entirely
missing rows or columns. Options are case insensitive. On an empty axis,
`any` removes nothing and `all` removes every row/column along the other axis.

`call df%fillna(value=0.0_dp)` replaces NaNs in place. Alternatively,
`call df%fillna(method="ffill")` propagates the preceding nonmissing value
within each column; `"bfill"` propagates the following value. Leading NaNs
remain after forward filling, trailing NaNs remain after backward filling,
and wholly missing columns remain missing. Supply exactly one of `value`
and `method`. These operations require allocated values; `dropna` also
requires matching index and column labels.

`read_csv` recognizes empty numeric fields and `NA`, `NaN`, and `null`
(case insensitive) as missing. Reading again replaces the previous contents.
Other invalid numeric fields are errors.

Run `python tests/test_missing_values.py` to compile and run the CSV and
edge-case tests with gfortran at `-O0` and `-O2`, including runtime checks and
invalid-option diagnostics. The two Fortran test programs are also included
in `make all` and `make run`.
