program xtable_stats
! test basic_stats_table
use kind_mod, only: dp
use table_stats_mod, only: basic_stats_table, stats_table, corr_table
use table_mod, only: Table, t, display
implicit none
integer, parameter :: n = 1000, ncol=2
real(kind=dp) :: x(n, ncol)
type(Table) :: xtable
call random_number(x)
x(:,2) = x(:,2) + x(:,1)
xtable = basic_stats_table(x, columns=["a", "b"])
call display(xtable, fmt_trailer="()")
call display(t(xtable), title="transpose", fmt_trailer="()")
call display(stats_table(["min", "max"], x), fmt_trailer="()")
call display(corr_table(x, columns=["a", "b"]), &
   title="correlation matrix")
end program xtable_stats
