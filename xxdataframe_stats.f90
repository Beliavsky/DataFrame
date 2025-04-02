program xxdataframe_stats
! driver for dataframe_stats_mod
use kind_mod, only: dp
use dataframe_mod, only: DataFrame, allocate_df, nrow, ncol, display
use table_mod, only: Table, display
use dataframe_stats_mod, only: moving_average, moving_sum, stats
implicit none
type(DataFrame) :: df
integer, parameter :: n1 = 5, n2 = 2
integer :: i
call allocate_df(df, n1, n2)
print*,nrow(df), ncol(df)
do i=1,nrow(df)
   df%values(i,:) = [10.0_dp, 0.1_dp] * i
end do
call display(df, title="df")
call display(moving_sum(df, 3), title="moving_sum(df, 3)")
call display(moving_average(df, 3), title="moving_average(df, 3)")
call display(stats(["mean", "sd  "], df), title="stats(['mean', 'sd  '], df)")
end program xxdataframe_stats
