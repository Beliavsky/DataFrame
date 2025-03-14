program xxdataframe_stats
use kind_mod, only: dp
use dataframe_mod, only: DataFrame, allocate_df, nrow, ncol, display
use dataframe_stats_mod, only: moving_average, moving_sum
implicit none
type(DataFrame) :: df
integer, parameter :: n1 = 5, n2 = 2
integer :: i
call allocate_df(df, n1, n2)
print*,nrow(df), ncol(df)
do i=1,nrow(df)
   df%values(i,:) = [10.0_dp, 0.1_dp] * i
end do
call display(df)
call display(moving_sum(df, 3))
call display(moving_average(df, 3))
end program xxdataframe_stats
