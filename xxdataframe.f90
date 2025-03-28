program xxdataframe
use dataframe_mod, only: DataFrame, random, display, operator(*), &
   operator(/), operator(+), operator(-), operator(**), subset_stride
use kind_mod, only: dp
implicit none
type(DataFrame) :: df, df_new(2)
call random(df, 3, 2)
call display(df)
call display(10.0_dp*df  , title="10.0_dp*df")
call display(df*10.0_dp  , title="df*10.0_dp")
call display(df/10.0_dp  , title="df/10.0_dp")
call display(df + 10.0_dp, title="df + 10.0_dp")
call display(10.0_dp + df, title="10.0_dp + df")
call display(df - 10.0_dp, title="df - 10.0_dp")
call display(10.0_dp - df, title="10.0_dp - df")
call display(10*df  , title="10*df")
call display(df*10  , title="df*10")
call display(df/10  , title="df/10")
call display(10/df  , title="10/df")
call display(df + 10, title="df + 10")
call display(10 + df, title="10 + df")
call display(df - 10, title="df - 10")
call display(10 - df, title="10 - df")
call display(df**2  , title="df**2")
call display(df**2.0_dp, title="df**2.0_dp")
call random(df, 8, 2)
call display(df)
df_new = subset_stride(df, [3, 4])
call display(df_new)
end program xxdataframe
