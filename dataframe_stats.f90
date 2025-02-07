module dataframe_stats_mod
use kind_mod, only: dp
use dataframe_mod, only: DataFrame, nrow
implicit none
private
public :: simple_ret
contains

function simple_ret(df) result(df_ret)
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
end module dataframe_stats_mod
