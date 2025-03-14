module dataframe_stats_mod
use kind_mod, only: dp
use dataframe_mod, only: DataFrame, nrow
implicit none
private
public :: simple_ret, moving_sum, moving_average
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

function moving_sum(df, nterms) result(df_sum)
type(DataFrame), intent(in) :: df
integer        , intent(in) :: nterms
type(DataFrame)             :: df_sum
integer                     :: i, j, nr
df_sum = df
nr = nrow(df)
do j=1,nr
   i = max(1, j - nterms + 1)
   df_sum%values(j,:) = sum(df%values(i:j,:), dim=1)
end do
end function moving_sum

function moving_average(df, nterms) result(df_sum)
type(DataFrame), intent(in) :: df
integer        , intent(in) :: nterms
type(DataFrame)             :: df_sum
integer                     :: i, j, nr
df_sum = df
nr = nrow(df)
do j=1,nr
   i = max(1, j - nterms + 1)
   df_sum%values(j,:) = sum(df%values(i:j,:), dim=1)/(j-i+1.0_dp)
end do
end function moving_average
end module dataframe_stats_mod
