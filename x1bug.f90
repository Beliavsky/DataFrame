module basic_stats_mod
implicit none
private
public :: foo
interface foo
   module procedure foo_mat
end interface foo
contains

subroutine foo_mat(x, nacf, label)
real, intent(in) :: x(:,:)
integer, intent(in) :: nacf
character (len=*), intent(in), optional :: label
end subroutine foo_mat

end module basic_stats_mod

module dataframe_stats_mod
use basic_stats_mod, only: foo
implicit none
private
public :: foo
interface foo
  module procedure foo_df
end interface foo
contains

impure elemental subroutine foo_df(nacf, outu, fmt_header)
integer          , intent(in) :: nacf
integer          , intent(in), optional :: outu
character (len=*), intent(in), optional :: fmt_header
if (present(fmt_header)) print*,"(4) in foo_df, fmt_header = '" // trim(fmt_header) // "'"
end subroutine foo_df

end module dataframe_stats_mod

program xdataframe_stats
use dataframe_stats_mod, only: foo
implicit none
print*,"calling foo with no title"
call foo(nacf = 5, fmt_header="()")
end program xdataframe_stats
