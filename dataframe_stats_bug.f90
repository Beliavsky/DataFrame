module dataframe_stats_mod
use basic_stats_mod, only: print_acf
implicit none
private
public :: print_acf
interface print_acf
  module procedure print_acf_df
end interface print_acf
contains

impure elemental subroutine print_acf_df(nacf, outu, fmt_header)
! print the autocorrelations of the columns of a dataframe
! type(DataFrame)  , intent(in) :: df
integer          , intent(in) :: nacf
integer          , intent(in), optional :: outu
character (len=*), intent(in), optional :: fmt_header
if (present(fmt_header)) print*,"(4) in print_acf_df, fmt_header = '" // trim(fmt_header) // "'"
end subroutine print_acf_df

end module dataframe_stats_mod
