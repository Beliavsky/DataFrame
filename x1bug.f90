module kind_mod
implicit none
private
public :: dp
integer, parameter :: dp = kind(1.0d0)
end module kind_mod

module basic_stats_mod
use iso_fortran_env, only: output_unit
use kind_mod, only: dp
! use util_mod, only: default, print_table
implicit none
private
public :: print_acf
interface print_acf
   module procedure print_acf_mat
end interface print_acf
contains

subroutine print_acf_mat(x, nacf, label)
real(kind=dp), intent(in) :: x(:,:)
integer, intent(in) :: nacf
character (len=*), intent(in), optional :: label
end subroutine print_acf_mat

end module basic_stats_mod

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

program xdataframe_stats
use dataframe_stats_mod, only: print_acf
implicit none
print*,"calling print_acf with no title"
call print_acf(nacf = 5, fmt_header="()")
stop "here"
end program xdataframe_stats
