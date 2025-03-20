program xprint_table
use kind_mod, only: dp
use util_mod, only: print_table
implicit none
integer, parameter :: n1=3, n2=4
real(kind=dp) :: x(n1, n2)
call random_number(x)
call print_table(x, row_names=["aa", "bb", "cc"], &
   col_names=["ddd", "eee", "fff", "ggg"])
call print_table(x, row_names=["aa", "bb", "cc"], &
   col_names=["ddd", "eee", "fff", "ggg"], &
   fmt_col_names="(*(a6,:,1x))", fmt_row="(a6, *(1x,f6.3))", &
   fmt_header="(/,'table')", fmt_trailer="('end', /)")
end program xprint_table