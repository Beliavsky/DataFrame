program xdataframe_stats
use dataframe_stats_mod, only: print_acf
implicit none
print*,"calling print_acf with no title"
call print_acf(nacf = 5, fmt_header="()")
stop "here"
end program xdataframe_stats
