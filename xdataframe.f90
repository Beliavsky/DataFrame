program xdataframe
use dataframe_mod, only: DataFrame
implicit none

type(DataFrame) :: df
character(len=*), parameter :: fname_in  = "spy_tlt.csv"
character(len=*), parameter :: fname_out = "output.csv"
logical, parameter :: display_all = .false.

! Read the CSV file into the DataFrame.
call df%read_csv(fname_in)

! Display the DataFrame using default behavior (first and last nrows_print/2 rows).
print *, "Contents of the DataFrame (default view from file:", fname_in, "):"
call df%display()
call df%display(fmt_ir="(i10,*(1x,f8.2))", &
fmt_header="(a10,*(1x,a8))", title="with non-default formats")

if (display_all) then
   ! Display the full DataFrame by passing .true. to print_all.
   print *, "Full contents of the DataFrame (from file:", fname_in, "):"
   call df%display(print_all=.true.)
end if

! Write the DataFrame out to another CSV file.
call df%write_csv(fname_out)
print *, "DataFrame written to:", fname_out

end program xdataframe
