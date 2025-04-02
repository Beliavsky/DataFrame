program test_table
   use table_mod, only: Table, display
   implicit none

   type(Table) :: tbl

   ! Example input and output CSV filenames.
   character(len=*), parameter :: fname_in  = "spy_tlt.csv"
   character(len=*), parameter :: fname_out = "table_output.csv"

   ! 1) Read the CSV file into the Table.
   call tbl%read_csv(fname_in)

   ! 2) Display the table in its default view
   !    (prints only the first/last few rows if > nrows_print).
   print *, "=== Default Table View (from file: ", fname_in, ") ==="
   call tbl%display(title="My Table (Default)")

   ! 3) Display all rows of the table.
   print *, "=== Full Table View (from file: ", fname_in, ") ==="
   call tbl%display(print_all=.true., title="My Table (All Rows)")

   ! 4) Write the table to a new CSV file.
   call tbl%write_csv(fname_out)
   print *, "Table data written to:", fname_out
call display(tbl)
end program test_table
