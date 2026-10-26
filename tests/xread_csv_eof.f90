! Compile with kind, date_mod, util, df_index_date_ops_mod,
! dataframe_index_date, and table (in dependency order). Run in a scratch directory.
program xread_csv_eof
use kind_mod, only: dp
use dataframe_index_date_mod, only: DataFrame_index_date
use table_mod, only: Table
implicit none
type(DataFrame_index_date) :: dated
type(Table) :: plain
integer :: unit, mode

do mode = 1, 2
   open(newunit=unit, file="reader_eof_test.csv", status="replace")
   write(unit,"(a)") "date,value", "2025-01-01,1.5", "2025-01-02,2.5"
   if (mode == 2) write(unit,"(a)") "", "2025-01-03,99.0"
   close(unit)
   call dated%read_csv("reader_eof_test.csv")
   if (size(dated%values,1) /= 2) error stop "dated row count"
   if (any(abs(dated%values(:,1)-[1.5_dp,2.5_dp]) > 1.e-12_dp)) error stop "dated values"
   call plain%read_csv("reader_eof_test.csv")
   if (size(plain%values,1) /= 2) error stop "table row count"
   if (any(abs(plain%values(:,1)-[1.5_dp,2.5_dp]) > 1.e-12_dp)) error stop "table values"
   call dated%read_csv("reader_eof_test.csv", max_rows=1)
   if (size(dated%values,1) /= 1) error stop "dated max_rows"
end do
! Reusing a table must also replace shapes and deferred column-name lengths.
open(newunit=unit, file="reader_eof_test.csv", status="replace")
write(unit,"(a)") "date,long_column_name,b", "2025-02-01,7.5,8.5"
close(unit)
call plain%read_csv("reader_eof_test.csv")
if (any(shape(plain%values) /= [1,2])) error stop "reread shape"
if (size(plain%index) /= 1 .or. size(plain%columns) /= 2) error stop "reread metadata shape"
if (plain%index(1) /= "2025-02-01") error stop "reread index"
if (len(plain%columns) /= 16) error stop "reread column length"
if (any(plain%columns /= [character(len=16) :: "long_column_name", "b"])) error stop "reread columns"
if (any(abs(plain%values(1,:)-[7.5_dp,8.5_dp]) > 1.e-12_dp)) error stop "reread values"
open(newunit=unit, file="reader_eof_test.csv", status="replace")
write(unit,"(a)") "date,v", "a,1", "b,2", "c,3"
close(unit)
call plain%read_csv("reader_eof_test.csv")
if (any(shape(plain%values) /= [3,1])) error stop "reread growing rows"
if (len(plain%columns) /= 1) error stop "reread shorter column length"
if (any(abs(plain%values(:,1)-[1._dp,2._dp,3._dp]) > 1.e-12_dp)) error stop "reread growing values"
open(newunit=unit, file="reader_eof_test.csv", status="old")
close(unit, status="delete")
print *, "xread_csv_eof: PASS"
end program xread_csv_eof
