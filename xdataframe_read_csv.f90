program xdataframe_read_csv
use dataframe_mod, only: DataFrame
implicit none
type(DataFrame) :: df
integer :: unit, mode

do mode = 1, 2
   open(newunit=unit, file='read_csv_test.csv', status='replace', action='write')
   write(unit,'(a)') ',SPY,TLT'
   write(unit,'(a)') '20200101,100.5,120.25'
   write(unit,'(a)') '20200102,101.75,119.5'
   if (mode == 2) then
      write(unit,'(a)') ''
      write(unit,'(a)') '20200103,999,999'
   end if
   close(unit)

   ! Exercise both natural EOF and a terminating blank record.
   call df%read_csv('read_csv_test.csv')
   if (size(df%index) /= 2) error stop 'wrong row count'
   if (any(df%index /= [20200101,20200102])) error stop 'wrong index'
   if (any(shape(df%values) /= [2,2])) error stop 'wrong shape'
   if (abs(sum(df%values) - 442.0) > 1.e-6) error stop 'wrong values'
   if (any(df%columns /= ['SPY','TLT'])) error stop 'wrong columns'

   call df%read_csv('read_csv_test.csv', max_rows=1)
   if (size(df%index) /= 1) error stop 'max_rows not respected'
   if (abs(sum(df%values) - 220.75) > 1.e-6) error stop 'wrong limited values'
end do
open(newunit=unit, file='read_csv_test.csv', status='old')
close(unit, status='delete')
print '(a)', 'xdataframe_read_csv: PASS'
end program xdataframe_read_csv
