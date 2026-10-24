program xdataframe_na_edges
use dataframe_mod, only: DataFrame
use kind_mod, only: dp
use ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_positive_inf, ieee_is_nan
implicit none
type(DataFrame) :: df, result, original
real(dp) :: nan, inf
logical, allocatable :: mask(:,:)
character(30) :: mode
nan=ieee_value(0.0_dp,ieee_quiet_nan)
inf=ieee_value(0.0_dp,ieee_positive_inf)
df%index=[10,20,30,40,50]
df%columns=['A','B','C']
allocate(df%values(5,3))
df%values(:,1)=[nan,2.0_dp,nan,4.0_dp,nan]
df%values(:,2)=nan
df%values(:,3)=[1.0_dp,inf,3.0_dp,4.0_dp,5.0_dp]
call get_command_argument(1,mode)
select case(trim(mode))
case('axis')
result=df%dropna(axis='bad')
case('how')
result=df%dropna(how='bad')
case('method')
call df%fillna(method='bad')
case('both')
call df%fillna(value=0.0_dp,method='ffill')
case('neither')
call df%fillna()
case('unallocated')
deallocate(df%values)
mask=df%isna()
end select
if (len_trim(mode)>0) error stop 'invalid option unexpectedly accepted'
original=df
mask=df%isna()
call check(count(mask)==8,'NaNs only, not infinity')
result=df%dropna()
call check(size(result%values,1)==0,'default drop any row')
result=df%dropna(axis=' COLUMNS ',how=' ALL ')
call check(all(result%index==df%index),'preserve row labels')
call check(all(result%columns==['A','C']),'preserve column order')
result=df%dropna(axis='rows',how='all')
call check(size(result%values,1)==5,'retain rows with some data')
call df%fillna(method='FFILL')
call check(ieee_is_nan(df%values(1,1)),'ffill leading NaN')
call check(all(df%values(2:5,1)==[2.0_dp,2.0_dp,4.0_dp,4.0_dp]),'ffill values')
call check(all(ieee_is_nan(df%values(:,2))),'ffill all missing column')
df=original
call df%fillna(method='bfill')
call check(ieee_is_nan(df%values(5,1)),'bfill trailing NaN')
call check(all(df%values(1:4,1)==[2.0_dp,2.0_dp,4.0_dp,4.0_dp]),'bfill values')
call check(all(ieee_is_nan(df%values(:,2))),'bfill all missing column')
call df%fillna(value=-9.0_dp)
call check(.not.any(df%isna()),'constant fill')
call check(df%values(2,3)==inf,'infinity preserved')
call check(count(original%isna())==8,'copies leave original unchanged')
! Nondefault storage lower bounds, with labels in normal positional order.
deallocate(df%values)
allocate(df%values(-2:2,0:2))
df%values=nan
df%values(0,0)=7.0_dp
call df%fillna(method='ffill')
call check(all(df%values(0:2,0)==7.0_dp),'nondefault bounds')
! Empty axes: ANY(empty)=false and ALL(empty)=true.
deallocate(df%index,df%columns,df%values)
allocate(df%index(0),df%columns(2),df%values(0,2))
df%columns=['A','B']
call df%fillna(method='ffill')
call df%fillna(method='bfill')
mask=df%isna()
call check(all(shape(mask)==[0,2]),'empty mask shape')
result=df%dropna(axis='cols',how='any')
call check(size(result%columns)==2,'empty any columns')
result=df%dropna(axis='cols',how='all')
call check(size(result%columns)==0,'empty all columns')
deallocate(df%index,df%columns,df%values)
allocate(df%index(2),df%columns(0),df%values(2,0))
df%index=[10,20]
result=df%dropna(how='any')
call check(all(result%index==[10,20]),'empty any rows')
result=df%dropna(how='all')
call check(size(result%index)==0,'empty all rows')
print *, 'xdataframe_na_edges: PASS'
contains
subroutine check(ok,message)
logical,intent(in)::ok
character(*),intent(in)::message
if (.not.ok) then
print *,message
error stop 'missing-value test failed'
end if
end subroutine
end program
