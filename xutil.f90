program xutil
use kind_mod, only: dp
use util_mod, only: default, write_merge, split_string
implicit none
integer :: i=3, iopt=5, j
real(kind=dp) :: x=3.0_dp, xopt=5.0_dp
character(len=:), allocatable :: tokens(:)
character (len=*), parameter :: string = "boy, girl, dog", delim = ","
print*,default(i)
print*,default(i, iopt)
print*,default(x)
print*,default(x, xopt)
call write_merge(.true.,"TRUE","FALSE")
call write_merge(.false.,"TRUE","FALSE")
call write_merge(.false.,"TRUE","FALSE", fmt="('<',a,'>')")
call split_string(string, delim, tokens)
print "(/, 'string: ', a)", "'" // string // "'" 
print "(/, 'delim: ', a)", "'" // delim // "'"
print*,"tokens:" 
do j=1,size(tokens)
   print "(a)", "'" // trim(tokens(j)) // "'"
end do
end program xutil
