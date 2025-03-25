program xutil
use kind_mod, only: dp
use util_mod, only: default, write_merge, split_string, read_words_line, &
                    exe_name, seq
implicit none
integer :: i=3, iopt=5, iu, j
real(kind=dp) :: x=3.0_dp, xopt=5.0_dp
character(len=:), allocatable :: tokens(:)
character (len=5), allocatable :: words(:)
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
open (newunit=iu, file="util.dat", action="read", status="old")
call read_words_line(iu, words)
print*,"words read: ", words
print*,"executable: " // trim(exe_name())
print "('seq(2, 10) =',*(1x,i0))", seq(2, 10)
print "('seq(2, 10, 3) =',*(1x,i0))", seq(2, 10, 3)
end program xutil
